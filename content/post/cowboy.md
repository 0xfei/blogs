+++
date = "2016-12-15T00:10:44+08:00"
title = "Erlang HTTP服务器cowboy"
draft = true
description = "tcp acceptor pool ranch"
tags = ["Erlang/OTP", "code", "cowboy"]
topics = ["Erlang/OTP", "code", "cowboy"]
+++

cowboy是基于[ranch](http://0x01f.com/post/ranch/)的http服务器框架，提供用户自定义路由、REST接口等便利功能。由于ranch是很完善的TCP池，所以在此之上的cowboy代码很容易支持http/https。虽说如此，http毕竟是很复杂的协议，好多细节不参考RFC根本搞不清楚。借此机会我要详细了解如何实现完整的HTTP服务器。首先了解cowboy完整的流程以及使用方法。

最简单的例子：

```
start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", toppage_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, 100, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}),
```

cowboy_router.erl模块用于路由匹配，用户传入路由列表，并将其作为cowboy:start_clear的第四个参数的env值。类型定义和解析非常具有Erlang特色，新手看绝对会头晕，甚至完全看不懂。我也是配合shell才完全理解。'_'代表host，随后的列表是path元组，匹配host之下的路径，支持[]可选模式和_通配符等，匹配成功后回调指定的模块，这里是toppage_handler。

cowboy:start_clear开始http服务器：

```
-spec start_clear(ranch:ref(), non_neg_integer(), ranch_tcp:opts(),
	cowboy_protocol:opts()) -> {ok, pid()} | {error, any()}.
start_clear(Ref, NbAcceptors, TransOpts0, ProtoOpts)
		when is_integer(NbAcceptors), NbAcceptors > 0 ->
	TransOpts = [connection_type(ProtoOpts)|TransOpts0],
	ranch:start_listener(Ref, NbAcceptors, ranch_tcp, TransOpts, cowboy_clear, ProtoOpts).
```

ranch accept成功后，回调cowboy_clear模块的start_link：

```
-spec start_link(ranch:ref(), inet:socket(), module(), cowboy:opts()) -> {ok, pid()}.
start_link(Ref, Socket, Transport, Opts) ->
	Pid = proc_lib:spawn_link(?MODULE, proc_lib_hack, [self(), Ref, Socket, Transport, Opts]),
	{ok, Pid}.
```

ranch引用值、客户端socket、参数等为参数创建proc_lib_hack进程，它会在try中执行init：

```
-spec init(pid(), ranch:ref(), inet:socket(), module(), cowboy:opts()) -> ok.
init(Parent, Ref, Socket, Transport, Opts) ->
	ok = ranch:accept_ack(Ref),
	init(Parent, Ref, Socket, Transport, Opts, cowboy_http).

init(Parent, Ref, Socket, Transport, Opts, Protocol) ->
	{Handler, Type} = maps:get(stream_handler, Opts, {cowboy_stream_h, supervisor}),
	_ = case Type of
		worker -> ok;
		supervisor -> process_flag(trap_exit, true)
	end,
	Protocol:init(Parent, Ref, Socket, Transport, Opts, Handler).
```

ranch:accpet_ack同步ranch服务中心的shoot消息，表示当前socket准备完毕，可以收发数据。init中，注意Handler默认是cowboy_stream_h，之后的代码中会出现无数的handler、state、pid等信息，稍不小心就搞混。Protocol:init即cowboy_http:init。进入真正的http模块。

到了http模块框架其实就走完了，接下来的无非就是接收数据、解析http请求行、http头和请求包体，然后根据路由规则处理。说来简单，实现的时候却涉及到相当多的技巧和细节。

cowboy使用半自动模式，即每次决定接收数据时将socket设置为{active,once}；消息循环loop开始前，先设置最大长连接数默认为100，并启动一个默认值为5s的定时器。http实现时默认是系统进程，需要处理进程退出、OTP回调等消息。主要关心数据接收、连接关闭和http数据流，这也是cowboy的核心功能。

数据接收和解析由cowboy_http:parse/2完成，内部状态state有一个值表示当前解析阶段——请求行、请求头、数据包体等。解析完后，生成一个map数据结构，维护当前请求状态。类似nginx的请求上下文，很重要的结构：

```
Req = #{
	ref => Ref,
	pid => self(),
	streamid => StreamID,
	peer => Peer,
	method => Method,
	scheme => Scheme,
	host => Host,
	port => Port,
	path => Path,
	qs => Qs,
	version => Version,
	headers => maps:remove(<<"transfer-encoding">>, Headers),
	has_body => HasBody,
	body_length => BodyLength
}
```

随后把状态更新为body接收，并取消定时器（这里的处理是否会有DOS风险？）。至此parse完成，并调用after_parse，后者回调Handler:init/3，默认是cowboy_stream_h:init：

```
-spec init(_,_,_) -> _.
init(_StreamID, Req=#{ref := Ref}, Opts) ->
	Env = maps:get(env, Opts, #{}),
	Middlewares = maps:get(middlewares, Opts, [cowboy_router, cowboy_handler]),
	Shutdown = maps:get(shutdown, Opts, 5000),
	Pid = proc_lib:spawn_link(?MODULE, proc_lib_hack, [Req, Env, Middlewares]),
	{[{spawn, Pid, Shutdown}], #state{ref=Ref, pid=Pid}}.
```

它接收stream id、请求结构体和http全局结构体的opts选项为参数。middlewares中间件默认有cowboy_router路由模块和cowboy_handler回调处理模块，我们可以自由添加，只需在cowboy:start_clear时传入合适的middlewares值。这点看来，比nginx的介入处理功能还要强大。

这里又会创建新进程proc_lib_hack，它会安顺西调用middlewares列表的模块的execute/2导出函数，cowboy_router和cowboy_handler的execute如下：

```
%% cowboy_router.erl
execute(Req=#{host := Host, path := Path}, Env=#{dispatch := Dispatch}) ->
	case match(Dispatch, Host, Path) of
		{ok, Handler, HandlerOpts, Bindings, HostInfo, PathInfo} ->
			{ok, Req#{
				host_info => HostInfo,
				path_info => PathInfo,
				bindings => Bindings
			}, Env#{
				handler => Handler,
				handler_opts => HandlerOpts
			}};
		...
	end.
```

match函数匹配Host和Path，并返回Handler和HandlerOpts，这里分别是toppage_handler和[]。再看最后的中间件cowboy_handler：

```
%% cowboy_handler.erl

execute(Req, Env=#{handler := Handler, handler_opts := HandlerOpts}) ->
	try Handler:init(Req, HandlerOpts) of
		{ok, Req2, State} ->
			Result = terminate(normal, Req2, State, Handler),
			{ok, Req2, [{result, Result}|Env]};
		{Mod, Req2, State} ->
			Mod:upgrade(Req2, Env, Handler, State, infinity, run);
		...
```

这里用户的回调模块便会发挥作用：

```
%% toppage_handler.erl
init(Req0, Opts) ->
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain">>
	}, <<"Hello world!">>, Req0),
	{ok, Req, Opts}.
```

hello_world例子中，直接调用cowboy_req:reply发送响应，然后返回ok结束http。

先回到after_parse，cowboy_stream_h:init的返回值为 *{Commands = [{spawn, Pid, Shutdown}], StreamState = #state{ref=Ref, pid=Pid}}.* 。http模块更新state状态的streams键，以及短连接时更新last_streamid。然后调用commands：

```
commands(State, _, []) ->
	State;
	
commands(State=#state{children=Children}, StreamID, [{spawn, Pid, Shutdown}|Tail]) ->
	commands(State#state{children=[{Pid, StreamID, Shutdown}|Children]}, StreamID, Tail);
```

将同一连接的子进程加入state的children状态。GET请求一般到这里就结束了，因为没有数据包。http连接终止时，会把子进程信息从children中取出。具体的终止在下面的响应包发送的时候提及。

做个小总结：每次http访问维护一个全局状态，而每个http长连接维护一组用stream id标识的streams，即基于cowboy_stream_h模块的进程，回调用户模块处理请求。

有一个很重要的功能还没介绍——发送响应。通过导出接口cowboy_req:reply实现：

```
reply(Status, Headers, Body, Req) when is_integer(Status); is_binary(Status) ->
	do_reply(Status, Headers#{<<"content-length">> => integer_to_binary(iolist_size(Body))}, Body, Req).
	
do_reply(Status, Headers, Body, Req=#{pid := Pid, streamid := StreamID}) ->
	Pid ! {{Pid, StreamID}, {response, Status, response_headers(Headers, Req), Body}},
	done_replying(Req, true).
```

向请求上下文记录的pid发送{Pid,StreamID}开头的消息，pid指向http进程：

```
%% cowboy_http.erl loop
loop(State=#state{parent=Parent, socket=Socket, transport=Transport,
		handler=_Handler, timer=TimerRef, children=Children}, Buffer) ->
	{OK, Closed, Error} = Transport:messages(),
	receive
		%% Messages pertaining to a stream.
		{{Pid, StreamID}, Msg} when Pid =:= self() ->
			loop(info(State, StreamID, Msg), Buffer);
	...
```

http调用info函数，info首先找出stream对应的状态，并回调stream处理模块cowboy_stream_h的info函数，info返回{Command, NewStreamState}，http调用commands函数处理命令并更新流状态：

```
%% cowboy_stream_h.erl 
%% Response.
info(_StreamID, Response = {response, _, _, _}, State) ->
	{[Response], State};
	
%% normal exit
info(_StreamID, {'EXIT', Pid, normal}, State=#state{pid=Pid}) ->
	{[stop], State};
```

这里顺便把连接关闭时的info处理过程放上，处理都是类似的。commands函数的{response,...}分支处理响应包。

```
commands(State0=#state{socket=Socket, transport=Transport, out_state=wait, streams=Streams}, StreamID,
		[{response, StatusCode, Headers0, Body}|Tail]) ->
	#stream{version=Version} = lists:keyfind(StreamID, #stream.id, Streams),
	{State, Headers} = connection(State0, Headers0, StreamID, Version),
	Response = cow_http:response(StatusCode, 'HTTP/1.1', headers_to_list(Headers)),
	case Body of
		{sendfile, O, B, P} ->
			Transport:send(Socket, Response),
			commands(State#state{out_state=done}, StreamID, [{sendfile, fin, O, B, P}|Tail]);
		_ ->
			Transport:send(Socket, [Response, Body]),
			maybe_terminate(State#state{out_state=done}, StreamID, Tail, fin)
	end;
```

Transport是ranch_tcp，发送响应。

cowboy是一款功能强大的http服务器，类似nginx，轻量、高度可定制，如果用来实现负载均衡器，应该也是不错的。更具体的细节我还没看，包括REST支持、介入请求处理流程等。熟悉之后再写更多的分析文章。