+++
date = "2016-12-20T19:40:44+08:00"
title = "HTTP请求处理cowboy_rest"
draft = true
description = "HTTP Request parser"
tags = ["Erlang/OTP", "code", "cowboy"]
topics = ["Erlang/OTP", "code", "cowboy"]
+++

cowboy_rest以REST方式，允许用户模块介入HTTP请求处理。看过nginx源码的同学都知道，nginx回调模块可以介入http请求处理，依靠请求过程中划分的十几个阶段，实现资源重定向、权限控制等。cowboy的做法略有不同，前一篇提到过中间件的概念。给middlewares设置实现了execute回调的模块，接收到请求头和数据包后介入处理，实现nginx类似功能，官方有个markdown的例子，回头分析。一般来说，我们关注正常请求，在用户回调模块中实现cowboy_rest模块的可选回调即可。

cowboy_handler作为请求处理的最后一环，根据路由规则回调用户模块，如果返回{cowboy_rest, Req, State}，就会调用cowboy_rest:upgrade/6：

```
execute(Req, Env=#{handler := Handler, handler_opts := HandlerOpts}) ->
	try Handler:init(Req, HandlerOpts) of
		...
		{Mod, Req2, State} ->
			Mod:upgrade(Req2, Env, Handler, State, infinity, run);
		...
```

<!--more-->

这里的Mod即cowboy_rest。

cowboy_rest:upgrade利用尾递归的形式，层层调用可选的回调模块，依次处理HTTP请求参数，先看一些辅助函数。expect回调模块导出函数，并根据返回值调用OnTrue或OnFalse：

```
expect(Req, State, Callback, Expected, OnTrue, OnFalse) ->
	case call(Req, State, Callback) of
		no_call ->
			next(Req, State, OnTrue);
		{stop, Req2, HandlerState} ->
			terminate(Req2, State#state{handler_state=HandlerState});
		{Expected, Req2, HandlerState} ->
			next(Req2, State#state{handler_state=HandlerState}, OnTrue);
		{_Unexpected, Req2, HandlerState} ->
			next(Req2, State#state{handler_state=HandlerState}, OnFalse)
	end.

call(Req, State=#state{handler=Handler, handler_state=HandlerState},
		Callback) ->
	case erlang:function_exported(Handler, Callback, 2) of
		true ->
			try
				Handler:Callback(Req, HandlerState)
			catch Class:Reason ->
				error_terminate(Req, State, Class, Reason)
			end;
		false ->
			no_call
	end.
	
next(Req, State, Next) when is_function(Next) ->
	Next(Req, State);
next(Req, State, StatusCode) when is_integer(StatusCode) ->
	respond(Req, State, StatusCode).

respond(Req, State, StatusCode) ->
	terminate(cowboy_req:reply(StatusCode, Req), State).
```

整个请求行的处理都是利用expect调用回调，或者使用默认处理。首先是service_avaliable，判断服务器状态，不可用是返回503：

```
-callback service_available(Req, State) when Req::cowboy_req:req(), State::any() -> 
	{boolean(), Req, State} | {stop, Req, State}.
```

接下来调用known_methods/2，返回可用的方法头，默认支持<<"HEAD">> <<"GET">> <"POST">> <<"PUT">> <<"PATCH">> <<"DELETE">> <<"OPTIONS">>，返回501或者执行uri_too_long:

```
uri_too_long(Req, State) ->
	expect(Req, State, uri_too_long, false, fun allowed_methods/2, 414).
```

这个函数名有些模糊，可以做的事情也没有详细说清楚。总之可以在回调里做很多事情，返回false时以414终止HTTP。接着执行allowed_methods，这里会处理OPTIONS选项，设置可用方法，或者以405终止请求。接下来调用malformed_request：

```
malformed_request(Req, State) ->
	expect(Req, State, malformed_request, false, fun is_authorized/2, 400).
```

malformed_request应该是用来处理乱七八糟的请求参数，然后调用is_authorized。is_authorized和forbidden回调一般同时提供，默认情况执行下面的流程，进入options函数：

```
forbidden(Req, State) ->
	expect(Req, State, forbidden, false, fun valid_content_headers/2, 403).

valid_content_headers(Req, State) ->
	expect(Req, State, valid_content_headers, true, fun valid_entity_length/2, 501).

valid_entity_length(Req, State) ->
	expect(Req, State, valid_entity_length, true, fun options/2, 413).
```

options处理 <<"OPTIONS">> 选项，设置allow头部，并返回200。其它方法进入content_types_provided/2处理。

content_types_provided回调用户模块的同名函数，该回调返回一组 *MIME类型以及处理函数* 组成的列表，保存为cowboy_rest内部状态的content_types_p参数。然后解析Accept头，并通过prioritize_accept/1选出最优先接受的类型。choose_media_type/3确定实际选择的类型，并设置content_type_a和media_type。然后进入languages_provided/2。

languages_provided/2同样回调同名函数，解析accept-language头，更新languages_p和languages_a，设置content-language响应头。进入charsets_provided/2，类似前面的处理流程，只是针对accept-charset请求头。接着调用set_content_type设置好响应包的content-type头，

漫长的类型相关处理之后，是缓存相关。首先variances函数设置Vary头（用于缓存控制）。然后调用resource_exists/2，判断缓存是否过期等：

```
resource_exists(Req, State) ->
	expect(Req, State, resource_exists, true, fun if_match_exists/2, fun if_match_must_not_exist/2).
```

if_match_exists/2 和 if_match_must_not_exist/2 根据请求头 if-match 是否存在，选择相反的处理逻辑。if-match和etag结合，是一种不同于修改时间的缓存策略，具体实施由服务端确定。if-unmodified-since、if-modified-since、if-match和if-none-match四个头控制缓存。这里的逻辑略微有些混乱，倒叙更适合一些。先看真正的判断和处理逻辑。

if-match头存在时，要匹配etag判断缓存：

```
if_match(Req, State, EtagsList) ->
	try generate_etag(Req, State) of
		%% Strong Etag comparison: weak Etag never matches.
		{{weak, _}, Req2, State2} ->
			precondition_failed(Req2, State2);
		{Etag, Req2, State2} ->
			case lists:member(Etag, EtagsList) of
				true -> if_none_match_exists(Req2, State2);
				%% Etag may be `undefined' which cannot be a member.
				false -> precondition_failed(Req2, State2)
			end
	catch Class:Reason ->
		error_terminate(Req, State, Class, Reason)
	end.
```

precondition_failed 返回 412 预处理错误。if_none_match_exists处理if-none-match请求头：

```
if_none_match_exists(Req, State) ->
	case cowboy_req:parse_header(<<"if-none-match">>, Req) of
		undefined ->
			if_modified_since_exists(Req, State);
		'*' ->
			precondition_is_head_get(Req, State);
		EtagsList ->
			if_none_match(Req, State, EtagsList)
	end.

if_none_match(Req, State, EtagsList) ->
	try generate_etag(Req, State) of
		{Etag, Req2, State2} ->
			case Etag of
				undefined ->
					precondition_failed(Req2, State2);
				Etag ->
					case is_weak_match(Etag, EtagsList) of
						true -> precondition_is_head_get(Req2, State2);
						false -> method(Req2, State2)
					end
			end
	catch Class:Reason ->
		error_terminate(Req, State, Class, Reason)
	end.
	
precondition_is_head_get(Req, State=#state{method=Method})
		when Method =:= <<"HEAD">>; Method =:= <<"GET">> ->
	not_modified(Req, State);
precondition_is_head_get(Req, State) ->
	precondition_failed(Req, State).
```

precondition_is_head_get处理etag匹配成功的情况。if_modified_since_exists处理if-modified-since头，当修改时间小于指定的值时，直接返回not_modified，即304。如果当前时间超过该值，直接调用method正常调用。

```
if_modified_since_exists(Req, State) ->
	try cowboy_req:parse_header(<<"if-modified-since">>, Req) of
		undefined ->
			method(Req, State);
		IfModifiedSince ->
			if_modified_since_now(Req, State, IfModifiedSince)
	catch _:_ ->
		method(Req, State)
	end.

if_modified_since_now(Req, State, IfModifiedSince) ->
	case IfModifiedSince > erlang:universaltime() of
		true -> method(Req, State);
		false -> if_modified_since(Req, State, IfModifiedSince)
	end.

if_modified_since(Req, State, IfModifiedSince) ->
	try last_modified(Req, State) of
		{undefined, Req2, State2} ->
			method(Req2, State2);
		{LastModified, Req2, State2} ->
			case LastModified > IfModifiedSince of
				true -> method(Req2, State2);
				false -> not_modified(Req2, State2)
			end
	catch Class:Reason ->
		error_terminate(Req, State, Class, Reason)
	end.
```

not_modified设置etag和expires，并发回304响应：

```
not_modified(Req, State) ->
	Req2 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
	try set_resp_etag(Req2, State) of
		{Req3, State2} ->
			try set_resp_expires(Req3, State2) of
				{Req4, State3} ->
					respond(Req4, State3, 304)
			catch Class:Reason ->
				error_terminate(Req, State2, Class, Reason)
			end
	catch Class:Reason ->
		error_terminate(Req, State, Class, Reason)
	end.
```

if_unmodified_since_exists处理if-unmodified-since请求头：

```
if_unmodified_since(Req, State, IfUnmodifiedSince) ->
	try last_modified(Req, State) of
		{LastModified, Req2, State2} ->
			case LastModified > IfUnmodifiedSince of
				true -> precondition_failed(Req2, State2);
				false -> if_none_match_exists(Req2, State2)
			end
	catch Class:Reason ->
		error_terminate(Req, State, Class, Reason)
	end.
```

if-unmodified-since 头如果被修改，直接返回 412 预处理失败；没被修改就检查 if-none-match 头，继续上面介绍的处理。

这里用了奇怪的循环检测，因为要考虑到所有的请求头，而且是尾递归，所以看起来有点乱。此外还有个遗漏，即资源不存在时的处理：

```
is_put_to_missing_resource(Req, State=#state{method= <<"PUT">>}) ->
	moved_permanently(Req, State, fun is_conflict/2);
is_put_to_missing_resource(Req, State) ->
	previously_existed(Req, State).
```

moved_permanently用于处理PUT请求，通过设置location响应头，实现重定向。或者调用accept_resource接收资源：

```
accept_resource(Req, State) ->
	case call(Req, State, content_types_accepted) of
		no_call ->
			respond(Req, State, 415);
		{stop, Req2, HandlerState} ->
			terminate(Req2, State#state{handler_state=HandlerState});
		{CTA, Req2, HandlerState} ->
			CTA2 = [normalize_content_types(P) || P <- CTA],
			State2 = State#state{handler_state=HandlerState},
			try cowboy_req:parse_header(<<"content-type">>, Req2) of
				ContentType ->
					choose_content_type(Req2, State2, ContentType, CTA2)
			catch _:_ ->
				respond(Req2, State2, 415)
			end
	end.
```

process_content_type根据资源类型，调用相应的函数来处理。这里是content_types_accepted返回的MIME类型和处理函数发挥作用的地方，这个函数在下面的method方法中也是很重要的分支：

```
process_content_type(Req, State=#state{method=Method, exists=Exists}, Fun) ->
	try case call(Req, State, Fun) of
		{stop, Req2, HandlerState2} ->
			terminate(Req2, State#state{handler_state=HandlerState2});
		{true, Req2, HandlerState2} when Exists ->
			State2 = State#state{handler_state=HandlerState2},
			next(Req2, State2, fun has_resp_body/2);
		{true, Req2, HandlerState2} ->
			State2 = State#state{handler_state=HandlerState2},
			next(Req2, State2, fun maybe_created/2);
		{false, Req2, HandlerState2} ->
			State2 = State#state{handler_state=HandlerState2},
			respond(Req2, State2, 400);
		{{true, ResURL}, Req2, HandlerState2} when Method =:= <<"POST">> ->
			State2 = State#state{handler_state=HandlerState2},
			Req3 = cowboy_req:set_resp_header(
				<<"location">>, ResURL, Req2),
			if
				Exists -> respond(Req3, State2, 303);
				true -> respond(Req3, State2, 201)
			end
	end catch Class:Reason = {case_clause, no_call} ->
		error_terminate(Req, State, Class, Reason)
	end.
```

其他流程最后都会走到method，真正的请求处理。

```
method(Req, State=#state{method= <<"DELETE">>}) ->
	delete_resource(Req, State);
method(Req, State=#state{method= <<"PUT">>}) ->
	is_conflict(Req, State);
method(Req, State=#state{method=Method})
		when Method =:= <<"POST">>; Method =:= <<"PATCH">> ->
	accept_resource(Req, State);
method(Req, State=#state{method=Method})
		when Method =:= <<"GET">>; Method =:= <<"HEAD">> ->
	set_resp_body_etag(Req, State);
method(Req, State) ->
	multiple_choices(Req, State).
```

简单介绍一下set_resp_body_etag函数，GET方法时，它会处理特定的类型并返回给客户，在set_resp_body里回调content_types_provided的返回内容。

upgrade调用在response处终结：

```
respond(Req, State, StatusCode) ->
	terminate(cowboy_req:reply(StatusCode, Req), State).

terminate(Req, #state{handler=Handler, handler_state=HandlerState}) ->
	Result = cowboy_handler:terminate(normal, Req, HandlerState, Handler),
	{ok, Req, Result}.
```

