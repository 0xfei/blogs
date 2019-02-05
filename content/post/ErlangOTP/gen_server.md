+++
title = "gen_server源码阅读"
date = "2016-12-02T14:47:10+08:00"
draft = true
description = "gen_server source code"
categories = ["Erlang/OTP"]
tags = ["Erlang/OTP", "源码分析"]
topics = ["Erlang/OTP"]
+++


gen_server是Erlang/OTP的通用服务器框架，使用最为广泛。源代码很少，只有几百行。抽空详细梳理了一遍流程，以简单的gen_server应用worker为例。

先看我们自己的worker.erl文件，根据OTP设计规范，既是回调模块，又是用户接口。对外导出 *start_link/0,1* , *query/1* , *set/1* , *show/1* , *stop/1*，同时实现gen_server行为模式的6个必要的回调函数：

<!--more-->

```
%% gen_server

-module(worker).
-behaviour(gen_server).

-export([start_link/1, start_link/0, query/1, set/1, show/1, stop/0]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-record(state, {key::term(), value::term()}).

-define(SERVER, ?MODULE).
-define(PARAM, {'0x01f', 'test'}).

%% export
start_link() ->
    start_link(?PARAM).
start_link(Param) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Param], [{timeout, 10}]).
stop() ->
    gen_server:stop(?SERVER).
query(Key) ->
    gen_server:call(?SERVER, {query, Key}).
set({Key, Value}) ->
    gen_server:call(?SERVER, {set, Key, Value}).
show(Key) ->
    gen_server:cast(?SERVER, {show, Key}).

%% callbacks
init([{Key, Value}]) ->
    io:format("Saving key: ~p with value ~p, self: ~p~n", [Key, Value, self()]),
    {ok, #state{key=Key, value=Value}, 0}.

handle_call({query,Key}, From, State=#state{key=Key, value=Value}) ->
    io:format("query from ~p, self: ~p~n", [From, self()]),
    {reply, {ok, Value}, State};
handle_call({set,Key,Value}, From, State) ->
    io:format("set from ~p, self: ~p~n", [From, self()]),
    {reply, {ok, {Key, Value}}, State#state{key=Key, value=Value}}.

handle_cast({show, Key}, State=#state{key=Key, value=Value}) ->
    io:format("show, key ~p value ~p, self: ~p~n", [Key, Value, self()]),
    {noreply, State}.

handle_info(timeout, State) ->
    io:format("Timeout~n"),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("terminate reason ~p, self: ~p~n", [Reason, self()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

有必要先看一下gen_server的注释和回调函数定义，简单介绍了gen_server设计理念和调用流程。源码永远比任何解释要精确。

```
%% gen_server.erl

%%% ---------------------------------------------------
%%%
%%% The idea behind THIS server is that the user module
%%% provides (different) functions to handle different
%%% kind of inputs. 
%%% If the Parent process terminates the Module:terminate/2
%%% function is called.
%%%
%%% The user module should export:
%%%
%%%   init(Args)  
%%%     ==> {ok, State}
%%%         {ok, State, Timeout}
%%%         ignore
%%%         {stop, Reason}
%%%
%%%   handle_call(Msg, {From, Tag}, State)
%%%
%%%    ==> {reply, Reply, State}
%%%        {reply, Reply, State, Timeout}
%%%        {noreply, State}
%%%        {noreply, State, Timeout}
%%%        {stop, Reason, Reply, State}  
%%%              Reason = normal | shutdown | Term terminate(State) is called
%%%
%%%   handle_cast(Msg, State)
%%%
%%%    ==> {noreply, State}
%%%        {noreply, State, Timeout}
%%%        {stop, Reason, State} 
%%%              Reason = normal | shutdown | Term terminate(State) is called
%%%
%%%   handle_info(Info, State) Info is e.g. {'EXIT', P, R}, {nodedown, N}, ...
%%%
%%%    ==> {noreply, State}
%%%        {noreply, State, Timeout}
%%%        {stop, Reason, State} 
%%%              Reason = normal | shutdown | Term, terminate(State) is called
%%%
%%%   terminate(Reason, State) Let the user module clean up
%%%        always called when server terminates
%%%
%%%    ==> ok
%%%
%%%
%%% The work flow (of the server) can be described as follows:
%%%
%%%   User module                          Generic
%%%   -----------                          -------
%%%     start            ----->             start
%%%     init             <-----              .
%%%
%%%                                         loop
%%%     handle_call      <-----              .
%%%                      ----->             reply
%%%
%%%     handle_cast      <-----              .
%%%
%%%     handle_info      <-----              .
%%%
%%%     terminate        <-----              .
%%%
%%%                      ----->             reply
%%%
%%%
%%% ---------------------------------------------------

-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_info(Info :: timeout | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
    term().
-callback code_change(OldVsn :: (term() | {down, term()}), State :: term(),
                      Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.
-callback format_status(Opt, StatusData) -> Status when
      Opt :: 'normal' | 'terminate',
      StatusData :: [PDict | State],
      PDict :: [{Key :: term(), Value :: term()}],
      State :: term(),
      Status :: term().

-optional_callbacks([format_status/2]).
```

gen_server（或者其他模式）的核心思想就是用户提供回调函数，gen_server框架提供进程管理、消息分发等，尽可能的把功能无关的代码封装在背后。

现在开始解释我们的五个导出函数如何和gen_server框架以及Erlang交互。

#### 启动

```
gen_server:start_link({local, ?SERVER}, ?MODULE, [Param], [{timeout, 10}]).
```

调用gen_server模块的start_link/4，具体到代码：

```
%% gen_server.erl
start_link(Name, Mod, Args, Options) ->
    gen:start(?MODULE, link, Name, Mod, Args, Options).
```

又进一步调用了gen模块的start/6函数，link 表示链接程序，gen_server:start/3,4 调用时会传入nolink。注意传给gen:start的第一个参数是 ?MODULE， 即 gen_server原子。

```
%% gen.erl

%%-----------------------------------------------------------------
%% Starts a generic process.
%% start(GenMod, LinkP, Mod, Args, Options)
%% start(GenMod, LinkP, Name, Mod, Args, Options)
%%    GenMod = atom(), callback module implementing the 'real' fsm
%%    LinkP = link | nolink
%%    Name = {local, atom()} | {global, term()} | {via, atom(), term()}
%%    Args = term(), init arguments (to Mod:init/1)
%%    Options = [{timeout, Timeout} | {debug, [Flag]} | {spawn_opt, OptionList}]
%%      Flag = trace | log | {logfile, File} | statistics | debug
%%          (debug == log && statistics)
%% Returns: {ok, Pid} | ignore |{error, Reason} |
%%          {error, {already_started, Pid}} |
%%    The 'already_started' is returned only if Name is given 
%%-----------------------------------------------------------------

-spec start(module(), linkage(), emgr_name(), module(), term(), options()) ->
	start_ret().

start(GenMod, LinkP, Name, Mod, Args, Options) ->
    case where(Name) of
	undefined ->
	    do_spawn(GenMod, LinkP, Name, Mod, Args, Options);
	Pid ->
	    {error, {already_started, Pid}}
    end.
	
where({global, Name}) -> global:whereis_name(Name);
where({via, Module, Name}) -> Module:whereis_name(Name);
where({local, Name})  -> whereis(Name).
```

start函数首先判断Name是否已经被注册，这里判断的是worker原子，提供注册名的gen_server进程只能存在一个。

随后调用do_spawn/6函数，参数不变。GenMode是gen_server、LinkP是link、Name是{local, worker}、Mod是我们自己的模块名worker、Args我们传入的参数[Param]、Options是[{timeout, 10}]。

```
%% gen.erl
do_spawn(GenMod, link, Name, Mod, Args, Options) ->
    Time = timeout(Options),
    proc_lib:start_link(?MODULE, init_it,
			[GenMod, self(), self(), Name, Mod, Args, Options],
			Time,
			spawn_opts(Options));

timeout(Options) ->
    case lists:keyfind(timeout, 1, Options) of
	{_,Time} ->
	    Time;
	false ->
	    infinity
    end.
```

gen:do_spawn调用proc_lib:start_link/5，传入的?MODULE即gen，Time为10，我们自己通过worker传入的参数被放到第三个列表中。

```
%% proc_lib.erl
-spec start_link(Module, Function, Args, Time, SpawnOpts) -> Ret when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Time :: timeout(),
      SpawnOpts :: [spawn_option()],
      Ret :: term() | {error, Reason :: term()}.

start_link(M,F,A,Timeout,SpawnOpts) when is_atom(M), is_atom(F), is_list(A) ->
    Pid = ?MODULE:spawn_opt(M, F, A, ensure_link(SpawnOpts)),
    sync_wait(Pid, Timeout).

sync_wait(Pid, Timeout) ->
    receive
	{ack, Pid, Return} ->
	    Return;
	{'EXIT', Pid, Reason} ->
	    {error, Reason}
    after Timeout ->
	    unlink(Pid),
	    exit(Pid, kill),
	    flush(Pid),
	    {error, timeout}
    end.
	
ensure_link(SpawnOpts) ->
    case lists:member(link, SpawnOpts) of
	true -> 
	    SpawnOpts;
	false ->
	    [link|SpawnOpts]
    end.
```

这里首先通过调用proc_lib:spawn_opt/4创建一个进程，然后调用sync_wait(Pid, Timeout)同步调用，ensure_link确保link选项存在。sync_wait的receive消息接收我们回头介绍，先看进程的创建。

```
%% proc_lib.erl
-spec spawn_opt(Module, Function, Args, SpawnOpts) -> pid() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      SpawnOpts :: [spawn_option()].

spawn_opt(M, F, A, Opts) when is_atom(M), is_atom(F), is_list(A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    check_for_monitor(Opts),
    erlang:spawn_opt(?MODULE, init_p, [Parent,Ancestors,M,F,A], Opts).
	
get_my_name() ->
    case proc_info(self(),registered_name) of
	{registered_name,Name} -> Name;
	_                      -> self()
    end.

-spec get_ancestors() -> [pid()].

get_ancestors() ->
    case get('$ancestors') of
	A when is_list(A) -> A;
	_                 -> []
    end.
	
proc_info(Pid,Item) when node(Pid) =:= node() ->
    process_info(Pid,Item);
proc_info(Pid,Item) ->
    case lists:member(node(Pid),nodes()) of
	true ->
	    check(rpc:call(node(Pid), erlang, process_info, [Pid, Item]));
	_ ->
	    hidden
    end.

check({badrpc,nodedown}) -> undefined;
check({badrpc,Error})    -> Error;
check(Res)               -> Res.

check_for_monitor(SpawnOpts) ->
    case lists:member(monitor, SpawnOpts) of
	true ->
	    erlang:error(badarg);
	false ->
	    false
    end.
```

get_my_name返回当前进程pid或注册名，get_ancestors返回祖先进程列表，check_for_monitor确保monitor选项不存在。然后调用系统函数erlang:spawn_opt生成新进程。新的进程执行proc_lib:init_p(Parent, Ancestors, M, F, A)。

```
%% proc_lib.erl

-spec init_p(pid(), [pid()], atom(), atom(), [term()]) -> term().

init_p(Parent, Ancestors, M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    put('$ancestors', [Parent|Ancestors]),
    put('$initial_call', trans_init(M, F, A)),
    init_p_do_apply(M, F, A).

init_p_do_apply(M, F, A) ->
    try
	apply(M, F, A) 
    catch
	Class:Reason ->
	    exit_p(Class, Reason)
    end.
	
%% -----------------------------------------------------
%% Translate the initial call to some useful information.
%% -----------------------------------------------------

trans_init(gen,init_it,[gen_server,_,_,supervisor,{_,Module,_},_]) ->
    {supervisor,Module,1};
trans_init(gen,init_it,[gen_server,_,_,_,supervisor,{_,Module,_},_]) ->
    {supervisor,Module,1};
trans_init(gen,init_it,[gen_server,_,_,supervisor_bridge,[Module|_],_]) ->
    {supervisor_bridge,Module,1};
trans_init(gen,init_it,[gen_server,_,_,_,supervisor_bridge,[Module|_],_]) ->
    {supervisor_bridge,Module,1};
trans_init(gen,init_it,[gen_event|_]) ->
    {gen_event,init_it,6};
trans_init(gen,init_it,[_GenMod,_,_,Module,_,_]) when is_atom(Module) ->
    {Module,init,1};
trans_init(gen,init_it,[_GenMod,_,_,_,Module|_]) when is_atom(Module) ->
    {Module,init,1};
trans_init(M, F, A) when is_atom(M), is_atom(F) ->
    {M,F,length(A)}.
```

init_p首先更新新进程的祖先进程'$ancestors'环境变量和'$initial_call'，trans_init只是做一个简单的转换，这里的M 是 gen，F 是 init_it，A 是上面详细介绍过的参数[GenMod, self(), self(), Name, Mod, Args, Options]，因此返回{Module, init, 1}。

init_p_do_apply直接调用M:F(A)，即gen:init_it(GenMod, self(), self(), Name, Mod, Args, Options)，注意这里的self()并不是新进程的pid，而是父进程pid，也就是调用sync_wait进程的pid，sync_wait的第一个参数Pid才是执行gen:init_it的新进程的pid。目前为止，其实只涉及到两个进程——运行gen_server框架的进程、新生成的进程。

回到gen模块，在新进程上下文执行init_it/7。

```
%%-----------------------------------------------------------------
%% Initiate the new process.
%% Register the name using the Rfunc function
%% Calls the Mod:init/Args function.
%% Finally an acknowledge is sent to Parent and the main
%% loop is entered.
%%-----------------------------------------------------------------
init_it(GenMod, Starter, Parent, Mod, Args, Options) ->
    init_it2(GenMod, Starter, Parent, self(), Mod, Args, Options).

init_it(GenMod, Starter, Parent, Name, Mod, Args, Options) ->
    case register_name(Name) of
	true ->
	    init_it2(GenMod, Starter, Parent, Name, Mod, Args, Options);
	{false, Pid} ->
	    proc_lib:init_ack(Starter, {error, {already_started, Pid}})
    end.

init_it2(GenMod, Starter, Parent, Name, Mod, Args, Options) ->
    GenMod:init_it(Starter, Parent, Name, Mod, Args, Options).
```

这里做了简单解释——初始化新进程、注册为{local, worker}、给父进程发送确认、进入loop循环。

init_it2调用GenMod:init_it(Starter, Parent, Name, Mod, Args, Options)，Starter是父进程pid，而Parent在链接时跟Starter一样，不链接时是原子self。GenMod是gen_server，回到gen_server模块。

```
%%% ---------------------------------------------------
%%% Initiate the new process.
%%% Register the name using the Rfunc function
%%% Calls the Mod:init/Args function.
%%% Finally an acknowledge is sent to Parent and the main
%%% loop is entered.
%%% ---------------------------------------------------
init_it(Starter, self, Name, Mod, Args, Options) ->
    init_it(Starter, self(), Name, Mod, Args, Options);
init_it(Starter, Parent, Name0, Mod, Args, Options) ->
    Name = gen:name(Name0),
    Debug = gen:debug_options(Name, Options),
    case catch Mod:init(Args) of
	{ok, State} ->
	    proc_lib:init_ack(Starter, {ok, self()}), 	    
	    loop(Parent, Name, State, Mod, infinity, Debug);
	{ok, State, Timeout} ->
	    proc_lib:init_ack(Starter, {ok, self()}), 	    
	    loop(Parent, Name, State, Mod, Timeout, Debug);
	{stop, Reason} ->
	    %% For consistency, we must make sure that the
	    %% registered name (if any) is unregistered before
	    %% the parent process is notified about the failure.
	    %% (Otherwise, the parent process could get
	    %% an 'already_started' error if it immediately
	    %% tried starting the process again.)
	    gen:unregister_name(Name0),
	    proc_lib:init_ack(Starter, {error, Reason}),
	    exit(Reason);
	ignore ->
	    gen:unregister_name(Name0),
	    proc_lib:init_ack(Starter, ignore),
	    exit(normal);
	{'EXIT', Reason} ->
	    gen:unregister_name(Name0),
	    proc_lib:init_ack(Starter, {error, Reason}),
	    exit(Reason);
	Else ->
	    Error = {bad_return_value, Else},
	    proc_lib:init_ack(Starter, {error, Error}),
	    exit(Error)
    end.
```

调用gen:name返回注册名，在这里是worker；gen:debug_options和调试相关，先不讨论。有意思的是这里用到了老的catch风格代码，Mod:init(Args)即worker:init([Param])，终于回调到了我们自己写的代码。根据返回值，调用proc_lib:init_ack给Starter进程发送消息。

```
%% proc_lib.erl

-spec init_ack(Parent, Ret) -> 'ok' when
      Parent :: pid(),
      Ret :: term().

init_ack(Parent, Return) ->
    Parent ! {ack, self(), Return},
    ok.
	
sync_wait(Pid, Timeout) ->
    receive
	{ack, Pid, Return} ->
	    Return;
	{'EXIT', Pid, Reason} ->
	    {error, Reason}
    after Timeout ->
	    unlink(Pid),
	    exit(Pid, kill),
	    flush(Pid),
	    {error, timeout}
    end.
```

早先提到的proc_lib:start_link调用sync_wait等待确认，这时会根据收到的消息执行不同的操作，直接返回或者终止进程。我们传入的超时值，在这里发挥作用。

至此gen_server进程启动。简单总结：worker:start_link -> gen_server:start_link -> gen:start -> gen:do_spawn -> proc_lib:start_link -> proc_lib:spawn_opt -> erlang:spawn_opt => new process -> proc_lib:init_p -> gen:init_it -> gen_server:init_it -> worker:init -> send ack to parent -> gen_server:loop。


#### loop循环

总体来说，gen_server的启动还是很简单的，只是一连串的调用流程，最后进入gen_server 的loop循环。

```
loop(Parent, Name, State, Mod, Time, Debug) ->
    Msg = receive
	      Input ->
		    Input
	  after Time ->
		  timeout
	  end,
    decode_msg(Msg, Parent, Name, State, Mod, Time, Debug, false).
	
decode_msg(Msg, Parent, Name, State, Mod, Time, Debug, Hib) ->
    case Msg of
	{system, From, Req} ->
	    sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug,
				  [Name, State, Mod, Time], Hib);
	{'EXIT', Parent, Reason} ->
	    terminate(Reason, Name, Msg, Mod, State, Debug);
	_Msg when Debug =:= [] ->
	    handle_msg(Msg, Parent, Name, State, Mod);
	_Msg ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3,
				      Name, {in, Msg}),
	    handle_msg(Msg, Parent, Name, State, Mod, Debug1)
    end.
```

新进程会阻塞在loop的receive，等到消息到来。我们假设已经收到消息，看下面的处理过程。对于{system, From, Req}系统消息，调用sys:handle_system_msg处理，用于在暂停、恢复或者终止进程等；{'EXIT', Parent, Reason}退出信号导致进程退出，并调用terminate函数；其他消息都会进入handle_msg函数。


#### 进程退出

先来看进程退出，即gen_server:terminate函数。

```
-spec terminate(_, _, _, _, _, _) -> no_return().
terminate(Reason, Name, Msg, Mod, State, Debug) ->
    terminate(Reason, Reason, Name, Msg, Mod, State, Debug).

-spec terminate(_, _, _, _, _, _, _) -> no_return().
terminate(ExitReason, ReportReason, Name, Msg, Mod, State, Debug) ->
    Reply = try_terminate(Mod, ExitReason, State),
    case Reply of
	{'EXIT', ExitReason1, ReportReason1} ->
	    FmtState = format_status(terminate, Mod, get(), State),
	    error_info(ReportReason1, Name, Msg, FmtState, Debug),
	    exit(ExitReason1);
	_ ->
	    case ExitReason of
		normal ->
		    exit(normal);
		shutdown ->
		    exit(shutdown);
		{shutdown,_}=Shutdown ->
		    exit(Shutdown);
		_ ->
		    FmtState = format_status(terminate, Mod, get(), State),
		    error_info(ReportReason, Name, Msg, FmtState, Debug),
		    exit(ExitReason)
	    end
    end.
	
try_terminate(Mod, Reason, State) ->
    try
	{ok, Mod:terminate(Reason, State)}
    catch
	throw:R ->
	    {ok, R};
	error:R ->
	    Stacktrace = erlang:get_stacktrace(),
	    {'EXIT', {R, Stacktrace}, {R, Stacktrace}};
	exit:R ->
	    Stacktrace = erlang:get_stacktrace(),
	    {'EXIT', R, {R, Stacktrace}}
    end.
```

terminate函数会回调我们的worker:terminate，并调用format_status格式化输出信息。这里也可以看到可选回调format_status的作用。

```
format_status(Opt, Mod, PDict, State) ->
    DefStatus = case Opt of
		    terminate -> State;
		    _ -> [{data, [{"State", State}]}]
		end,
    case erlang:function_exported(Mod, format_status, 2) of
	true ->
	    case catch Mod:format_status(Opt, [PDict, State]) of
		{'EXIT', _} -> DefStatus;
		Else -> Else
	    end;
	_ ->
	    DefStatus
    end.
```

为了完整的讨论进程终止，我们顺便介绍gen_server:stop函数：

```
stop(Name) ->
    gen:stop(Name).

stop(Name, Reason, Timeout) ->
    gen:stop(Name, Reason, Timeout).
```

简单调用gen:stop函数：

```
stop(Process) ->
    stop(Process, normal, infinity).

stop(Process, Reason, Timeout)
  when Timeout =:= infinity; is_integer(Timeout), Timeout >= 0 ->
    Fun = fun(Pid) -> proc_lib:stop(Pid, Reason, Timeout) end,
    do_for_proc(Process, Fun).
	
%% Local or remote by pid
do_for_proc(Pid, Fun) when is_pid(Pid) ->
    Fun(Pid);
%% Local by name
do_for_proc(Name, Fun) when is_atom(Name) ->
    case whereis(Name) of
	Pid when is_pid(Pid) ->
	    Fun(Pid);
	undefined ->
	    exit(noproc)
    end;
```

do_for_proc会以pid为参数调用第二个参数fun函数。fun即 proc_lib:stop(Pid, Reason, Timeout)。

```
%% proc_lib.erl
-spec stop(Process, Reason, Timeout) -> 'ok' when
      Process :: pid() | RegName | {RegName,node()},
      RegName :: atom(),
      Reason :: term(),
      Timeout :: timeout().
stop(Process, Reason, Timeout) ->
    {Pid, Mref} = erlang:spawn_monitor(do_stop(Process, Reason)),
    receive
	{'DOWN', Mref, _, _, Reason} ->
	    ok;
	{'DOWN', Mref, _, _, {noproc,{sys,terminate,_}}} ->
	    exit(noproc);
	{'DOWN', Mref, _, _, CrashReason} ->
	    exit(CrashReason)
    after Timeout ->
	    exit(Pid, kill),
	    receive
		{'DOWN', Mref, _, _, _} ->
		    exit(timeout)
	    end
    end.

-spec do_stop(Process, Reason) -> Fun when
      Process :: pid() | RegName | {RegName,node()},
      RegName :: atom(),
      Reason :: term(),
      Fun :: fun(() -> no_return()).
do_stop(Process, Reason) ->
    fun() ->
	    Mref = erlang:monitor(process, Process),
	    ok = sys:terminate(Process, Reason, infinity),
	    receive
		{'DOWN', Mref, _, _, ExitReason} ->
		    exit(ExitReason)
	    end
    end.
```

这里会多次用到进程监控。proc_lib:stop生成并监控新进程 proc_lib:do_stop。do_stop也会监控要终止的进程，即worker，然后调用sys:terminate，它并不会直接终止进程，而是发消息：

```
terminate(Name, Reason, Timeout) ->
    send_system_msg(Name, {terminate, Reason}, Timeout).
	
send_system_msg(Name, Request, Timeout) ->
    case catch gen:call(Name, system, Request, Timeout) of
	{ok,Res} -> Res;
	{'EXIT', Reason} -> exit({Reason, mfa(Name, Request, Timeout)})
    end.
```

利用gen:call函数给Name - {local, worker}进程发消息，由之前介绍过的loop消息循环接收并处理，会收到一个系统消息，然后回调gen_server:system_terminate，然后回到gen_server:terminate。这里先略过详细流程，只要知道最后还是会由gen_server:terminate来处理。为什么要搞这么多监控呢？把复杂的任务放到新进程中，然后用一个简单的进程（简单到几乎不会崩溃）监控它，是Erlang最根本的哲学。


#### gen_server:call

worker.erl程序中，导出函数query和set调用了gen_server:call：

```
%% gen_server.erl
call(Name, Request) ->
    case catch gen:call(Name, '$gen_call', Request) of
	{ok,Res} ->
	    Res;
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, call, [Name, Request]}})
    end.

call(Name, Request, Timeout) ->
    case catch gen:call(Name, '$gen_call', Request, Timeout) of
	{ok,Res} ->
	    Res;
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, call, [Name, Request, Timeout]}})
    end.
```

会以参数[{local, worker}, '$gen_call', Request]调用到gen:call——之前有过介绍，这里详细看一下代码：

```
call(Process, Label, Request) -> 
    call(Process, Label, Request, ?default_timeout).

call(Process, Label, Request, Timeout)
  when Timeout =:= infinity; is_integer(Timeout), Timeout >= 0 ->
    Fun = fun(Pid) -> do_call(Pid, Label, Request, Timeout) end,
    do_for_proc(Process, Fun).

do_call(Process, Label, Request, Timeout) ->
    try erlang:monitor(process, Process) of
	Mref ->
	    catch erlang:send(Process, {Label, {self(), Mref}, Request},
		  [noconnect]),
	    receive
		{Mref, Reply} ->
		    erlang:demonitor(Mref, [flush]),
		    {ok, Reply};
		{'DOWN', Mref, _, _, noconnection} ->
		    Node = get_node(Process),
		    exit({nodedown, Node});
		{'DOWN', Mref, _, _, Reason} ->
		    exit(Reason)
	    after Timeout ->
		    erlang:demonitor(Mref, [flush]),
		    exit(timeout)
	    end
    catch
	error:_ ->
	    Node = get_node(Process),
	    monitor_node(Node, true),
	    receive
		{nodedown, Node} -> 
		    monitor_node(Node, false),
		    exit({nodedown, Node})
	    after 0 -> 
		    Tag = make_ref(),
		    Process ! {Label, {self(), Tag}, Request},
		    wait_resp(Node, Tag, Timeout)
	    end
    end.
```

do_call执行真正的调用，也就是消息发送。首先monitor进程，然后调用erlang:send发送消息。如果进程在其他节点，需要监控节点的进程。erlang:send发送的消息由loop循环接收，gen_server:handle_msg最后详细介绍。


#### gen_server:cast

worker:show会调用gen_server:cast，发送异步消息：

```
cast({global,Name}, Request) ->
    catch global:send(Name, cast_msg(Request)),
    ok;
cast({via, Mod, Name}, Request) ->
    catch Mod:send(Name, cast_msg(Request)),
    ok;
cast({Name,Node}=Dest, Request) when is_atom(Name), is_atom(Node) -> 
    do_cast(Dest, Request);
cast(Dest, Request) when is_atom(Dest) ->
    do_cast(Dest, Request);
cast(Dest, Request) when is_pid(Dest) ->
    do_cast(Dest, Request).

do_cast(Dest, Request) -> 
    do_send(Dest, cast_msg(Request)),
    ok.
    
cast_msg(Request) -> {'$gen_cast',Request}.

do_send(Dest, Msg) ->
    case catch erlang:send(Dest, Msg, [noconnect]) of
	noconnect ->
	    spawn(erlang, send, [Dest,Msg]);
	Other ->
	    Other
    end.
```

异步消息只需要发送，不关心进程的响应。所以很简单，erlang:send发送'$gen_cast'标记的消息。

#### gen_server:handle_msg

消息收发的关键函数时handle_msg，所以现在介绍。

```
handle_msg({'$gen_call', From, Msg}, Parent, Name, State, Mod) ->
    Result = try_handle_call(Mod, Msg, From, State),
    case Result of
	{ok, {reply, Reply, NState}} ->
	    reply(From, Reply),
	    loop(Parent, Name, NState, Mod, infinity, []);
	{ok, {reply, Reply, NState, Time1}} ->
	    reply(From, Reply),
	    loop(Parent, Name, NState, Mod, Time1, []);
	{ok, {noreply, NState}} ->
	    loop(Parent, Name, NState, Mod, infinity, []);
	{ok, {noreply, NState, Time1}} ->
	    loop(Parent, Name, NState, Mod, Time1, []);
	{ok, {stop, Reason, Reply, NState}} ->
	    {'EXIT', R} = 
		(catch terminate(Reason, Name, Msg, Mod, NState, [])),
	    reply(From, Reply),
	    exit(R);
	Other -> handle_common_reply(Other, Parent, Name, Msg, Mod, State)
    end;
handle_msg(Msg, Parent, Name, State, Mod) ->
    Reply = try_dispatch(Msg, Mod, State),
    handle_common_reply(Reply, Parent, Name, Msg, Mod, State).
	
try_handle_call(Mod, Msg, From, State) ->
    try
	{ok, Mod:handle_call(Msg, From, State)}
    catch
	throw:R ->
	    {ok, R};
	error:R ->
	    Stacktrace = erlang:get_stacktrace(),
	    {'EXIT', {R, Stacktrace}, {R, Stacktrace}};
	exit:R ->
	    Stacktrace = erlang:get_stacktrace(),
	    {'EXIT', R, {R, Stacktrace}}
    end.
	
reply({To, Tag}, Reply) ->
    catch To ! {Tag, Reply}.
```

首先处理'$gen_call'消息，try_handle_call是try保护的回调调用，调用worker:handle_call，我们自己编写的回调函数，看到其实可以在handle_call中直接throw返回结果。调用reply发回响应，From的形式是{To, Tag}，To表示pid或者注册名，Tag是monitor返回的引用值，这个消息会被之前介绍的gen:do_call接收，并返回{ok, Reply}到我们的应用程序。

返回其他值走向handle_common_reply。

其他消息走到第二个分支，调用try_dispatch。

```
try_dispatch({'$gen_cast', Msg}, Mod, State) ->
    try_dispatch(Mod, handle_cast, Msg, State);
try_dispatch(Info, Mod, State) ->
    try_dispatch(Mod, handle_info, Info, State).

try_dispatch(Mod, Func, Msg, State) ->
    try
	{ok, Mod:Func(Msg, State)}
    catch
	throw:R ->
	    {ok, R};
	error:R ->
	    Stacktrace = erlang:get_stacktrace(),
	    {'EXIT', {R, Stacktrace}, {R, Stacktrace}};
	exit:R ->
	    Stacktrace = erlang:get_stacktrace(),
	    {'EXIT', R, {R, Stacktrace}}
    end.
```

handle_cast和handle_info的使用在这里体现，它们返回的结果都会交由handle_common_reply处理：

```
handle_common_reply(Reply, Parent, Name, Msg, Mod, State) ->
    case Reply of
	{ok, {noreply, NState}} ->
	    loop(Parent, Name, NState, Mod, infinity, []);
	{ok, {noreply, NState, Time1}} ->
	    loop(Parent, Name, NState, Mod, Time1, []);
	{ok, {stop, Reason, NState}} ->
	    terminate(Reason, Name, Msg, Mod, NState, []);
	{'EXIT', ExitReason, ReportReason} ->
	    terminate(ExitReason, ReportReason, Name, Msg, Mod, State, []);
	{ok, BadReply} ->
	    terminate({bad_return_value, BadReply}, Name, Msg, Mod, State, [])
    end.
```

这里没什么特殊的，除了注意超时值，如果某次返回没有超时，就会更新为infinity；这也很好理解，尾递归情况，之前返回的参数不会影响后续调用。还有，handle_cast和handle_info的返回值只能是{ok, {noreply, NState...}}。随后接着进入loop循环。

#### 最后

实际运行worker看一下效果：

```
Erlang/OTP 19 [erts-8.0] [64-bit] [smp:8:8] [async-threads:10]

Eshell V8.0  (abort with ^G)
1> worker:start_link().
Saving key: '0x01f' with value test, self: <0.58.0>
Timeout
{ok,<0.58.0>}
2> worker:show('0x01f').
show, key '0x01f' value test, self: <0.58.0>
ok
3> worker:query('0x01f').
query from {<0.56.0>,#Ref<0.0.1.59>}, self: <0.58.0>
{ok,test}
4> self().
<0.56.0>
5> 
```

唯一没有介绍的回调函数code_change由gen_server:system_code_change回调：

```
system_code_change([Name, State, Mod, Time], _Module, OldVsn, Extra) ->
    case catch Mod:code_change(OldVsn, State, Extra) of
	{ok, NewState} -> {ok, [Name, NewState, Mod, Time]};
	Else -> Else
    end.
```

总结一下。虽然篇幅很长，但几乎都是列源代码，只有少部分简单解释。因为Erlang实在是太简单了——这里简单的意思是简洁。很容易就能理解它的所有能力，而这种能力又非常稳定和强大。
