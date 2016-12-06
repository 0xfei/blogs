+++
title = "supervisor源码阅读"
date = "2016-12-06T15:40:54+08:00"
draft = true
description = "supervisor source code"
tags = ["Erlang/OTP", "code", "supervisor"]
topics = ["Erlang/OTP", "code", "supervisor"]
+++


supervisor是监控树中最重要的组成部分，负责启动、停止和监控子进程，异常时重启等。supervisor模块应该尽可能简单，保持功能单一——负责监控。supervisor模块本身基于gen_server，所以它实现了gen_server的六个主要回调函数，是一个gen_server进程。通过阅读supervisor模块源码，了解实现supervisor回调的过程以及实现进程监控器的步骤。

先写一个简单的worker_sup.erl，实现了supervisor行为模式，配合worker.erl模块使用：

```
%% supervisor
-module(worker_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(worker_sup, []).

init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [#{id => worker, start => {worker, start_link, []}, restart => permanent, shutdown => brutal_kill, type => worker, modules => [worker]}],
    {ok, {SupFlags, ChildSpecs}}.
```

<!--more-->

效果如下，注意进程实现了一次自动重启：

```
ကEshell V8.0  (abort with ^G)
1> c(worker_sup).
{ok,worker_sup}
2> worker_sup:start_link().
Saving key: '0x01f' with value test, self: <0.64.0>
Timeout
{ok,<0.63.0>}
3> worker:
code_change/3  handle_call/3  handle_cast/2  handle_info/2  init/1         
module_info/0  module_info/1  query/1        set/1          show/1         
start_link/0   start_link/1   stop/0         terminate/2    
3> worker:show('0x01f').
show, key '0x01f' value test, self: <0.64.0>
ok
4> worker:stop().
terminate reason normal, self: <0.64.0>
Saving key: '0x01f' with value test, self: <0.68.0>
Timeout
ok
5> worker:stop().
terminate reason normal, self: <0.68.0>
Saving key: '0x01f' with value test, self: <0.71.0>
Timeout
ok
6> worker:stop().
terminate reason normal, self: <0.71.0>
** exception exit: shutdown
```

所有行为模式模块都要实现必需的回调函数，supervisor只需要模块实现init回调：

```
-callback init(Args :: term()) ->
    {ok, {SupFlags :: sup_flags(), [ChildSpec :: child_spec()]}} | ignore.
```

init回调返回三元组{ok, SupFlags, [ChildSpec]}，或者ignore。之前翻译的OTP规范文档有较详细的介绍，但是还是从源码里看一下SupFlags和ChildSpec的定义。



#### 监控策略

源码中策略的定义：

```
-type strategy() :: 'one_for_all' | 'one_for_one' | 'rest_for_one' | 'simple_one_for_one'.

-type sup_flags() :: #{strategy => strategy(),         % optional
					   intensity => non_neg_integer(), % optional
					   period => pos_integer()}        % optional
					 |{RestartStrategy :: strategy(),
                       Intensity :: non_neg_integer(),
                       Period :: pos_integer()}.
					  
-define(default_flags, #{strategy  => one_for_one,
						 intensity => 1,
						 period    => 5}).
```

strategy表示子进程重启策略，intensity和period表示重启间隔，在period时间内最多允许重启intensity次，违规后的策略根据strategy而不同。

#### 子进程规范

子进程规范定义和类型声明以及默认值：

```
-type child_id() :: term().
-type mfargs()   :: {M :: module(), F :: atom(), A :: [term()] | undefined}.
-type modules()  :: [module()] | 'dynamic'.
-type restart()  :: 'permanent' | 'transient' | 'temporary'.
-type shutdown() :: 'brutal_kill' | timeout().
-type worker()   :: 'worker' | 'supervisor'.

-type child_spec() :: #{id := child_id(),       % mandatory
						start := mfargs(),      % mandatory
						restart => restart(),   % optional
						shutdown => shutdown(), % optional
						type => worker(),       % optional
						modules => modules()}   % optional
                     | {Id :: child_id(),
                        StartFunc :: mfargs(),
                        Restart :: restart(),
                        Shutdown :: shutdown(),
                        Type :: worker(),
                        Modules :: modules()}.
						
-define(default_child_spec, #{restart => permanent, type => worker}).
```

监控器内部用id标识进程，start表示要启动的子进程模块、函数和参数。其余参数均为可选参数。

#### supervisor内部状态

由于supervisor是一个gen_server进程，因此需要维护服务器内部状态：

```
-type child()    :: 'undefined' | pid().

-record(child, {pid = undefined :: child() | {restarting, pid() | undefined} | [pid()], % pid is undefined when child is not running
				name            :: child_id(),
				mfargs          :: mfargs(),
				restart_type    :: restart(),
				shutdown        :: shutdown(),
				child_type      :: worker(),
				modules = []    :: modules()}).
				
-type child_rec() :: #child{}.

-define(DICTS, dict).
-define(DICT, dict:dict).
-define(SETS, sets).
-define(SET, sets:set).

-record(state, {name, 
				strategy :: strategy() | 'undefined', 
				children = [] :: [child_rec()], 
				dynamics :: {'dict', ?DICT(pid(), list())} | {'set', ?SET(pid())} | 'undefined',
				intensity              :: non_neg_integer() | 'undefined',
				period                 :: pos_integer() | 'undefined',
				restarts = [],
				dynamic_restarts = 0   :: non_neg_integer(),
				module,
				args}).
-type state() :: #state{}.
```

state保存了supervisor的参数以及所有子进程的信息。

#### supervisor启动

调用supervisor:start_link启动监控器，以模块名和参数列表为参数，参数列表会传给回调的init函数。

```
start_link() ->
    supervisor:start_link(worker_sup, []).
```

看一下supervisor:start_link/2的定义：

```
-spec start_link(Module, Args) -> startlink_ret() when
      Module :: module(),
      Args :: term().
start_link(Mod, Args) ->
    gen_server:start_link(supervisor, {self, Mod, Args}, []).
 
-spec start_link(SupName, Module, Args) -> startlink_ret() when
      SupName :: sup_name(),
      Module :: module(),
      Args :: term().
start_link(SupName, Mod, Args) ->
    gen_server:start_link(SupName, supervisor, {SupName, Mod, Args}, []).
```

通过gen_server:start_link启动进程，上文已经介绍过gen_server的启动流程——创建一个新进程并最终回调supervisor:init函数。

```
-define(is_simple(State), State#state.strategy =:= simple_one_for_one).

-type init_sup_name() :: sup_name() | 'self'.

-type stop_rsn() :: {'shutdown', term()}
                  | {'bad_return', {module(),'init', term()}}
                  | {'bad_start_spec', term()}
                  | {'start_spec', term()}
                  | {'supervisor_data', term()}.

-spec init({init_sup_name(), module(), [term()]}) ->
        {'ok', state()} | 'ignore' | {'stop', stop_rsn()}.

init({SupName, Mod, Args}) ->
    process_flag(trap_exit, true),
    case Mod:init(Args) of
	{ok, {SupFlags, StartSpec}} ->
	    case init_state(SupName, SupFlags, Mod, Args) of
		{ok, State} when ?is_simple(State) ->
		    init_dynamic(State, StartSpec);
		{ok, State} ->
		    init_children(State, StartSpec);
		Error ->
		    {stop, {supervisor_data, Error}}
	    end;
	ignore ->
	    ignore;
	Error ->
	    {stop, {bad_return, {Mod, init, Error}}}
    end.
```

首先调用process_flag设置自己为系统进程，然后回调init，即我们自己实现的worker_sup:init。

```
init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [#{id => worker, start => {worker, start_link, []}, restart => permanent, shutdown => brutal_kill, type => worker, modules => [worker]}],
    {ok, {SupFlags, ChildSpecs}}.
```

init_state接收监控器名、监控策略、子进程模块、子进程参数，返回supervisor进程的内部状态：

```
%%-----------------------------------------------------------------
%% Func: init_state/4
%% Args: SupName = {local, atom()} | {global, atom()} | self
%%       Type = {Strategy, MaxIntensity, Period}
%%         Strategy = one_for_one | one_for_all | simple_one_for_one |
%%                    rest_for_one
%%         MaxIntensity = integer() >= 0
%%         Period = integer() > 0
%%       Mod :== atom()
%%       Args :== term()
%% Purpose: Check that Type is of correct type (!)
%% Returns: {ok, state()} | Error
%%-----------------------------------------------------------------
init_state(SupName, Type, Mod, Args) ->
    set_flags(Type, #state{name = supname(SupName,Mod), module = Mod, args = Args}).

set_flags(Flags, State) ->
    try check_flags(Flags) of
	#{strategy := Strategy, intensity := MaxIntensity, period := Period} ->
	    {ok, State#state{strategy = Strategy, intensity = MaxIntensity, period = Period}}
    catch
		Thrown -> Thrown
    end.

check_flags(SupFlags) when is_map(SupFlags) ->
    do_check_flags(maps:merge(?default_flags,SupFlags));
check_flags({Strategy, MaxIntensity, Period}) ->
    check_flags(#{strategy => Strategy, intensity => MaxIntensity, period => Period});
check_flags(What) ->
    throw({invalid_type, What}).

do_check_flags(#{strategy := Strategy, intensity := MaxIntensity, period := Period} = Flags) ->
    validStrategy(Strategy),
    validIntensity(MaxIntensity),
    validPeriod(Period),
    Flags.

validStrategy(simple_one_for_one) -> true;
validStrategy(one_for_one)        -> true;
validStrategy(one_for_all)        -> true;
validStrategy(rest_for_one)       -> true;
validStrategy(What)               -> throw({invalid_strategy, What}).

validIntensity(Max) when is_integer(Max),
                         Max >=  0 -> true;
validIntensity(What)               -> throw({invalid_intensity, What}).

validPeriod(Period) when is_integer(Period),
                         Period > 0 -> true;
validPeriod(What)                   -> throw({invalid_period, What}).

supname(self, Mod) -> {self(), Mod};
supname(N, _)      -> N.
```

这一段代码很有Erlang风格，很好的利用Erlang的模式匹配，检查init返回的重启策略参数，并初始化state相关的值。

如果子进程重启策略的strategy键是simple_one_for_one，则调用init_dynamic，否则调用init_children。

#### simple_one_for_one和init_dynamic

simple_one_for_one用于动态生成子进程，初始化时没有任何子进程，通过调用supervisor:start_child，会按照子进程规范启动新的子进程。调用init_dynamic初始化。

```
init_dynamic(State, [StartSpec]) ->
    case check_startspec([StartSpec]) of
        {ok, Children} ->
	    {ok, State#state{children = Children}};
        Error ->
            {stop, {start_spec, Error}}
    end;
init_dynamic(_State, StartSpec) ->
    {stop, {bad_start_spec, StartSpec}}.
```

init_dynamic的第二个参数必须是子进程规范列表，check_startspec检查子进程启动参数：

```
%%% ------------------------------------------------------
%%% Check that the children start specification is valid.
%%% Input: [child_spec()]
%%% Returns: {ok, [child_rec()]} | Error
%%% ------------------------------------------------------

check_startspec(Children) -> check_startspec(Children, []).

check_startspec([ChildSpec|T], Res) ->
    case check_childspec(ChildSpec) of
	{ok, Child} ->
	    case lists:keymember(Child#child.name, #child.name, Res) of
		%% The error message duplicate_child_name is kept for
		%% backwards compatibility, although
		%% duplicate_child_id would be more correct.
		true -> {duplicate_child_name, Child#child.name};
		false -> check_startspec(T, [Child | Res])
	    end;
	Error -> Error
    end;
check_startspec([], Res) ->
    {ok, lists:reverse(Res)}.
```

check_startspec检索列表的所有子进程约束，调用check_childspec检查参数，并确保子进程名不会重复。

```
check_childspec(ChildSpec) when is_map(ChildSpec) ->
    catch do_check_childspec(maps:merge(?default_child_spec,ChildSpec));
check_childspec({Name, Func, RestartType, Shutdown, ChildType, Mods}) ->
    check_childspec(#{id => Name,
		      start => Func,
		      restart => RestartType,
		      shutdown => Shutdown,
		      type => ChildType,
		      modules => Mods});
check_childspec(X) -> {invalid_child_spec, X}.
```

check_childspec把子进程规范转换成map，并调用do_check_childspec，真正的参数检查：

```
do_check_childspec(#{restart := RestartType, type := ChildType} = ChildSpec)->
    Name = case ChildSpec of
	       #{id := N} -> N;
	       _ -> throw(missing_id)
	   end,
    Func = case ChildSpec of
	       #{start := F} -> F;
	       _ -> throw(missing_start)
	   end,
    validName(Name),
    validFunc(Func),
    validRestartType(RestartType),
    validChildType(ChildType),
    Shutdown = case ChildSpec of
		   #{shutdown := S} -> S;
		   #{type := worker} -> 5000;
		   #{type := supervisor} -> infinity
	       end,
    validShutdown(Shutdown),
    Mods = case ChildSpec of
	       #{modules := Ms} -> Ms;
	       _ -> {M,_,_} = Func, [M]
	   end,
    validMods(Mods),
    {ok, #child{name = Name, mfargs = Func, restart_type = RestartType,
		shutdown = Shutdown, child_type = ChildType, modules = Mods}}.

validChildType(supervisor) -> true;
validChildType(worker) -> true;
validChildType(What) -> throw({invalid_child_type, What}).

validName(_Name) -> true.

validFunc({M, F, A}) when is_atom(M), 
                          is_atom(F), 
                          is_list(A) -> true;
validFunc(Func)                      -> throw({invalid_mfa, Func}).

validRestartType(permanent)   -> true;
validRestartType(temporary)   -> true;
validRestartType(transient)   -> true;
validRestartType(RestartType) -> throw({invalid_restart_type, RestartType}).

validShutdown(Shutdown)
  when is_integer(Shutdown), Shutdown > 0 -> true;
validShutdown(infinity)             -> true;
validShutdown(brutal_kill)          -> true;
validShutdown(Shutdown)             -> throw({invalid_shutdown, Shutdown}).

validMods(dynamic) -> true;
validMods(Mods) when is_list(Mods) ->
    lists:foreach(fun(Mod) ->
		    if
			is_atom(Mod) -> ok;
			true -> throw({invalid_module, Mod})
		    end
		  end,
		  Mods);
validMods(Mods) -> throw({invalid_modules, Mods}).
```

do_check_childspec检查参数类型合法，并返回{ok, Child}。因此check_startspec会按照子进程约束列表的顺序，返回服务器状态所需的child列表。真正的启动过程将在后续start_child讨论中解释。

#### 正常的子进程启动

非simple_one_for_one类型的初始化过程会调用init_children：

```
init_children(State, StartSpec) ->
    SupName = State#state.name,
    case check_startspec(StartSpec) of
        {ok, Children} ->
            case start_children(Children, SupName) of
                {ok, NChildren} ->
                    {ok, State#state{children = NChildren}};
                {error, NChildren, Reason} ->
                    _ = terminate_children(NChildren, SupName),
                    {stop, {shutdown, Reason}}
            end;
        Error ->
            {stop, {start_spec, Error}}
    end.
```

和init_dynamic类似，首先检查参数。随后调用start_children启动子进程，同时会传入SupName：

```
%%-----------------------------------------------------------------
%% Func: start_children/2
%% Args: Children = [child_rec()] in start order
%%       SupName = {local, atom()} | {global, atom()} | {pid(), Mod}
%% Purpose: Start all children.  The new list contains #child's
%%          with pids.
%% Returns: {ok, NChildren} | {error, NChildren, Reason}
%%          NChildren = [child_rec()] in termination order (reversed
%%                        start order)
%%-----------------------------------------------------------------
start_children(Children, SupName) -> start_children(Children, [], SupName).

start_children([Child|Chs], NChildren, SupName) ->
    case do_start_child(SupName, Child) of
	{ok, undefined} when Child#child.restart_type =:= temporary ->
	    start_children(Chs, NChildren, SupName);
	{ok, Pid} ->
	    start_children(Chs, [Child#child{pid = Pid}|NChildren], SupName);
	{ok, Pid, _Extra} ->
	    start_children(Chs, [Child#child{pid = Pid}|NChildren], SupName);
	{error, Reason} ->
	    report_error(start_error, Reason, Child, SupName),
	    {error, lists:reverse(Chs) ++ [Child | NChildren], {failed_to_start_child,Child#child.name,Reason}}
    end;
start_children([], NChildren, _SupName) ->
    {ok, NChildren}.
```

start_children会按照子进程规范列表启动子进程，并按照反序存放在列表，因此服务器状态的#state.child存储顺序和启动顺序相反。真正的启动函数是do_start_child：

```
do_start_child(SupName, Child) ->
    #child{mfargs = {M, F, Args}} = Child,
    case catch apply(M, F, Args) of
	{ok, Pid} when is_pid(Pid) ->
	    NChild = Child#child{pid = Pid},
	    report_progress(NChild, SupName),
	    {ok, Pid};
	{ok, Pid, Extra} when is_pid(Pid) ->
	    NChild = Child#child{pid = Pid},
	    report_progress(NChild, SupName),
	    {ok, Pid, Extra};
	ignore ->
	    {ok, undefined};
	{error, What} -> {error, What};
	What -> {error, What}
    end.
```

do_start_child简单调用apply——这里的模块实际上只能是gen_server或supervisor行为模式的模块，比如worker:start_link，因此总是生成新的进程。最后调用report_progress把进程信息写入log日志。

因为supervisor进程本身也是一个gen_server进程，因此supervisor本身的初始化过程很简单：supervisor:start_link -> gen_server:init -> supervisor:init -> Mod:init -> init_state -> init_dynamic/init_children -> check_startspec -> start_children。

#### supervisor:start_child

start_child启动子进程：

```
-spec start_child(SupRef, ChildSpec) -> startchild_ret() when
      SupRef :: sup_ref(),
      ChildSpec :: child_spec() | (List :: [term()]).
start_child(Supervisor, ChildSpec) ->
    call(Supervisor, {start_child, ChildSpec}).
	
call(Supervisor, Req) ->
    gen_server:call(Supervisor, Req, infinity).
```

gen_server的call调用会回调handle_call函数：

```
handle_call({start_child, EArgs}, _From, State) when ?is_simple(State) ->
    Child = hd(State#state.children),
    #child{mfargs = {M, F, A}} = Child,
    Args = A ++ EArgs,
    case do_start_child_i(M, F, Args) of
	{ok, undefined} ->
	    {reply, {ok, undefined}, State};
	{ok, Pid} ->
	    NState = save_dynamic_child(Child#child.restart_type, Pid, Args, State),
	    {reply, {ok, Pid}, NState};
	{ok, Pid, Extra} ->
	    NState = save_dynamic_child(Child#child.restart_type, Pid, Args, State),
	    {reply, {ok, Pid, Extra}, NState};
	What ->
	    {reply, What, State}
    end;
	
handle_call({start_child, ChildSpec}, _From, State) ->
    case check_childspec(ChildSpec) of
	{ok, Child} ->
	    {Resp, NState} = handle_start_child(Child, State),
	    {reply, Resp, NState};
	What ->
	    {reply, {error, What}, State}
    end;
```

首先处理simple_one_for_one的子进程创建，去子进程列表的第一个规范，然后调用do_start_child_i启动进程，并通过save_dynamic_child保存动态子进程信息：

```
do_start_child_i(M, F, A) ->
    case catch apply(M, F, A) of
	{ok, Pid} when is_pid(Pid) ->
	    {ok, Pid};
	{ok, Pid, Extra} when is_pid(Pid) ->
	    {ok, Pid, Extra};
	ignore ->
	    {ok, undefined};
	{error, Error} ->
	    {error, Error};
	What ->
	    {error, What}
    end.
	
save_dynamic_child(temporary, Pid, _, #state{dynamics = Dynamics} = State) ->
    DynamicsDb = dynamics_db(temporary, Dynamics),
    State#state{dynamics = {set, ?SETS:add_element(Pid, DynamicsDb)}};
save_dynamic_child(RestartType, Pid, Args, #state{dynamics = Dynamics} = State) ->
    DynamicsDb = dynamics_db(RestartType, Dynamics),
    State#state{dynamics = {dict, ?DICTS:store(Pid, Args, DynamicsDb)}}.

dynamics_db(temporary, undefined) ->
    ?SETS:new();
dynamics_db(_, undefined) ->
    ?DICTS:new();
dynamics_db(_, {_Tag, DynamicsDb}) ->
    DynamicsDb.
```

temporary子进程会保存到sets中，而其他子进程通过dicts字典保存。

其它类型的子进程生成略有不同。首先传入的参数是一个子进程规范——而不是额外参数，说明可以生成一个和init返回规范不同的子进程。通过调用handle_start_child实现：

```
handle_start_child(Child, State) ->
    case get_child(Child#child.name, State) of
	false ->
	    case do_start_child(State#state.name, Child) of
		{ok, undefined} when Child#child.restart_type =:= temporary ->
		    {{ok, undefined}, State};
		{ok, Pid} ->
		    {{ok, Pid}, save_child(Child#child{pid = Pid}, State)};
		{ok, Pid, Extra} ->
		    {{ok, Pid, Extra}, save_child(Child#child{pid = Pid}, State)};
		{error, What} ->
		    {{error, {What, Child}}, State}
	    end;
	{value, OldChild} when is_pid(OldChild#child.pid) ->
	    {{error, {already_started, OldChild#child.pid}}, State};
	{value, _OldChild} ->
	    {{error, already_present}, State}
    end.
```

首先检查子进程名字是否已经存在，然后调用do_start_child，如果子进程是temporary，并且init回调返回ignore，不会保存进程信息——尽可能保持轻量，反之调用save_child：

```
save_child(#child{restart_type = temporary, mfargs = {M, F, _}} = Child, #state{children = Children} = State) ->
    State#state{children = [Child#child{mfargs = {M, F, undefined}} |Children]};
save_child(Child, #state{children = Children} = State) ->
    State#state{children = [Child |Children]}.
```

save_child会省略temporary的参数信息。


#### 子进程退出和重启

supervisor监控器存在的意义就是子进程退出时根据监控策略和子进程规范，选择重启或者停止进程。子进程退出时，监控进程会收到{'EXIT', ...}信息，看一下处理过程：

```
handle_info({'EXIT', Pid, Reason}, State) ->
    case restart_child(Pid, Reason, State) of
	{ok, State1} ->
	    {noreply, State1};
	{shutdown, State1} ->
	    {stop, shutdown, State1}
    end;
```

restart_child重启进程：

```
restart_child(Pid, Reason, #state{children = [Child]} = State) when ?is_simple(State) ->
    RestartType = Child#child.restart_type,
    case dynamic_child_args(Pid, RestartType, State#state.dynamics) of
	{ok, Args} ->
	    {M, F, _} = Child#child.mfargs,
	    NChild = Child#child{pid = Pid, mfargs = {M, F, Args}},
	    do_restart(RestartType, Reason, NChild, State);
	error ->
            {ok, State}
    end;

restart_child(Pid, Reason, State) ->
    Children = State#state.children,
    case lists:keyfind(Pid, #child.pid, Children) of
	#child{restart_type = RestartType} = Child ->
	    do_restart(RestartType, Reason, Child, State);
	false ->
	    {ok, State}
    end.
```

对于simple_one_for_one进程，调用dynamic_child_args从动态进程列表中获取参数；其它进程，则直接从Children列表中查找子进程规范。最后都会调用do_restart：

```
do_restart(permanent, Reason, Child, State) ->
    report_error(child_terminated, Reason, Child, State#state.name),
    restart(Child, State);
do_restart(_, normal, Child, State) ->
    NState = state_del_child(Child, State),
    {ok, NState};
do_restart(_, shutdown, Child, State) ->
    NState = state_del_child(Child, State),
    {ok, NState};
do_restart(_, {shutdown, _Term}, Child, State) ->
    NState = state_del_child(Child, State),
    {ok, NState};
do_restart(transient, Reason, Child, State) ->
    report_error(child_terminated, Reason, Child, State#state.name),
    restart(Child, State);
do_restart(temporary, Reason, Child, State) ->
    report_error(child_terminated, Reason, Child, State#state.name),
    NState = state_del_child(Child, State),
    {ok, NState}.
```

do_restart以重启类型、退出原因、子进程规范和服务器状态为参数，选择重启策略：

* permanent 进程一定会被重启
* 退出原因是normal、shutdown、{shutdown, term()}的进程不会被重启
* transient 进程在其他情况退出会重启
* 临时进程不会被重启

state_del_child会在dynamic列表或者字典中删除子进程的信息。

最后的步骤是 restart 重启。这是 supervisor 功能核心，根据重启策略的 strategy 键值，确定重启行为；

```
restart(Child, State) ->
    case add_restart(State) of
	{ok, NState} ->
	    case restart(NState#state.strategy, Child, NState) of
		{try_again,NState2} ->
		    %% Leaving control back to gen_server before
		    %% trying again. This way other incoming requsts
		    %% for the supervisor can be handled - e.g. a
		    %% shutdown request for the supervisor or the
		    %% child.
		    Id = if ?is_simple(State) -> Child#child.pid;
			    true -> Child#child.name
			 end,
		    ok = try_again_restart(self(), Id),
		    {ok,NState2};
		{try_again, NState2, #child{name=ChName}} ->
		    ok = try_again_restart(self(), ChName),
		    {ok,NState2};
		Other ->
		    Other
	    end;
	{terminate, NState} ->
	    report_error(shutdown, reached_max_restart_intensity,
			 Child, State#state.name),
	    {shutdown, remove_child(Child, NState)}
    end.
```

首先调用 add_restart 检查重启频率，这也是重启策略的重启强度参数起作用的时候：

```
add_restart(State) ->  
    I = State#state.intensity,
    P = State#state.period,
    R = State#state.restarts,
    Now = erlang:monotonic_time(1),
    R1 = add_restart([Now|R], Now, P),
    State1 = State#state{restarts = R1},
    case length(R1) of
	CurI when CurI  =< I ->
	    {ok, State1};
	_ ->
	    {terminate, State1}
    end.

add_restart([R|Restarts], Now, Period) ->
    case inPeriod(R, Now, Period) of
	true ->
	    [R|add_restart(Restarts, Now, Period)];
	_ ->
	    []
    end;
add_restart([], _, _) ->
    [].

inPeriod(Then, Now, Period) ->
    Now =< Then + Period.
```

add_restart 维护当前时段的所有重启进程，如果超过intensity，返回terminate，restart会返回{shutdown, ...}，让整个监控树终止。没有超过重启强度时，调用restart/3具体的实现重启：

##### one_for_one

```
restart(one_for_one, Child, State) ->
    OldPid = Child#child.pid,
    case do_start_child(State#state.name, Child) of
	{ok, Pid} ->
	    NState = replace_child(Child#child{pid = Pid}, State),
	    {ok, NState};
	{ok, Pid, _Extra} ->
	    NState = replace_child(Child#child{pid = Pid}, State),
	    {ok, NState};
	{error, Reason} ->
	    NState = replace_child(Child#child{pid = restarting(OldPid)}, State),
	    report_error(start_error, Reason, Child, State#state.name),
	    {try_again, NState}
    end;
```

one_for_one 是最简单的情况，子进程不影响其他进程，所以这里只是简单调用do_start_child重启name名的进程，并替换服务器保存的子进程数据。

##### rest_for_one

```
restart(rest_for_one, Child, State) ->
    {ChAfter, ChBefore} = split_child(Child#child.pid, State#state.children),
    ChAfter2 = terminate_children(ChAfter, State#state.name),
    case start_children(ChAfter2, State#state.name) of
	{ok, ChAfter3} ->
	    {ok, State#state{children = ChAfter3 ++ ChBefore}};
	{error, ChAfter3, {failed_to_start_child, ChName, _Reason}}
	  when ChName =:= Child#child.name ->
	    NChild = Child#child{pid=restarting(Child#child.pid)},
	    NState = State#state{children = ChAfter3 ++ ChBefore},
	    {try_again, replace_child(NChild,NState)};
	{error, ChAfter3, {failed_to_start_child, ChName, _Reason}} ->
	    NChild = lists:keyfind(ChName, #child.name, ChAfter3),
	    NChild2 = NChild#child{pid=?restarting(undefined)},
	    NState = State#state{children = ChAfter3 ++ ChBefore},
	    {try_again, replace_child(NChild2,NState), NChild2}
    end;
```

rest_for_one 策略时，子进程重启会先终止子进程启动之后的子进程，并重启所有这些子进程。

首先调用 split_child 把子进程列表分开：

```
%% Chs = [S4, S3, Ch, S1, S0]
%% Ret: {[S4, S3, Ch], [S1, S0]}
split_child(Name, Chs) ->
    split_child(Name, Chs, []).

split_child(Name, [Ch|Chs], After) when Ch#child.name =:= Name ->
    {lists:reverse([Ch#child{pid = undefined} | After]), Chs};
split_child(Pid, [Ch|Chs], After) when Ch#child.pid =:= Pid ->
    {lists:reverse([Ch#child{pid = undefined} | After]), Chs};
split_child(Name, [Ch|Chs], After) ->
    split_child(Name, Chs, [Ch | After]);
split_child(_, [], After) ->
    {lists:reverse(After), []}.
```

ChBefore是不需要重启的。首先调用terminate_children终止ChAfter进程，需要注意的是因为temporary进程不会被重启，因此这里也会跳过temporary进程；终止子进程时可以有超时值，允许子进程执行某些清理工作。

start_children会启动所有子进程，如果启动失败，会返回try_again。

在restart/2中，针对{try_again, ...}会调用 try_again_restart ，然后返回。

```
%%% Called by restart/2
-spec try_again_restart(SupRef, Child) -> ok when
      SupRef :: sup_ref(),
      Child :: child_id() | pid().
try_again_restart(Supervisor, Child) ->
    cast(Supervisor, {try_again_restart, Child}).
```

发送一个异步请求，然后返回；这样做是为了处理其他的消息——如终止监控树等。这是很好的编程范例。这里顺便看一下handle_cast处理try_again_restart的过程：

```
-spec handle_cast({try_again_restart, child_id() | pid()}, state()) ->
			 {'noreply', state()} | {stop, shutdown, state()}.

handle_cast({try_again_restart,Pid}, #state{children=[Child]}=State)
  when ?is_simple(State) ->
    RT = Child#child.restart_type,
    RPid = restarting(Pid),
    case dynamic_child_args(RPid, RT, State#state.dynamics) of
	{ok, Args} ->
	    {M, F, _} = Child#child.mfargs,
	    NChild = Child#child{pid = RPid, mfargs = {M, F, Args}},
	    case restart(NChild,State) of
		{ok, State1} ->
		    {noreply, State1};
		{shutdown, State1} ->
		    {stop, shutdown, State1}
	    end;
	error ->
            {noreply, State}
    end;

handle_cast({try_again_restart,Name}, State) ->
    case lists:keyfind(Name,#child.name,State#state.children) of
	Child = #child{pid=?restarting(_)} ->
	    case restart(Child,State) of
		{ok, State1} ->
		    {noreply, State1};
		{shutdown, State1} ->
		    {stop, shutdown, State1}
	    end;
	_ ->
	    {noreply,State}
    end.
```

只是再次执行restart流程。

##### one_for_all

```
restart(one_for_all, Child, State) ->
    Children1 = del_child(Child#child.pid, State#state.children),
    Children2 = terminate_children(Children1, State#state.name),
    case start_children(Children2, State#state.name) of
	{ok, NChs} ->
	    {ok, State#state{children = NChs}};
	{error, NChs, {failed_to_start_child, ChName, _Reason}}
	  when ChName =:= Child#child.name ->
	    NChild = Child#child{pid=restarting(Child#child.pid)},
	    NState = State#state{children = NChs},
	    {try_again, replace_child(NChild,NState)};
	{error, NChs, {failed_to_start_child, ChName, _Reason}} ->
	    NChild = lists:keyfind(ChName, #child.name, NChs),
	    NChild2 = NChild#child{pid=?restarting(undefined)},
	    NState = State#state{children = NChs},
	    {try_again, replace_child(NChild2,NState), NChild2}
    end.
```

类似 rest_for_one，只不过这次会终止所有进程，然后再重启。

##### simple_one_for_one

```
restart(simple_one_for_one, Child, State0) ->
    #child{pid = OldPid, mfargs = {M, F, A}} = Child,
    State = case OldPid of
		?restarting(_) ->
		    NRes = State0#state.dynamic_restarts - 1,
		    State0#state{dynamic_restarts = NRes};
		_ ->
		    State0
	    end,
    Dynamics = ?DICTS:erase(OldPid, dynamics_db(Child#child.restart_type,
					       State#state.dynamics)),
    case do_start_child_i(M, F, A) of
	{ok, Pid} ->
            DynamicsDb = {dict, ?DICTS:store(Pid, A, Dynamics)},
	    NState = State#state{dynamics = DynamicsDb},
	    {ok, NState};
	{ok, Pid, _Extra} ->
            DynamicsDb = {dict, ?DICTS:store(Pid, A, Dynamics)},
	    NState = State#state{dynamics = DynamicsDb},
	    {ok, NState};
	{error, Error} ->
	    NRestarts = State#state.dynamic_restarts + 1,
            DynamicsDb = {dict, ?DICTS:store(restarting(OldPid), A, Dynamics)},
	    NState = State#state{dynamic_restarts = NRestarts,
				 dynamics = DynamicsDb},
	    report_error(start_error, Error, Child, State#state.name),
	    {try_again, NState}
    end;
```

simple_one_for_one是最特殊的，因为所有子进程都按照一份子进程规范动态生成。所以子进程退出时只是简单的新建一个进程，并更新动态子进程列表。

总体来说，supervisor 行为模式的实现很简单，这是理所当然的，监控器做的事要尽可能少。另外，它基于gen_server，通过服务器状态维护子进程和动态子进程信息，根据用户设置的重启策略和子进程规范，实现健壮的监控模型。