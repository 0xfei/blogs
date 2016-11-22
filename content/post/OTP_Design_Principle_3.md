+++
title = "9 OTP设计原则——第三部分"
date = "2016-11-21T04:09:05+08:00"
draft = true
description = "OTP Design Priciple"
tags = ["Erlang/OTP", "translation"]
topics = ["Erlang/OTP", "translation"]
+++


#### 9.5 gen_event 行为模式

这一章节结合 *stdlib* man page中的 *gen_event(3)* 阅读，man page中详细介绍了gen_event行为模式的接口函数和回调函数。

##### 9.5.1 事件处理原则

在OTP中，事件管理器是一个可以接收并记录事件的命名对象。错误、警告和其他信息都是可以被记录的事件。

在事件管理器中，可以安装一个或多个事件处理模块。当事件处理器接收到事件通知时，所有安装的事件处理模块会处理这个事件。例如，处理错误的事件管理器默认有一个事件处理器，它会把错误日志写到终端。如果特定时期的错误信息需要保存到文件中，那么用户可以安装一个事件处理器来做这件事。当不需要写入文件时，这个事件处理器可以被删除。

事件管理器可以用进程的形式实现，而事件管理器可以实现它的回调模块。

事件管理器内部维护一个{Module, State}列表，每个Module是一个事件处理器，State是事件处理器的内部状态。

##### 9.5.2 例子

向终端输出错误信息的事件处理器回调模块代码看起来如下：

```
-module(terminal_logger).
-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2]).

init(_Args) ->
	{ok, []}.
handle_event(ErrorMsg, State) ->
	io:format("***Error*** ~p~n", [ErrorMsg]),
	{ok, State}.
terminate(_Args, _State) ->
	ok.
```

把错误信息保存到文件的事件处理器回调模块代码看起来如下：

```
-module(file_logger).
-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2]).
init(File) ->
	{ok, Fd} = file:open(File, read),
	{ok, Fd}.
handle_event(ErrorMsg, Fd) ->
	io:format(Fd, "***Error*** ~p~n", [ErrorMsg]),
	{ok, Fd}.
terminate(_Args, Fd) ->
	file:close(Fd).
```

下面小节详细介绍这个例子。


##### 9.5.3 启动事件管理器

为了启动前一节描述的处理错误的事件管理器，要调用下面的函数：

```
gen_event:start_link({local, error_man}).
```

这个函数创建并链接一个新的进程，一个事件管理器。

参数{local, error_man}表示进程名，表示在本地注册error_man；如果省略名字，gen_fsm进程没有注册，因此要用到Pid。name也可以是{global, Name}的形式，这样的话事件管理器要用global:register_name/2注册。

当gen_event进程作为监控树的一部分时，gen_event:start_link会被监控进程调用以启动进程；作为独立进程时，可以调用gen_event:start启动。


##### 9.5.4 添加事件处理器

下面的代码演示了如何在shell中启动事件管理器并添加一个事件处理器：

```
1> gen_event:start({local, error_man}).
{ok,<0.31.0>}
2> gen_event:add_handler(error_man, terminal_logger, []).
ok
```

这个函数给注册为error_man的事件管理器发送一个消息，让它添加事件处理器terminal_logger。这个事件管理器会回调terminal_logger:init([])，[]是add_handler的第三个参数。init应该返回{ok, State}，State是事件处理器的内部状态。

```
init(_Args) ->
	{ok, []}.
```

这里，init不需要用到输入数据，因此忽略参数。对于terminal_logger，内部状态同样不需要。对于file_logger，内部状态是文件描述符。

```
init(File) ->
	{ok, Fd} = file:open(File, read),
	{ok, Fd}.
```

##### 9.5.5 事件通知

```
3> gen_event:notify(error_man, no_reply).
***Error*** no_reply
ok
```

error_man是事件管理器的名称，no_reply是事件。

事件会被当做消息发送给消息管理器。当事件到达时，事件管理器按照安装顺序调用每个事件处理器的handle_event(Event, State)。handle_event函数应该返回{ok, State1}，State1是事件处理器的新状态。

在terminal_logger中：

```
handle_event(ErrorMsg, State) ->
	io:format("***Error*** ~p~n", [ErrorMsg]),
	{ok, State}.
```

在file_logger中：

```
handle_event(ErrorMsg, Fd) ->
	io:format(Fd, "***Error*** ~p~n", [ErrorMsg]),
	{ok, Fd}.
```

##### 9.5.6 删除事件处理器

```
4> gen_event:delete_handler(error_man, terminal_logger, []).
ok
```

这个函数给注册为error_man的事件管理器发送一个消息，让它删除事件处理器terminal_logger。事件管理器会调用回调函数terminal_logger:terminate([], State),[]是delete_handler的的第三个参数。terminate对应init，做必要的清理工作，返回值会被忽略。

对于terminal_logger，不需要清理资源：

```
terminate(_Args, _State) ->
	ok.
```

对于file_logger，open中打开的文件描述符需要关闭：

```
terminate(_Args, Fd) ->
	file:close(Fd).
```

##### 9.3.7 停止

当事件管理器停止时，会调用每个事件处理器的terminate/2函数做清理工作，类似删除事件处理器。

###### 在监控树中

如果事件管理器是监控树的一部分，不需要提供专门的stop函数，会被它的监控进程supervisor自动终止。具体如何终止通过在监控进程中设置shutdown strategy实现。

###### 独立gen_fsm进程

事件管理器可以通过调用下面的函数停止：

```
> gen_event:stop(error_man).
ok
```

##### 9.3.8 其他消息处理

如果gen_event收到其他消息，必须实现回调函数handle_info(Info, State)用来处理他们（这里原文似乎有个错误）。其他消息包括退出信号，当gen_event进程链接其他进程并捕捉exit信号时。

```
handle_info({'EXIT', Pid, Reason}, StateName, StateData) ->
	..code to handle exits here..
	{ok, NewState}.
```

此外，回调函数code_change必须实现。

```
code_change(OldVsn, State, Extra) ->
	..code to convert state (and more) during code change
	{ok, NewState}
```



#### 9.6 supervisor 行为模式

这一章节结合 *stdlib* man page中的 *supervisor(3)* 阅读，man page中详细介绍了supervisor行为模式的接口函数和回调函数。

##### 9.6.1 监控原则

一个监控器负责启动、停止和监控它的所有子进程。监控器的基本思想是必要的时候通过重启保持子进程存活。

子进程的启动和监控通过 *子进程规范(child specifications)* 列表来确定。子进程按列表顺序启动，按相反顺序终止。

##### 9.6.2 例子

一个gen_server行为模式服务器的supervisor的回调模块如下代码：

```
-module(ch_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link(ch_sup, []).
init(_Args) ->
	SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
	ChildSpecs = [#{id => ch3,start => {ch3, start_link, []},restart => permanent,shutdown => brutal_kill,type => worker,modules => [cg3]}],
	{ok, {SupFlags, ChildSpecs}}.
```

init/1返回的SupFlags变量代表监视器标志。

返回列表的ChildSpecs变量是子进程规范的列表。

##### 9.6.3 监视器标志

这是监视器标志位的类型定义：

```
sup_flags() = #{strategy => strategy(), % optional
				intensity => non_neg_integer(), % optional
				period => pos_integer()} % optional
strategy() = one_for_all | one_for_one | rest_for_one | simple_one_for_one
```

*   *strategy* 表示重启策略
*	*intensity* 和 *period* 表明 *重启强度(maximum restart intensity)*

##### 9.6.4 重启策略

重启策略通过回调函数init返回的监视器标志的strategy键值确定：

```
SupFlags = #{strategy => Stratege, ...}
```

strategy键是可选的，默认是 *one_for_one* 。

###### one_for_one

如果一个子进程意外终止，只有它自己会被重启。

###### one_for_all

如果一个子进程意外终止，所有子进程都会被终止，然后重启——类似进程组“同生同灭”的概念。

###### rest_for_one

如果一个子进程意外终止，剩余的子进程（按启动顺序排列，在终止的进程后面启动的子进程）也被终止。随后这些子进程会重启——说明这种模式下子进程的顺序是有关联的。

###### simple_one_for_one

具体查看 *simple_one_for_one 监视器* 部分。


##### 9.6.5 重启强度(Maximum Restart Intensity)

监视器有一个内建机制用于限制给定时间段内最多可以重启的次数。监视器标志中的键值 *intensity* 和 *period* 来确定。

```
SupFlags = #{intensity => MaxR, period => MaxT, ...}
```

如果在过去的MaxT秒时间超过MaxR次重启发生，那么监视器会终止所有子进程，然后自己退出。

当监视器终止时，高一级的监视器会采取动作。要么重启已终止的监视器，要么终止自己。

重启机制的目的是为了防止一个进程因为同样的情况反复重启、退出。

键值 *intensity* 和 *period* 是可选的， 默认为 1 和 5。

##### 9.6.6 子进程规范

子进程规范的类型定义如下：

```
child_spec() = #{
			id => child_id(), 		%% 必须
			start => mfargs(), 		%% 必须
			restart => restart(), 	%% 可选
			shutdown => shutdown(), %% 可选
			type => worker(), 		%% 可选
			modules => modules() 	%% 可选
			}
child_id() = term()
mfargs() = {M :: module(), F :: atom(), A :: [term()]}
modules() = [module()] | dynamic
restart() = permanent | transient | temporary
shutdown() = brutal_kill | timeout()
worker() = worker | supervisor
```

* id 用于监视器内部明确标识子进程，id键是必需的。注意这个标识有时候被称为"name"。我们尽量使用“identifier”或者“id”，但是为了向下兼容，有时候会看到“name”的使用，例如在某些错误信息中。

* start 定义了子进程要启动的线程。是模块-函数-参数元组，用于 apply(M, F, A)。它会导致下面的几个调用：
	* supervisor:start_link
	* gen_server:start_link
	* gen_fsm: start_link
	* gen_statem: start_link
	* gen_event: start_link
	* 和上面函数兼容的其它函数。更多细节阅读 *supervisor(3)* 的man page。
  start是必需的。

* restart 定义了如何重启一个终止的进程。
	* permanent 子进程一定会被重启
	* temporary 子进程不会被重启（监视器的重启策略是rest_for_one或one_for_all，其他进程可能导致temporary进程终止）。
	* transient 子进程只有意外终止时会被重启，即退出原因不是normal、shutdown或{shutdown, Term}。
  restart是可选的，默认值为permanent。

* shutdown 定义子进程如何被终止。
	* brutal_kill 表示子进程会无条件的被 exit(Child, kill) 终止。
	* 整型超时值表示监视器会通过exit(Child, shutdown)通知子进程退出，然后等到退出信号。如果特定时间内没有收到信号，就会用exit(Child, kill)无条件退出。
	* 如果子进程是监视器，shutdown被设为infinity，从而让子进程树有充足的时间完成终止。也可以给工作进程设定这个值。看一下下面的提醒：
	
	*警告：要小心给工作进程设定关闭时间为infinity。因为在这种情况下，监控树的终止取决于子进程；这个资金从必须用安全的方式实现，确保清理过程一定会返回。*
  shutdown标志是可选的。对于worker进程，默认是5000；对于supervisor子进程，默认值是infinity。
	
* type 标志表明子进程是工作进程或监控器。
  type是可选的，默认为worker进程。
	
* modules 是只有一个成员的列表，[Module]，如果子进程是supervisor、gen_server、gen_fsm或gen_statem，Module是回调模块的名称。如果子进程是gen_event，这个值应该是dynamic。
  模块信息用于升级或回退版本时发行控制，查看 *Release Handling* 部分。
  modules键是可选的，默认情况下为子进程启动{M, F, A}中的M。
  
**例子** ：服务器ch3的子进程标识符如下：

```
#{id => ch3, start => {ch3, start_link, []}, restart => permanent, shutdown => brutal_kill, type => worker, modules => [ch3]}
```

或者省去默认值，简单写：

```
#{id => ch3, start => {ch3, start_link, []}, shutdown => brutal_kill} （官方文档似乎少了个逗号）
```

**例子** ：子进程为事件管理器：

```
#{id => error_man, start => {gen_event, start_link, [{local, error_man}]}, modules => dynamic}
```

ch3服务器和事件管理器的注册进程应该一直可以访问，因此重启策略使用默认值 permanent。

ch3退出时不需要清理资源，因此用brutal_kill终止策略。error_man需要一些时间来删除事件处理器，因此使用默认值5000ms。

**例子** ：子进程是监视器：

```
#{id => sup, start => {sup, start_link, []}, restart => transient, type => supervisor} % will cause default shutdown=>infinity
```

##### 启动监视器

在前面的章节，监视器通过调用ch_sup:start_link()启动：

```
start_link() ->
	supervisor:start_link(ch_sup, []).
```

ch_sup:start_link调用 supervisor:start_link/2，创建并链接新的监视器进程。

* 第一个参数，ch_sup，是回调模块的名称，也是init回调函数所在的模块。
* 第二个参数，[]，是传给init函数的参数，这里是空。

这个例子中，监视器没有注册，因此Pid需要用到。注册名可以通过调用 supervisor:start_link({local, Name}, Module, Args) 或者 supervisor:start_link({global, Name}, Module, Args)。

新进程回调 ch_sup:init([])，init应该返回 {ok, {SupFlags, ChildSpecs}}：

```
init(_Args) -> 
	SupFlags = #{}, 
	ChildSpecs = [#{id => ch3, start => {ch3, start_link, []}, shutdown => brutal_kill}], 
	{ok, {SupFlags, ChildSpecs}}.
```

监视器会根据子进程规范启动进程，这里会启动ch3进程。

supervisor:start_link是同步的，因此所有子进程启动之后它才会返回。

##### 9.6.8 新增子进程

除了固定的监控树，动态子进程可以被添加到已有的监控树：

```
supervisor:start_child(Sup, ChildSpec)
```

Sup是pid或者进程名。ChildSpec是 *子进程规范* .

通过调用start_child/2新增的子进程类似其他子进程，只有一个区别：监视器退出并重启后，动态添加的子进程会丢失。

##### 9.6.9 停止子进程

任何子进程——动态添加或者静态生成的，都可以通过下面的调用终止：

```
supervisor:terminate_child(Sup, Id)
```

可以用下面的调用删除一个已停止的子进程：

```
supervisor:delete_child(Sup, Id)
```

Sup是监视器的pid或者注册名，Id是 *子进程规范* 中指定的id键。

类似动态添加的子进程，删除子进程之后，监视器重启后它会丢失——即不会再次生成。

##### 9.6.10 简单一对一监视器

监视器重启策略simple_one_for_one是one_for_one的简单版本，所有子进程用同样的进程实例模板动态添加。

下面的例子是一个simple_one_for_one监视器：

```
-module(simple_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link(simple_sup, []).
init(_Args) ->
	SupFlags = #{strategy => simple_one_for_one, intensity => 0, period => 1},
	ChildSpecs = [#{id => call, start => {call, start_link, []}, shutdown => brutal_kill}],
	{ok, {SupFlags, ChildSpecs}}.
```

监视器启动时，并不会创建任何子进程。相反，所有子进程通过下面的调用动态添加：

```
supervisor:start_child(Sup, List)
```

Sup是监视器的pid或注册名，List是自定义的原子列表，会被添加到子进程的参数列表。如果启动函数是{ M, F, A}，这个子进程会调用 apply(M, F, A++List) 启动。

例如，给simple_sup添加一个子进程：

```
supervisor:start_child(Pid, [id1])
```

这个子进程通过调用apply(call, start_link, []++[id1])启动，实际上如下：

```
call:start_link(id1)
```

simple_one_for_one监视器的子进程可以通过下面的调用终止：

```
supervisor:terminate_child(Sup, Pid)
```

Sup和Pid的意义不言而喻。

因为一个simple_one_for_one可以有很多子进程，因此会异步终止它们。这意味着子进程可以并行的清理资源，终止顺序也不一定。

##### 9.6.11 终止。

因为监视器是监控树的一部分，它会被它的监视器自动终止。被要求关闭时，它会按照启动的相反顺序终止所有子进程，最后停止自己。


#### 9.7 sys和proc_lib

sys模块有一些用行为模式实现的函数，可以简单调试进程。还有一些函数，结合proc_lib模块，可以用于实现遵从OTP设计原则的 **特殊进程** 而不必使用标准行为模式。这些函数可以用于实现自定义的行为模式。

sys和proc_lib模块都属于STDLIB应用。

###### 9.7.1 简单调试

sys模块有一些使用行为模式实现的调试函数。*gen_fsm行为模式* 章节的code_lock例子用来演示：

```
% erl
Erlang (BEAM) emulator version 5.2.3.6 [hipe] [threads:0]
Eshell V5.2.3.6 (abort with ^G)
1> code_lock:start_link([1,2,3,4]).
{ok,<0.32.0>}
2> sys:statistics(code_lock, true).
ok
3> sys:trace(code_lock, true).
ok
4> code_lock:button(4).
*DBG* code_lock got event {button,4} in state closed
ok
*DBG* code_lock switched to state closed
5> code_lock:button(3).
*DBG* code_lock got event {button,3} in state closed
ok
*DBG* code_lock switched to state closed
6> code_lock:button(2).
*DBG* code_lock got event {button,2} in state closed
ok
*DBG* code_lock switched to state closed
7> code_lock:button(1).
*DBG* code_lock got event {button,1} in state closed
ok
OPEN DOOR
*DBG* code_lock switched to state open
*DBG* code_lock got event timeout in state open
CLOSE DOOR
*DBG* code_lock switched to state closed
8> sys:statistics(code_lock, get).
{ok,[{start_time,{{2003,6,12},{14,11,40}}},{current_time,{{2003,6,12},{14,12,14}}},{reductions,333},{messages_in,5},{messages_out,0}]}
9> sys:statistics(code_lock, false).
ok
10> sys:trace(code_lock, false).
ok
11> sys:get_status(code_lock).
{status,<0.32.0>,
	{module,gen_fsm},
	[[{'$ancestors',[<0.30.0>]},
	 {'$initial_call',{gen,init_it,[gen_fsm,<0.30.0>,<0.30.0>,{local,code_lock},code_lock,[1,2,3,4],[]]}}],
	 running,<0.30.0>,[],
	 [code_lock,closed,{[],[1,2,3,4]},code_lock,infinity]]}
```

##### 9.7.2 特殊进程

这一节介绍符合OTP设计规范的特殊进程，而不使用标准行为模式。这样的进程可以用于：
* 以某种方式启动进程从而适应一个监控树
* 支持 sys 的 *调试机制*
* 处理 *系统消息*

系统消息是由特殊含义的消息，在监控树中使用。典型的系统消息有跟踪输出、暂停和恢复进程运行（发型控制中使用）。使用标准行为模式实现的进程自动理解这些消息。

###### 例子

来自 *Overview* 中的简单例子，用sys和proc_lib实现，来适应监控树：

```
-module(ch4).
-export([start_link/0]).
-export([alloc/0, free/1]).
-export([init/1]).
-export([system_continue/3, system_terminate/4, write_debug/3, system_get_state/1, system_replace_state/2]).

start_link() ->
	proc_lib:start_link(ch4, init, [self()]).
	
alloc() ->
	ch4 ! {self(), alloc},
	receive
		{ch4, Res} ->
			Res
	end.
	
free(Ch) ->
	ch4 ! {free, Ch},
	ok.
	
init(Parent) ->
	register(ch4, self()),
	Chs = channels(),
	Deb = sys:debug_options([]),
	proc_lib:init_ack(Parent, {ok, self()}),
	loop(Chs, Parent, Deb).
	
loop(Chs, Parent, Deb) ->
	receive
		{From, alloc} ->
			Deb2 = sys:handle_debug(Deb, fun ch4:write_debug/3,
			ch4, {in, alloc, From}),
			{Ch, Chs2} = alloc(Chs),
			From ! {ch4, Ch},
			Deb3 = sys:handle_debug(Deb2, fun ch4:write_debug/3, ch4, {out, {ch4, Ch}, From}),
			loop(Chs2, Parent, Deb3);
		{free, Ch} ->
			Deb2 = sys:handle_debug(Deb, fun ch4:write_debug/3,ch4, {in, {free, Ch}}),
			Chs2 = free(Ch, Chs),
			loop(Chs2, Parent, Deb2);
		{system, From, Request} ->
			sys:handle_system_msg(Request, From, Parent,ch4, Deb, Chs)
	end.
	
system_continue(Parent, Deb, Chs) ->
	loop(Chs, Parent, Deb).
	
system_terminate(Reason, _Parent, _Deb, _Chs) ->
	exit(Reason).
	
system_get_state(Chs) ->
	{ok, Chs}.
	
system_replace_state(StateFun, Chs) ->
	NChs = StateFun(Chs),
	{ok, NChs, NChs}.
	
write_debug(Dev, Event, Name) ->
	io:format(Dev, "~p event = ~p~n", [Name, Event]).
```

ch4中同样展示了如何使用sys提供的简单调试函数：

```
1> ch4:start_link().
{ok,<0.30.0>}
2> sys:statistics(ch4, true).
ok
3> sys:trace(ch4, true).
ok
4> ch4:alloc().
ch4 event = {in,alloc,<0.25.0>}
ch4 event = {out,{ch4,ch1},<0.25.0>}
ch1
5> ch4:free(ch1).
ch4 event = {in,{free,ch1}}
ok
6> sys:statistics(ch4, get).
{ok,[{start_time,{{2003,6,13},{9,47,5}}},{current_time,{{2003,6,13},{9,47,56}}},{reductions,109},{messages_in,2},{messages_out,1}]}
7> sys:statistics(ch4, false).
ok
8> sys:trace(ch4, false).
ok
9> sys:get_status(ch4).
{status,<0.30.0>,{module,ch4},[[{'$ancestors',[<0.25.0>]},{'$initial_call',{ch4,init,[<0.25.0>]}}],running,<0.25.0>,[],[ch1,ch2,ch3]]}
```

###### 启动进程

proc_lib中的函数用于启动进程。有几个函数可以使用，例如spawn_link/3,4用于异步启动，start_link/3,4,5用于同步启动。

使用这些函数启动的进程存储关键信息（例如，关于进程祖先和初始调用）对于监控树中的进程是必要的。

如果进程因为normal或shutdown之外的原因终止，会生成crash报告。更多信息查看 *SASL User's Guide* 。

这个例子中，用到了同步调用。进程通过ch4:start_link()启动:

```
start_link() ->
	proc_lib:start_link(ch4, init, [self()]).
```

ch4:start_link调用proc_lib:start_link函数。这个函数需要模块名、进程名和参数，启动并链接一个新进程。新进程通过执行特定函，这里是ch4:init(Pid)，Pid是第一个进程，对于init来说是父进程。

初始化工作，包括注册进程名，会在init中完成。新进程必须知道它是被哪个父进程启动的：

```
init(Parent) ->
	...
	proc_lib:init_ack(Parent, {ok, self()}),
	loop(...).
```

proc_lib:start_link是同步的，直到proc_lib:init_ack调用之后才会返回。

###### 调试

为了支持sys的调试机制，需要用到 *调试结构体* 。Deb元组使用sys:debug_options/1初始化：

```
init(Parent) ->
	...
	Deb = sys:debug_options([]),
	...
	loop(Chs, Parent, Deb).
```

sys:debug_options/1需要选项列表作为参数。这里列表为空，表示最开始调试没有开启。更多可能的选项，查看STDLIB中 *sys(3)* 的man page。

对于每个需要被记录和跟踪的 **system event**，下面的函数被调用:

```
sys:handle_debug(Deb, Func, Info, Event) => Deb1
```

这里：
* Deb是调试结构体
* Func是用于格式化输出信息的函数。对于每个系统事件，格式化函数以Func(Dev, Event, Info)的形式被调用，在这里：
	* Dev是IO设备，用于输出信息。查看 STDLIB 的 *io(3)* man page。
	* Event和Info从handle_debug传入
* Info用于给Func传递更多信息，可以使任何原子。
* Event是系统事件。系统事件的定义和代表含义由用户定义。一般来说输入和输出消息被当做系统消息，以下面的形式{in, Msg[,From]}、{out, Msg, To}。

handle_debug返回更新的调试结构体 Deb1.

这个例子中，handle_debug会在消息发送和到达时被调用。格式化函数Func是ch4:write_debug/3，它简单的调用io:format/3输出调试信息。

```
loop(Chs, Parent, Deb) ->
	receive
		{From, alloc} ->
			Deb2 = sys:handle_debug(Deb, fun ch4:write_debug/3,
			ch4, {in, alloc, From}),
			{Ch, Chs2} = alloc(Chs),
			From ! {ch4, Ch},
			Deb3 = sys:handle_debug(Deb2, fun ch4:write_debug/3, ch4, {out, {ch4, Ch}, From}),
			loop(Chs2, Parent, Deb3);
		{free, Ch} ->
			Deb2 = sys:handle_debug(Deb, fun ch4:write_debug/3,ch4, {in, {free, Ch}}),
			Chs2 = free(Ch, Chs),
			loop(Chs2, Parent, Deb2);
		{system, From, Request} ->
			sys:handle_system_msg(Request, From, Parent,ch4, Deb, Chs)
	end.

write_debug(Dev, Event, Name) ->
	io:format(Dev, "~p event = ~p~n", [Name, Event]).
```


###### 处理系统消息

** 系统消息 ** 以下面的形式接收：

```
{system, From, Request}
```

消息内容和含义不需要被进程理解。相反，下面的函数被调用：

```
sys:handle_system_msg(Request, From, Parent, Module, Deb, State)
```

这个函数不会返回，它处理这个系统消息，如果进程继续执行，会调用下面的函数：

```
Module:system_continue(Parent, Deb, State)
```

或者终止进程：

```
Module:system_continue(Parent, Deb, State)
```

监控树中的进程应该和父进程以同样的原因终止：

* Request 和 From 会被pass下去
* Parent 是父进程的pid
* Module 是模块名
* Deb 是调试结构体
* State 描述内部状态，会发送给system_continue/system_terminate/system_get_state/system_replace_state。

如果进程要获取它的状态，handle_system_msg调用：

```
Module:system_get_state(State)
```

如果进程要用StateFun替换它的状态，handle_system_msg调用：

```
Module:system_replace_state(StateFun, State)
```

这个例子中：

```
loop(Chs, Parent, Deb) ->
	receive
		...
		{system, From, Request} ->
			sys:handle_system_msg(Request, From, Parent,ch4, Deb, Chs)
	end.
	
system_continue(Parent, Deb, Chs) ->
	loop(Chs, Parent, Deb).
	
system_terminate(Reason, _Parent, _Deb, _Chs) ->
	exit(Reason).
	
system_get_state(Chs) ->
	{ok, Chs}.
	
system_replace_state(StateFun, Chs) ->
	NChs = StateFun(Chs),
	{ok, NChs, NChs}.
```

如果特殊进程设置为捕捉退出信号，它的父进程终止，预期的行为是以同样的原因终止：

```
init(...) ->
	...,
	process_flag(trap_exit, true),
	...,
	loop(...).
loop(...) ->
	receive
		...
		{'EXIT', Parent, Reason} ->
			..maybe some cleaning up here..
			exit(Reason);
		...
	end
```

##### 9.7.3 用户自定义行为模式

为了实现用户自定义行为模式，需要编写和之前的特殊进程类似的代码，但是所有的函数都在回调模块中，来处理具体任务。

如果想让编译器提醒模块缺少回调函数，可以类似OTP的行为模式，可以再行为模块中添加-callback属性，指定必须的回调函数：

```
-callback Name1(Arg1_1, Arg1_2, ..., Arg1_N1) -> Res1.
-callback Name2(Arg2_1, Arg2_2, ..., Arg2_N2) -> Res2.
...
-callback NameM(ArgM_1, ArgM_2, ..., ArgM_NM) -> ResM.
```

NameX是回调函数名，ArgX和ResX是类型描述。-callback属性支持完整的-spec属性。

可选的回调函数用-optional_callbacks属性指定：

```
-optional_callbacks([OptName1/OptArity1, ..., OptNameK/OptArityK]).
```

每个OptName/OptArity表明回调函数的名称和元数。注意optional_callbacks属性和-callback属性一起使用，不能联合下面介绍的behaviour_info()。

Behaviour:behaviour_info(optional_callbacks)可以获取所有可选的回调函数。

*注意：我们推荐使用-callback属性而不是behaviour_info()函数。因为额外的类型信息可以用于文档生成工具或排查差异。*

作为-callback和-optional_callbacks属性的替代，可以导出behaviour_info()函数：

```
behaviour_info(callbacks) ->
	[{Name1, Arity1},...,{NameN, ArityN}].
```

这个函数可以用-callback属性自动生成。

当编译器发现模块属性-behaviour(Behaviour)时。在模块Mod中，调用Behaviour:behaviour_info(callbacks)并比较实际导出的函数，如果缺少必要的回调会给出警告信息：

**例子** ：

```
%% User-defined behaviour module
-module(simple_server).
-export([start_link/2, init/3, ...]).
-callback init(State :: term()) -> 'ok'.
-callback handle_req(Req :: term(), State :: term()) -> {'ok', Reply :: term()}.
-callback terminate() -> 'ok'.
-callback format_state(State :: term()) -> term().
-optional_callbacks([format_state/1]).

%% Alternatively you may define:
%%
%% -export([behaviour_info/1]).
%% behaviour_info(callbacks) ->
%%     [{init,1}, {handle_req,2},{terminate,0}].
start_link(Name, Module) ->
	proc_lib:start_link(?MODULE, init, [self(), Name, Module]).
init(Parent, Name, Module) ->
	register(Name, self()),
	...,
	Dbg = sys:debug_options([]),
	proc_lib:init_ack(Parent, {ok, self()}),
	loop(Parent, Module, Deb, ...).
```

在回调模块中：

```
-module(db).
-behaviour(simple_server).
-export([init/1, handle_req/2, terminate/0]).
...
```

行为模块中的-callback属性可以添加-spec重定义。如下：

```
-module(db).
-behaviour(simple_server).
-export([init/1, handle_req/2, terminate/0]).

-record(state, {field1 :: [atom()], field2 :: integer()}).
-type state() 	:: 	#state{}.
-type request() :: 	{'store', term(), term()};
					{'lookup', term()}.
...
-spec handle_req(request(), state()) -> {'ok', term()}.
...
```

每个spec约束是callback约束的子类型。


