+++
title = "9 OTP设计原则——第三部分"
date = "2016-11-21T04:09:05+08:00"
draft = true
description = "OTP Design Priciple"
tags = ["Erlang/OTP", "translation"]
topics = ["Erlang/OTP", "translation"]
+++


#### 9.3 gen_fsm 行为模式

*注意：在新代码中应该使用新行为模式gen_statem替换gen_fsm，gen_statem添加了更丰富的功能。为了确保老机器上的代码正常运行，gen_fsm模块在可见的未来不会被移除。*

这一章节结合*stdlib* man page中的*gen_fsm(3)*阅读，man page中详细介绍了gen_fsm行为模式的接口函数和回调函数。

##### 9.3.1 有限状态机

有限状态机（FSM）可以被描述为如下形式的关系组：

```
State(S) x Event(E) -> Actions(A), State(S')
```

这组关系可以这样理解：

* 如果当前状态是S，并且事件E发生，那么我们将执行行为A然后转移到状态S'。

对于一个gen_fsm行为模式实现的有限状态机，状态转移规则写成一组Erlang转换函数，类似下面的转换：

```
StateName(Event, StateData) ->
	.. code ..
	{next_state, StateName', StateData'}
```

##### 9.3.2 例子

带密码锁的门可以看作一个有限状态机。初始时，门是关着的。任何时候有人摁下数字，产生一个事件。根据之前的按键情况，目前为止产生的序列可能是正确的、不完整的或者错误的。

如果是正确的，门会打开30秒（30,000ms）；如果不完整，就等待后续按键。如果错误，就从头开始，等待新的按键序列。

用gen_fsm实现密码锁有限状态机，我们得到如下回调模块：

```
-module(code_lock).
-behaviour(gen_fsm).
-export([start_link/1]).
-export([button/1]).
-export([init/1, locked/2, open/2]).

start_link(Code) ->
	gen_fsm:start_link({local, code_lock}, code_lock, lists:reverse(Code), []).
button(Digit) ->
	gen_fsm:send_event(code_lock, {button, Digit}).
	
init(Code) ->
	{ok, locked, {[], Code}}.
locked({button, Digit}, {SoFar, Code}) ->
	case [Digit|SoFar] of
		Code ->
			do_unlock(),
			{next_state, open, {[], Code}, 30000};
		Incomplete when length(Incomplete)<length(Code) ->
			{next_state, locked, {Incomplete, Code}};
		_Wrong ->
			{next_state, locked, {[], Code}}
	end.
open(timeout, State) ->
	do_lock(),
	{next_state, locked, State}.
```

下面小节详细介绍这个例子。

##### 9.3.3 启动gen_fsm

上面的例子中，gen_fsm通过调用code_lock:start_link(Code):

```
start_link(Code) ->
	gen_fsm:start_link({local, code_lock}, code_lock, lists:reverse(Code), []).
```

start_link通过调用gen_fsm:start_link/4，创建并链接一个新的进程，我们称实现gen_fsm行为模式的进程为一个gen_fsm进程。

* 第一个参数，{local, code_lock}，进程名，表示在本地注册code_lock；如果省略名字，gen_fsm进程没有注册，因此要用到Pid。name也可以是{global, Name}的形式，这样的话gen_fsm调用global:register_name/2注册。
* 第二个参数，code_lock，回调模块名，表示回调函数所在的模块文件。这个例子中，接口函数（start_link和button）跟回调函数（init locked 和 open）在同一个模块。这是良好的编程习惯，将进程和模块文件关联起来，一个模块文件对应一组进程。
* 第三个参数，Code，是一组数字，反转之后传给init，init初始化时有正确的密码。
* 第四个参数，[]，一组可选选项，*gen_fsm(3)*的man page有详细说明。

如果进程名注册成功，新的gen_fsm进程会调用回调函数code_lock:init(Code)，init应当返回{ok, StateName, StateData}，StateName是gen_fsm的初始状态。这个例子中是*locked*，假设门刚开始处于锁状态。StateData是gen_fsm的内部状态。（对于gen_fsm，内部状态通常称作'状态数据'以此区分状态机的状态）。这个例子中，状态数据是目前为止的按钮序列——开始是空，Code时表示可以状态转换。

```
init(Code) ->
	{ok, locked, {[], Code}}.
```

gen_fsm:start_link是同步调用。gen_fsm进程初始化成功并准备好接收通知时才会返回。

当gen_fsm进程作为监控树的一部分时，gen_fsm:start_link会被监控进程调用以启动进程；作为独立进程时，可以调用gen_fsm:start启动。


##### 9.3.4 事件通知

通过调用gen_fsm:send_event/2，button函数通知代码按键事件的发生：

```
button(Digit) ->
	gen_fsm:send_event(code_lock, {button, Digit}).
```

code_lock是gen_fsm的进程名，必须和启动时注册的名称相同。{button, Digit}是实际事件。

这个事件会产生一条消息，并发送给gen_fsm框架。当事件到达时，gen_fsm框架调用StateName(Event, StateData)，并期待返回{next_state, StateName1, StateData1}。StateName是当前状态，StateName1是下一个状态。StateData1是gen_fsm的新状态数据。

```
locked({button, Digit}, {SoFar, Code}) ->
	case [Digit|SoFar] of
		Code ->
			do_unlock(),
			{next_state, open, {[], Code}, 30000};
		Incomplete when length(Incomplete) < length(Code) ->
			{next_state, locked, {Incomplete, Code}};
		_Wrong ->
			{next_state, locked, {[], Code}};
	end.
```

如果门处于锁状态，按钮按下，完整的按钮序列会和正确密码比较，根据比较结果，门会打开——导致gen_fsm状态变成open，或者维持locked状态。

##### 9.3.5 超时

正确的按钮序列会打开门，此时locked/2返回的元组如下：

```
{next_state, open, {[], Code}, 30000};
```

30,000是以毫秒为单位的超时值。过了超时时长，即30秒，超时时间发生。StateName(timeout, StateData)被调用。这个超时发生在门被打开30秒后，随后门被再次上锁。

```
open(timeout, State) ->
	do_lock(),
	{next_state, locked, State}.
```

##### 9.3.5 All State Events(怎么翻译？通杀事件？)

有时候一个事件需要发送给处于所有状态的gen_fsm。可以用gen_fsm:send_event/2，然后给每一个事件处理都加一条模式匹配，但是更好的办法是调用gen_fsm:send_all_state_event/2来发送消息，Module:handle_event/3来处理消息：

```
-module(code_lock).
...

-export([stop/0]).
...
stop() ->
	gen_fsm:send_all_state_event(code_lock, stop).

...
handle_event(stop, _StateName, StateData) ->
	{stop, normal, StateData}.
```

##### 9.3.6 停止

###### 在监控树中

如果gen_fsm进程是监控树的一部分，不需要提供专门的stop函数。gen_fsm进程会被它的监控进程supervisor自动终止。具体如何终止通过在监控进程中设置*shutdown strategy*实现。

如果在进程退出之前需要清理资源，那么终止策略必须是一个超时值，而且gen_fsm进程要在init中设置为系统进程——通过捕捉exit信号。这样当终止时，gen_fsm框架会回调terminate(shutdown, StateName, StateData)函数:

```
init(Args) ->
	...,
	process_flag(trap_exit, true),
	...,
	{ok, StateName, StateData}.
...
terminate(shutdown, StateName, StateData) ->
	..code for cleaning up here..
	ok.
```

###### 独立gen_fsm进程

如果gen_fsm不在监控树中，要用到stop停止函数。例子：

```
...
-export([stop/0]).
...
stop() ->
	gen_fsm:send_all_state_event(code_lock, stop).
...
handle_event(stop, _StateName, StateData) ->
	{stop, normal, StateData}.
...
terminate(normal, _StateName, _StateData) ->
	ok.
```

handle_event回调函数处理stop事件并返回{stop, normal, StateData1}元组，normal表示正常退出，StateData1是gen_fsm进程的新状态数据。随后gen_fsm调用terminate(normal, StateName, StateData1)，进程按预期终止。

##### 9.3.7 其他消息处理

如果gen_fsm收到其他消息，必须实现回调函数handle_info(Info, StateName, StateData1)用来处理他们。其他消息包括退出信号，当gen_fsm进程链接其他进程并捕捉exit信号时。

```
handle_info({'EXIT', Pid, Reason}, StateName, StateData) ->
	..code to handle exits here..
	{next_state, StateName1, StateData1}.
```

此外，回调函数code_change必须实现。

```
code_change(OldVsn, StateName, StateData, Extra) ->
	..code to convert state (and more) during code change..
	{ok, NextStateName, NewStateData}
```
