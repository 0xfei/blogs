+++
title = "9 OTP设计原则——第一部分"
date = "2016-11-19T02:06:05+08:00"
draft = true
description = "OTP Design Priciple"
categories = ["Erlang/OTP"]
tags = ["Erlang/OTP", "翻译文章"]
topics = ["Erlang/OTP"]
+++


#### 9.1 概述

OTP设计原则用于指导以进程、模块和目录的形式组织Erlang代码的规范。

##### 9.1.1 Supervision树——进程管理树（监控树）

Erlang/OTP最基本的概念是监控树，由workers进程和supervisors进程组成。workers是真正执行任务的进程，我们在这里实现自己的功能；supervisors是监控进程，用于监视worker进程的行为，当worker进程异常时可以重启它。通过这种设计，worker进程可以专注工作，而supervisor监控进程尽量少的做事——它只负责监视自己负责的进程，可能是worker进程，也可能是次级监控进程。监控树按照层级组织代码，由顶层监控进程、次级监控进程和工作进程组成，很容易设计高容错系统。

##### 9.1.2 行为模式（behaviours）

在监控树中，许多进程有类似的结构和模式。例如，supervisors进程结构上类似，区别就是监控的子进程不一样。workers进程几乎都是客户端-服务端架构、有限状态机模型，以及拥有类似的事件处理机制，比如异常日志。

<!--more-->

行为模式是这些代码共同部分的具体化，即把类似的代码提取出来，当做行为模块，用户（程序员）只要提供回调，就能实现一套模板、多个实现。

下面的例子展示了如何把代码分成通用模块和具体实现两部分。把它当做一个简单服务器，它跟踪channels通道的数量，其它进程可以分配和释放channel通过alloc/0和free/1。


```
%% ch1.erl
-module(ch1).
-export([start/0]).
-export([alloc/0, free/1]).
-export([init/0]).

start() ->
	spawn(ch1, init, []).

alloc() ->
	ch1 ! {self(), alloc},
	receive
		{ch1, Res} ->
			Res
	end.

free(Ch) ->
	ch1 ! {free, Ch},
	ok.

init() ->
	register(ch1, self()),
	Chs = channels(),
	loop(Chs).

loop(Chs) ->
	receive
		{From, alloc} ->
			{Ch, Chs2} = alloc(Chs),
			From ! {ch1, Ch},
			loop(Chs2);
		{free, Ch} ->
			Chs2 = free(Ch, Chs),
			loop(Chs2)
	end.
```

这个程序分成通用模板和具体实现之后如下：

```
%% server.erl
-module(server).
-export([start/1]).
-export([call/2, cast/2]).
-export([init/1]).

start(Mod) ->
	spawn(server, init, [Mod]).
	
call(Name, Req) ->
	Name ! {call, self(), Req},
	receive
		{Name, Res} ->
			Res
	end.
	
cast(Name, Req) ->
	Name ! {cast, Req},
ok.

init(Mod) ->
	register(Mod, self()),
	State = Mod:init(),
	loop(Mod, State).
	
loop(Mod, State) ->
	receive
		{call, From, Req} ->
			{Res, State2} = Mod:handle_call(Req, State),
			From ! {Mod, Res},
			loop(Mod, State2);
		{cast, Req} ->
			State2 = Mod:handle_cast(Req, State),
			loop(Mod, State2)
	end.
```

可以看到上面的模板提供回调接口，下面的代码是具体实现。

```
%% ch2.erl
-module(ch2).
-export([start/0]).
-export([alloc/0, free/1]).
-export([init/0, handle_call/2, handle_cast/2]).

start() ->
	server:start(ch2).
alloc() ->
	server:call(ch2, alloc).
free(Ch) ->
	server:cast(ch2, {free, Ch}).
init() ->
	channels().
handle_call(alloc, Chs) ->
	alloc(Chs). % => {Ch,Chs2}
handle_cast({free, Ch}, Chs) ->
	free(Ch, Chs). % => Chs2
```

通过拆分代码，不仅实现了功能，还提供了下面的便利：

* server.erl可以被不同功能的服务器复用，只要我们提供合适的回调函数
* server名称ch2对客户端函数的用户隐藏了，这意味着我们可以随便修改服务名
* 消息发送和接收也做了封装。这是很好的编程习惯，我们可以修改内部协议，而不用变动使用这些接口的功能代码
* server模块的功能可以随意扩充，增加功能提高程序稳定性，而ch2，即实现模块没必要关心这些

为了例子的完整性，下面是channels\0 alloc\1 和 free\2的简单实现，现实中的例子往往要更复杂，需要考虑更多——例如资源耗尽等。

```
channels() ->
	{_Allocated = [], _Free = lists:seq(1,100)}.

alloc({Allocated, [H|T] = _Free}) ->
	{H, {[H|Allocated], T}}.
free(Ch, {Alloc, Free} = Channels) ->
	case lists:member(Ch, Alloc) of
		true ->
			{lists:delete(Ch, Alloc), [Ch|Free]};
		false ->
			Channels
	end.
```

不使用行为模式的代码可能更高效，但是这点效率的提高需要在其他方面付出极高的代价。能以统一地（in a consistent manner）管理系统中所有应用是很重要的。

利用行为模式编写的代码更易读，也更容易被其它人理解。简单随意的编码风格或许更高效，但是往往更难理解。

上面的server模块在某种程度上，和Erlang/OTP的行为模式gen_server相对应。

标准的Erlang/OTP行为模式有如下几种：

* gen_server
	客户端-服务端架构的通用服务器实现
	
* gen_fsm
	老版本中实现有限状态机
	
* gen_statem
	新版本中的有限状态机实现
	
* gen_event
	实现事件处理功能
	
* supervisor
	实现监控树结构中的监控进程
	
Erlang编译器能理解 -behaviour(Behaviour) 模块属性，并在没有实现所有回调函数时产生警告。下面是一个例子：

```
-module(chs3).
-behaviour(gen_server).
...

3> c(chs3).
./chs3.erl:10: Warning: undefined call-back function handle_call/3
{ok,chs3}
```

##### 9.1.3 应用（Applications）

Erlang/OTP附带很多组件，每个都有特定的功能。这些组件在Erlang/OTP中被称为“应用”。Mnesia就是Erlang/OTP的一个应用，它提供了数据库服务所需要的所有内容；还有Debugger应用，用于调试Erlang程序。每个Erlang/OTP系统都包含两个基本应用：

* Kernel - 提供运行Erlang所需的基本功能
* STDLIB - Erlang标准库

应用的理念与程序（进程组织）和目录（模块组织）密切相关。

最简单的应用没有任何进程，但是由实现了特定功能的模块组成，这样的应用被称为库应用，库应用的例子有STDLIB。

创建进程的应用很容易利用标准行为模式实现监控树模型。

如何编写应用会在 *Applications* 章节提供。

##### 9.1.4 发行（Releases）

一个版本发行（release）是由Erlang/OTP应用子集和一组用户应用组成的完整系统。 *Releases* 章节详细介绍。

##### 9.1.5 版本控制（Release Handling）

版本控制是指对在一个系统中对某应用升级或者降低版本。 *Release Handling* 章节详细介绍这部分内容。




#### 9.2 gen_server 行为模式

这一章节结合 *stdlib* man page中的 *gen_server(3)* 阅读，man page中详细介绍了gen_server行为模式的接口函数和回调函数。

##### 9.2.1 C/S架构

C/S架构有一个中心服务器以及任意数量的客户端，客户端-服务器模型在资源管理操作中很常见，不同的客户共享同一个资源。服务器用于资源管理。

##### 9.2.2 一个例子

在前一节 *Overview* 中展示的纯Erlang编写的服务器可以用gen_server行为模式实现，下面的ch3.erl实现了回调模块：

```
-module(ch3).
-behaviour(gen_server).
-export([start_link/0]).
-export([alloc/0, free/1]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
	gen_server:start_link({local, ch3}, ch3, [], []).

alloc() ->
	gen_server:call(ch3, alloc).
free(Ch) ->
	gen_server:cast(ch3, {free, Ch}).

init(_Args) ->
	{ok, channels()}.
handle_call(alloc, _From, Chs) ->
	{Ch, Chs2} = alloc(Chs),
	{reply, Ch, Chs2}.
handle_cast({free, Ch}, Chs) ->
	Chs2 = free(Ch, Chs),
	{noreply, Chs2}.
```

下面小节详细介绍这个例子。

##### 9.2.3 启动gen_server

前面的例子，gen_server通过调用ch3:start_link启动:

```
start_link() ->
	gen_server:start_link({local, ch3}, ch3, [], []) => {ok, Pid}.
```

start_link调用gen_server:start_link/4，它创建并链接一个新的进程，我们称实现gen_server行为模式的进程为一个gen_server进程。（在下面的翻译中，gen_server有时候指的是自己编写的符合gen_server规范的进程，有时候指gen_server框架，即背后的模块，还有时候指gen_server接口。《Erlang/OTP并发编程实践》第三章详细的解释了这三个很容易混淆的名词。）

* 第一个参数，{local, ch3}，进程名，表示在本地注册ch3；如果省略名字，gen_server不会注册，因此要用到Pid。name也可以是{global, Name}的形式，这样的话gen_server调用global:register_name/2注册。
* 第二个参数，ch3，回调模块名，表示回调函数所在的模块文件。这个例子中，接口函数（start_link alloc和free）跟回调函数（init handle_call 和 handle_cast）在同一个模块。这是良好的编程习惯，将进程和模块文件关联起来，一个模块文件对应一组进程。
* 第三个参数，[]，是gen_server模式回调init是传入的参数；这里的init不需要参数，因此传入空列表。
* 第四个参数，[]，是gen_server的可选参数， *gen_server(3)* 的man page有详细说明。

如果进程名注册成功，gen_server会调用ch3:init([])，init应当返回{ok, State}，State是gen_server进程的内部状态，活跃在整个生命周期。这个例子中是有效的channel。

```
init(_Args) ->
	{ok, channels()}.
```

gen_server:start_link是同步调用，直到gen_server初始化成功可以接收请求才会返回。

当gen_server作为监控树的一部分时，gen_server:start_link必须被一个监控进程调用。如果这个gen_server不属于任何监控树，可以调用gen_server:start，用于启动单独的gen_server。

##### 9.2.4 同步请求 - Call

本例中的alloc()函数通过调用gen_server:call/2实现同步请求。

```
alloc() ->
	gen_server:call(ch3, alloc).
```

ch3是gen_server进程的名字，必须和启动时注册的名字相同。alloc代表请求。

这个请求会生成一条消息，发送给gen_server。当请求到达时，gen_server调用handle_call(Request, From, State)，并期待调用返回{reply, Reply, State1}。Reply是客户端的答复信息，State1是gen_server进程的新状态。

```
handle_call(alloc, _From, Chs) ->
	{Ch, Chs2} = alloc(Chs),
	{reply, Ch, Chs2}.
```

这个例子中，应答是分配好的channel Ch和gen_server进程新状态，即剩余的可用channels集合Chs2.

至此，ch3:alloc()返回分配好的channel，gen_server进程更新自己的state，并接着等待处理请求。


##### 9.2.5 异步请求 - Cast

free(Ch)调用gen_server:cast/2实现异步请求。

```
free(Ch) ->
	gen_server:cast(ch3, {free, Ch}).
```

ch3是gen_server进程的注册名，{free, Ch}是具体请求。

这条请求生成消息并发送给gen_server，释放资源，最后返回ok。

当请求到达时，gen_server调用handle_cast(Request, State)，并期望返回{noreply, State1}。State1表示gen_server进程的新状态。

```
handle_cast({free,Ch}, Chs) ->
	Chs2 = free(Ch, Chs),
	{noreply, Chs2}.
```

##### 9.2.6 停止

###### gen_server监控树中

如果gen_server进程是监控树的一部分，不需要提供专门的stop函数。会被监控进程supervisor自动终止。终止策略通过在监控进程中设置 *shutdown strategy* 实现。（补充：后面的supervisor章节会有更详细的说明）

如果在进程退出之前需要清理资源，那么终止策略必须是一个超时值，而且gen_server进程要在init中设置为系统进程——通过捕捉exit信号。这样当终止时，gen_server会回调terminate(shutdown, State)函数。

```
init(Args) ->
	...,
	process_trap(trap_exit, true),
	...,
	{ok, State}.
	
...

terminate(shutdown, State) ->
	.. clean up ..,
	ok.
```

###### 独立gen_server进程

如果gen_server不在监控树中，要用到stop停止函数。例子：

```
...
export([stop/0]).
...

stop() ->
	gen_server:cast(ch3, stop).

...

handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast({free, Ch}, State) ->
	...

...

terminate(normal, State) ->
	ok.
```

cast回调函数处理stop请求，并返回{stop, normal, State1}，normal表示正常退出，State1是新的进程状态。之后gen_server调用terminate(normal, State1)，进程顺利终止。

##### 9.2.7 其他消息处理

如果gen_server收到其他消息——handle_call和handle_cast之外的，回调函数handle_info(Info, State)用来处理他们。其他消息包括退出信号，当gen_server进程链接到其他进程并捕捉exit信号时。

```
handle_info({'EXIT', Pid, Reason}, State) ->
	.. code ..
	{noreply, State1}.
```

此外，回调函数code_change必须实现，用于版本管理。

```
code_change(OldVsn, State, Extra) ->
	..code..
	{ok, NewState}
```


#### 9.3 gen_fsm 行为模式

*注意：在新代码中应该使用新行为模式gen_statem替换gen_fsm，gen_statem添加了更丰富的功能。为了确保老机器上的代码正常运行，gen_fsm模块在可见的未来不会被移除。*

这一章节结合 *stdlib* man page中的 *gen_fsm(3)* 阅读，man page中详细介绍了gen_fsm行为模式的接口函数和回调函数。

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
* 第四个参数，[]，一组可选选项， *gen_fsm(3)* 的man page有详细说明。

如果进程名注册成功，新的gen_fsm进程会调用回调函数code_lock:init(Code)，init应当返回{ok, StateName, StateData}，StateName是gen_fsm的初始状态。这个例子中是 *locked* ，假设门刚开始处于锁状态。StateData是gen_fsm的内部状态。（对于gen_fsm，内部状态通常称作'状态数据'以此区分状态机的状态）。这个例子中，状态数据是目前为止的按钮序列——开始是空，Code时表示可以状态转换。

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

##### 9.3.6 All State Events(怎么翻译？通杀事件？)

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

##### 9.3.7 停止

###### 在监控树中

如果gen_fsm进程是监控树的一部分，不需要提供专门的stop函数。gen_fsm进程会被它的监控进程supervisor自动终止。具体如何终止通过在监控进程中设置 *shutdown strategy* 实现。

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

##### 9.3.8 其他消息处理

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
