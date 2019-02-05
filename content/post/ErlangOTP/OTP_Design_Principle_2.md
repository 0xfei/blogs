+++
title = "9 OTP设计原则——第二部分"
date = "2016-11-20T04:14:10+08:00"
draft = true
description = "OTP Design Priciple"
categories = ["Erlang/OTP"]
tags = ["Erlang/OTP", "翻译文章"]
topics = ["Erlang/OTP"]
+++


#### 9.4 gen_statem 行为模式

这一章节结合*stdlib* man page中的gen_statem(3)阅读，其中详细介绍了gen_statem行为模式的接口函数和回调函数。

*注意：这是Erlang/OTP 19.0 开始提供的新行为模式，它已经经过严格的检验和慎重思考，并且稳定的运行在至少两个重量级OTP应用。根据用户反馈，可能会在Erlang/OTP 20.0中做一些不向下兼容的改动，我们希望不会如此。*

##### 9.4.1 事件驱动的状态机

现有公认的有效状态机理论并没有涉及状态转移如何触发，我们假设输出是由输入和状态函数运算，并产生一些值。

对于事件驱动的状态机，输入是一个触发状态转移的事件，输出是状态转移过程中执行的动作。有限状态机的数学模型和下面的关系集合类似：

```
State(S) x Event(E) -> Actions(A), State(S')
```

这组关系可以这样理解：

<!--more-->


* 如果当前状态是S，并且事件E发生，那么我们将执行行为A然后转移到状态S'，注意S'可以和S相等。

由于A和S'仅取决于S和E，这种类型的状态机可以称之为 *米利型有限状态机* （可以查看维基百科相关页面）。

类似大多数gen_行为模式，除了状态，gen_statem进程内部维护一个服务器数据。状态和输入数据的数目没有限制（假设有足够的虚拟机内存），因此这种行为模式的状态机实际上是图灵完全的。实际上更像是事件驱动的米利型有限状态机。

##### 9.4.2 回调模型

gen_statem行为模式支持两种回调模型：

* state_functions模型，每个状态转移都用单独Erlang函数实现，产生如下转换:

```
StateName(EventType, EventContent, Data) ->
    .. code for actions here ...
    {next_state, NewStateName, NewData}.
```

* handle_event_function模型，仅用一个函数实现所有状态转移规则：

```
handle_event(EventType, EventContent, State, Data) ->
    .. code for actions here ...
    {next_state, NewState, NewData}
```

两种模型都可以返回其它元组，查看 *gen_statem* 的man page页面 *Module:StateName/3* 相关内容。其它返回值能产生不同效果，例如，停止状态机、状态机执行状态转移行为以及发送响应等。

##### 回调模式的选择

两种回调模式有不同的功能和限制，但是目的相同：用于处理各种可能的事件和状态。

我们可以每次集中于一种状态，并且每种状态都确保能处理所有事件。也可以每次集中处理一种事件，并确保它可以在每种状态下得到处理。当然，这两种策略可以混合使用。

使用state_functions模型，你只能使用原子状态，gen_statem框架调用分支取决于你提供的状态名。这种模式鼓励回调模块中针对一种状态的所有事件处理代码放在一起，每次集中处理一种状态。

当状态流有规律时，这种模型很好的解决问题。例如本章的例子，每种状态的所有的事件和行为都围绕着该状态，每种状态都有自己的名字。

使用handle_event_function模型，可以随意处理各种状态，所有的事件和状态都在一个函数中处理。

当你希望一次集中处理一个事件或一种状态时，这种模型同样可以很好的解决问题，只是函数 *Module:handle_event/4* 会变得非常长，如果没有分支处理函数。

这种模型可以使用非原子状态，例如，复杂的状态甚至分层级状态。如果状态流类似客户端-服务端交互的协议，我们会有状态{StateName, server}和{StateName, client}，根据StateName确定如何处理当前状态的事件。元组的第二个元素用于区分客户端事件还是服务端事件。

##### 9.4.3 例子

这个例子开头和gen_fsm那一节的例子相同。之后的小节，增加的代码会用到gen_statem特有的功能。这章末尾的例子会给出增加了所有额外特性的完整代码。

一个有密码锁的的门可以看作一个状态机。初识时，门处于锁状态。当有人摁下按钮，一个事件产生。结合之前按下的按钮序列，当前序列可能正好等于密码、不完整或者错误。如果密码正确，门会打开10秒（10,000毫秒）。如果不完整，我们等待另一个按钮事件；如果错误，就从头开始，等新的按钮序列。

这个密码门状态机可以用gen_statem编写，下面是回调模块代码：

```
-module(code_lock).
-behaviour(gen_statem).
-define(NAME, code_lock).
-export([start_link/1]).
-export([button/1]).

-export([init/1,callback_mode/0,terminate/3,code_change/4]).
-export([locked/3,open/3]).

start_link(Code) ->
    gen_statem:start_link({local,?NAME}, ?MODULE, Code, []).
button(Digit) ->
    gen_statem:cast(?NAME, {button,Digit}).

init(Code) ->
    do_lock(),
    Data = #{code => Code, remaining => Code},
    {ok,locked,Data}.

callback_mode() ->
    state_functions.

locked(cast, {button,Digit},#{code := Code, remaining := Remaining} = Data) ->
    case Remaining of
        [Digit] -> 
            do_unlock(),
            {next_state,open,Data#{remaining := Code},10000};
        [Digit|Rest] -> % Incomplete
            {next_state,locked,Data#{remaining := Rest}};
        _Wrong ->
            {next_state,locked,Data#{remaining := Code}}
    end.
open(timeout, _,  Data) ->
    do_lock(),
    {next_state,locked,Data};
open(cast, {button,_}, Data) ->
    do_lock(),
    {next_state,locked,Data}.
do_lock() ->
    io:format("Lock~n", []).
do_unlock() ->
    io:format("Unlock~n", []).

terminate(_Reason, State, _Data) ->
    State =/= locked andalso do_lock(),
    ok.
code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.
```

这段代码会在下面小节介绍。

##### 9.4.4 启动gen_statem进程

前一节的代码中，gen_statem通过调用code_lock:start_link(Code)启动:

```
start_link(Code) ->
	gen_statem:start_link({local, ?NAME}, ?MODULE, Code, []).
```

start_link通过调用gen_statem:start_link/4，创建并链接一个新的gen_statem进程。

* 第一个参数，{local, ?NAME}，进程名，这个例子中gen_statem展开?NAME宏并注册code_lock；如果省略名字，gen_statem进程不会注册，因此要用到Pid。name也可以是{global, Name}的形式，这样的话gen_statem调用kernel的global:register_name/2注册。
* 第二个参数，?MODULE，回调模块名，表示回调函数所在的模块文件。这个例子中，接口函数（start_link/1和button/1）跟回调函数（init/1,locked/3和open/3）在同一个模块。这是良好的编程习惯——将进程和模块文件关联起来，一个模块文件对应一组进程。
* 第三个参数，Code，是一组数字，反转之后传给init，init初始化时有正确的密码。
* 第四个参数，[]，一组可选选项， *gen_statem:start_link/3* 的man page有详细说明。

如果进程名注册成功，新的gen_statem进程会调用回调函数code_lock:init(Code)，init应当返回{ok, State, Data}，State是gen_statem进程的初始状态。这个例子中是 *locked* ，假设门刚开始处于锁状态。Data是gen_statem的内部状态。这个例子中，状态数据是一个map，code存储正确的按钮序列，remaining存储剩余所需的正确序列（开始时是完整密码Code）。

```
init(Code) ->
    do_lock(),
    Data = #{code => Code, remaining => Code},
    {ok,locked,Data}.
```

gen_statem:start_link是同步调用。gen_statem进程初始化成功并准备好接收通知时才会返回。

当gen_statem进程作为监控树的一部分时，gen_state:start_link会被监控进程调用以启动进程；作为独立进程时，可以调用gen_fsm:start启动。

函数 *Module:callback_mode/0* 返回模块使用的回调模式，这个例子中是state_functions，每种状态有自己的处理函数。

```
callback_mode() ->
    state_functions.
```


##### 9.4.5 事件处理

通过调用gen_statem:cast/2，button函数通知代码按键事件的发生：

```
button(Digit) ->
	gen_statem:cast(?NAME, {button, Digit}).
```

?NAME是gen_fsm的进程名，必须和启动时注册的名称相同。{button, Digit}是实际事件。

这个事件会产生一条消息，并发送给gen_statem框架。当事件到达时，gen_statem框架调用StateName(cast, Event, Data)，并期待返回{next_state, NewStateName, NewData}。StateName是当前状态，NewStateName是下一个状态。NewData是gen_statem的新状态数据。

```
locked(cast, {button,Digit},#{code := Code, remaining := Remaining} = Data) ->
    case Remaining of
        [Digit] -> 
            do_unlock(),
            {next_state,open,Data#{remaining := Code},10000};
        [Digit|Rest] -> % Incomplete
            {next_state,locked,Data#{remaining := Rest}};
        _Wrong ->
            {next_state,locked,Data#{remaining := Code}}
    end.
open(timeout, _, Data) ->
    do_lock(),
    {next_state,locked,Data};
open(cast, {button,_}, Data) ->
    do_lock(),
    {next_state,locked,Data}.
```

如果门处于锁状态，按钮按下，完整的按钮序列会和正确密码比较，根据比较结果，门会打开——导致gen_statem状态变成open，或者维持locked状态。

如果密码错误，服务器数据回到开始状态；如果处于打开状态，任何按钮都会上锁，事件会取消事件定时器，因此超时事件不会在按钮事件发生后产生。

##### 9.4.6 超时事件

正确的按钮序列会打开门，此时locked/2返回的元组如下：

```
{next_state, open, Data#{remaining := Code}, 10000};
```

10,000是以毫秒为单位的超时值。过了超时时长，即10秒，超时时间发生。StateName(timeout, 10000, StateData)被调用。这个超时发生在门被打开10秒后，随后门被再次上锁。

```
open(timeout, _, Data) ->
	do_lock(),
	{next_state, locked, Data}.
```

##### 9.4.7 All State Events(怎么翻译？通杀事件？)

有时候一个事件需要发送给处于所有状态的gen_statem。通用状态处理函数可以很方便的处理各种状态对应的事件处理函数不关心的事件。

假设 *code_length/0* 函数返回正确密码的长度(that should not be sensitive to reveal)。我们用handle_event/3函数处理没有明确状态的事件。

```
...
-export([button/1,code_length/0]).
...

code_length() ->
    gen_statem:call(?NAME, code_length).

...

locked(...) -> ... ;

locked(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).
...

open(...) -> ... ;
open(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

handle_event({call,From}, code_length, #{code := Code} = Data) ->
    {keep_state,Data,[{reply,From,length(Code)}]}.
```

这个例子调用gen_statem:call/2，并等待服务器响应。这个响应返回{keep_state, ... }元组，有以{reply, From, Reply}元组组成的响应列表。

##### 9.4.8 一个事件处理函数

如果使用handle_event_function模式，所有事件都在 *Module:handle_event/4* 中处理，我们可以（不一定非得）以事件为中心，类似我们刚开始以事件和状态分支：

```
...
-export([handle_event/4]).
...

callback_mode() ->
    handle_event_function.

handle_event(cast, {button,Digit}, State, #{code := Code} = Data) ->
    case State of
    	locked ->
    		case maps:get(remaining, Data) of
    			[Digit] -> % Complete
    				do_unlock(),
    				{next_state,open,Data#{remaining := Code},10000};
    			[Digit|Rest] -> % Incomplete
    				{keep_state,Data#{remaining := Rest}};
    			[_|_] -> % Wrong
    				{keep_state,Data#{remaining := Code}}
    		end;
    	open ->
     		do_lock(),
     		{next_state,locked,Data}
    	end;
    	
handle_event(timeout, _, open, Data) ->
    do_lock(),
    {next_state,locked,Data}.
...
```

##### 9.4.9 停止

###### 在监控树中

如果gen_statem进程是监控树的一部分，不需要提供专门的stop函数。gen_statem进程会被它的监控进程supervisor自动终止。具体如何终止通过在监控进程中设置 *shutdown strategy* 实现。

如果在进程退出之前需要清理资源，那么终止策略必须是一个超时值，而且gen_statem进程要在init中设置为系统进程——通过捕捉exit信号，调用 *process_flag(trap_exit, true)* 。这样当终止时，gen_statem框架会回调terminate(shutdown, State, Data)函数:

```
init(Args) ->
    process_flag(trap_exit, true),
    do_lock(),
	 ...
```

这个例子中，函数 *terminate/3* 会在open状态时给门上锁，这样当监控树终止时门不会保持打开状态：

```
terminate(_Reason, State, _Data) ->
    State =/= locked andalso do_lock(),
    ok.
```

###### 独立gen_fsm进程

如果gen_statem不在监控树中，要用到gen_statem:stop停止函数，这是一个系统API。例子：

```
...
-export([start_link/1,stop/0]).
... stop() ->
    gen_statem:stop(?NAME).
```

gen_statem:stop会让gen_statem框架回调函数terminate/3，类似监控服务器等待进程退出。

##### 9.4.10 Actions行为

第一节我们提过行为作为通用状态机模型的一部分。返回gen_statem引擎之前，回调模块gen_statem在事件处理回调函数中执行的代码实现这些通用的行为。

回调函数可以让gen_statem引擎处理更多明确的状态转移行为在函数返回之后。这些指令通过在回调函数中返回actions列表实现。这些状态转移会影响gen_statem状态机本身，还可以产生下面的效果：
* 延迟处理当前事件
* 让gen_statem休眠
* 事件超时
* 响应调用者
* 产生下一个需要处理的事件

之前的例子中提到过事件超时和响应。延迟处理事件的例子会在本章后面提及。更多的细节可以查看 *gen_statem* 的man page。我们可以返回给多个调用者，或者产生多个接下来要处理的事件。

##### 9.4.11 事件类型

之前的章节简单提及 *事件类型(event types)* 。同样类型的事件会在相同的回调函数中处理，一个给定的状态，回调函数通过参数得到EventType和EventContent——事件类型和事件内容。

下面是一个完整的事件类型列表，以及它们的来源：

* cast				由gen_statem:cast产生
* {call, From}	由gen_statem:call产生，From是需要响应的地址，通过状态转移行为{reply, From, Msg}或者调用 *gen_statem:reply* .
* info				由常规进程产生，通过发送消息给gen_statem进程
* timeout			超时事件由状态转移行为{timeout, Time, EventContent}产生
* internal		由状态转移行为{next_event, internal, EventContent}产生。上面提到的所有事件类型都可以用{next_event, EventType, EventContent}产生。

##### 9.4.12 超时状态

由状态转移行为{timeout, Time, EventContent}产生的超时事件是一个事件超时，当事件到达时这个“定时器”会被取消。你只能得到一个事件或者一个超时通知，不可能同时得到。

如果你不希望一个定时器被任何事件取消或者在某个状态你希望启动一个定时器并且在另一个状态响应超时。可以用常规的Erlang定时器 *erlang:start_timer* 实现。

本章的例子中，用gen_statem事件定时器有这样的不足：处于open状态时，当按钮事件产生，超时事件会被取消，按钮事件发送。这样的话门会上锁。

假设我们不希望按钮事件把门重新锁上，除了在open状态时忽略按钮事件，还可以在进入open状态时启动一个定时器，然后等待它超时，在此之前忽略按钮事件：

```
... 
locked(cast, {button,Digit},#{code := Code, remaining := Remaining} = Data) ->
    case Remaining of
        [Digit] -> 
            do_unlock(),
        	  Tref = erlang:start_timer(10000, self(), lock),
        	  {next_state,open,Data#{remaining := Code, timer := Tref}};
...
open(info, {timeout,Tref,lock}, #{timer := Tref} = Data) ->
    do_lock(),
    {next_state,locked,Data};
open(cast, {button,_}, Data) ->
	 {keep_state,Data};
...
```

如果你想因为其他事件取消定时器，可以调用 *erlang:cancel_timer(Tref)* 。需要注意一个取消定时器之后超时事件不会发送成功，除非之前使用延迟处理（下一节介绍），因此确保你不会意外的延迟处理这些消息。

另一种取消定时器的方法是不取消它，只是在某个状态当超时到达时忽略它。

##### 延迟处理事件

如果你想在当前状态忽略某个事件，但是又想在以后的某个状态处理它，需要延迟处理这个事件。状态改变时一个延迟处理事件会被重试，即当OldState =/= NewState时。

延迟处理可以用状态延迟行为 *postpone* 设定。

在这个例子中，open状态时我们不忽略按钮事件，而是延迟处理它们，这样这些事件会被放入某个队列，在之后的locked状态时得到处理。

```
...
open(cast, {button, _}, Data) ->
	{keep_state, Data, [postpone]};
...
```

事实上一个延迟事件只会在状态改变为特定的事件和状态之后才会被重试。选择在服务器状态信息或者数据信息里存储状态数据时需要注意——如果这份信息的改变会影响到事件被处理的流程，那么它应该存储到服务器状态信息里，而不是数据信息。

你希望避免这种情况：可能很久之后才决定要延迟处理一个事件，而且它不会被重试，那么代码应该只改变服务器数据信息而不是状态信息。

###### 模糊状态图

状态图中没有具体说明如何处理这个图中那些在某个状态没有详细描述的事件是很常见的，幸运的是这些信息可能会在相关的文档或者上下文中得出。

可能采取的动作：忽略或者丢掉这个信息，或者延迟处理它，让后续的状态决定如何处理。

###### 有选择的接收

Erlang的选择性消息接收让我们能直接用Erlang代码来描述简单状态机。下面是一个简单例子：

```
-module(code_lock).
-define(NAME, code_lock_1).
-export([start_link/1,button/1]).

start_link(Code) ->
    spawn(fun () -> true = register(?NAME, self()), do_lock(),locked(Code, Code) end).

button(Digit) ->
    ?NAME ! {button,Digit}.

locked(Code, [Digit|Remaining]) ->
    receive
    	 {button,Digit} when Remaining =:= [] ->
    	     do_unlock(),
    	     open(Code);
    	 {button,Digit} ->
    	     locked(Code, Remaining);
    	 {button,_} ->
    	     locked(Code, Code)
    end.

open(Code) ->
    receive
    after 10000 ->
       do_lock(),
       locked(Code, Code)
    end.

do_lock() ->
    io:format("Locked~n", []).
do_unlock() ->
    io:format("Open~n", []).
```

这个例子中的选择性接受消息实际上实现了open到locked状态的延迟处理。

选择性接受不能用于gen_statem行为模式，也不能用于gen_行为模式，因为消息接收语句是在gen_引擎内部。因为所有sys兼容的行为模式必须响应系统消息，需要在内部处理消息循环，把非系统消息发送给回调模块。

状态转移行为 *postpone* 被设计为可以调整选择接收方式。一个选择性接收进程隐式延迟任何不接收的事件，但是 *postpone* 状态转移行为显式地延迟一个接收到的事件。

两种机制有同样的时间和空间复杂度，然而选择接收方式有更小的常数因子。

##### 自产事件(Self-Generated Events)

有时候自己产生事件给状态机是很有必要的。可以用状态转移行为{next_event, EventType, EventContent}完成。

我们可以产生任何已有的类型事件，但是由于 *internal* 类型事件只能通过 *next_event* 行为产生。因此，它不能来源于外部代码。因此可以确定一个 *internal* 事件是自己给自己生成的。

使用自产事件的一个例子是当你有一个明确状态进入行为的状态机。你可以编写使用普通函数做状态转移。但是，如果你想对其它状态逻辑隐藏这些代码，你可以插入一个 *internal* 事件处理entry行为。这也可能有同样不幸的结果：每次进入某个状态，都必须明确插入 *internal* 事件或者使用状态转移函数。

下面是一个使用 *internal* 事件实现entry行为的例子， *enter* 要用到一个辅助函数 *enter/3* ：

```
...
init(Code) ->
    process_flag(trap_exit, true),
    Data = #{code => Code},
    enter(ok, locked, Data).

callback_mode() ->
    state_functions.

locked(internal, enter, _Data) ->
    do_lock(),
    {keep_state,Data#{remaining => Code}};

locked(cast, {button,Digit},#{code := Code, remaining := Remaining} = Data) ->
    case Remaining of
        [Digit] ->
        	   enter(next_state, open, Data);
...

open(internal, enter, _Data) ->
    Tref = erlang:start_timer(10000, self(), lock),
    do_unlock(),
    {keep_state,Data#{timer => Tref}};

open(info, {timeout,Tref,lock}, #{timer := Tref} = Data) ->
	 enter(next_state, locked, Data);

enter(Tag, State, Data) ->
    {Tag,State,Data,[{next_event,internal,enter}]}.
```

##### 9.4.15 例子回顾

这一节的例子包括所有提及的例子以及相关改动，增加了使用entry行为的代码。

注意这个状态图（没有贴）没有明确open状态时按钮事件如何处理。因此你需要注意有的地方不明确的事件会被忽略但是在其它状态会被处理。此外，状态图没有显示 *code_length/0* 会在每个状态被调用。

```
-module(code_lock).
-behaviour(gen_statem).
-define(NAME, code_lock_2).
-export([start_link/1,stop/0]).
-export([button/1,code_length/0]).
-export([init/1,callback_mode/0,terminate/3,code_change/4]).
-export([locked/3,open/3]).

start_link(Code) ->
    gen_statem:start_link({local,?NAME}, ?MODULE, Code, []).
stop() ->
    gen_statem:stop(?NAME).

button(Digit) ->
    gen_statem:cast(?NAME, {button,Digit}).
code_length() ->
    gen_statem:call(?NAME, code_length).

init(Code) ->
    process_flag(trap_exit, true),
    Data = #{code => Code},
    enter(ok, locked, Data).

callback_mode() ->
    state_functions.

locked(internal, enter, #{code := Code} = Data) ->
    do_lock(),
    {keep_state,Data#{remaining => Code}};
locked(cast, {button,Digit},#{code := Code, remaining := Remaining} = Data) ->
    case Remaining of
        [Digit] -> % Complete
            enter(next_state, open, Data);
        [Digit|Rest] -> % Incomplete
            {keep_state,Data#{remaining := Rest}};
        [_|_] -> % Wrong
            {keep_state,Data#{remaining := Code}}
    end;
locked(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

open(internal, enter, Data) ->
    Tref = erlang:start_timer(10000, self(), lock),
    do_unlock(),
    {keep_state,Data#{timer => Tref}};
open(info, {timeout,Tref,lock}, #{timer := Tref} = Data) ->
    enter(next_state, locked, Data);
open(cast, {button,_}, _) ->
    {keep_state_and_data,[postpone]};
open(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

handle_event({call,From}, code_length, #{code := Code}) ->
    {keep_state_and_data,[{reply,From,length(Code)}]}.

enter(Tag, State, Data) ->
    {Tag,State,Data,[{next_event,internal,enter}]}.

do_lock() ->
    io:format("Locked~n", []).

do_unlock() ->
    io:format("Open~n", []).

terminate(_Reason, State, _Data) ->
    State =/= locked andalso do_lock(),
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.
```

###### 回调模式：handle_event_function

这一章节描述使用一个 *handle_event/4* 函数处理事件。之前根据事件处理分支的例子不能完成我们的目标因为有entry行为。因此这个例子根据状态做分支处理：

```
...
-export([handle_event/4]).
...

callback_mode() ->
    handle_event_function.

%% State: locked
handle_event(internal, enter, locked, #{code := Code} = Data) ->
    do_lock(),
    {keep_state,Data#{remaining => Code}};
handle_event(cast, {button,Digit}, locked, #{code := Code, remaining := Remaining} = Data) ->
    case Remaining of
        [Digit] -> % Complete
            enter(next_state, open, Data);
        [Digit|Rest] -> % Incomplete
            {keep_state,Data#{remaining := Rest}};
        [_|_] -> % Wrong
            {keep_state,Data#{remaining := Code}}
    end;

%% State: open
handle_event(internal, enter, open, Data) ->
    Tref = erlang:start_timer(10000, self(), lock),
    do_unlock(),
    {keep_state,Data#{timer => Tref}};
handle_event(info, {timeout,Tref,lock}, open, #{timer := Tref} = Data) ->
    enter(next_state, locked, Data);
handle_event(cast, {button,_}, open, _) ->
    {keep_state_and_data,[postpone]};

%% Any state
handle_event({call,From}, code_length, _State, #{code := Code}) ->
    {keep_state_and_data,[{reply,From,length(Code)}]}.
...
```

##### 9.4.16 过滤状态

目前为止，所有的服务程序例子都在错误log日志中打印出所有内部状态，例如，当被exit信号终止或因为内部错误。

状态数据可以看作敏感数据，当不可预知错误发生时你可能不希望这些信息被记录到日志中。

另一个需要过滤状态的原因是状态可能太大，完全打印出来很困难，因此需要去掉错误日志不关心的细节。

可以通过格式化内部状态，通过调用 *sys:get_status/1,2* 实现 *Module:format_status/2* ，例如下面的代码：

```
...
-export([init/1,terminate/3,code_change/4,format_status/2]).
...
format_status(Opt, [_PDict,State,Data]) ->
    StateData = {State, maps:filter(
    							fun (code, _) -> false; 
    							    (remaining, _) -> false; 
    							    (_, _) -> true 
    							end,
    							Data)},
    case Opt of
        terminate ->
            StateData;
        normal ->
            [{data,[{"State",StateData}]}]
    end.
```

format_status并不是强制实现的，如果没有，默认实现类似这个例子，只是不会过滤Data，类似 StateData = {State, Data}，包括了敏感信息。

##### 9.4.17 复杂状态

回调模式 *handle_event_function* 允许使用非原子状态，例如，用元组作为复杂状态描述。

当状态信息影响事件处理时我们希望用刀复杂状态描述，尤其是涉及延迟处理事件。我们为之前的例子增加一个可配置的lock按钮，可以在open状态时直接给门上锁，一个API函数 *set_lock_button/1* 用来设置锁按钮。

假设我们现在调用set_lock_button，此时门打开，而且已经延迟发送了一个按钮事件，因此目前为止还没有lock按钮。乐观的说，按钮事件发生很早，因此我们没有把它当作lock事件。然而，当状态变为locked时，一个普通的按钮事件被当作lock按钮。

因此我们通过 *button/1* 函数同步的通过 *gen_statem:call* 在open状态延迟处理事件。open状态时的button/1调用不会返回，直到状态变为locked。之后这个时间才能得到处理并返回响应。

如果一个进程调用 *set_lock_button/1* 改变lock按钮，此时另外的进程在button函数中挂起并等待新的lock按钮，挂起的lock按钮调用回立即产生作用并上锁。因此，我们将当前锁按钮变成状态的一部分。我们改变lock锁，状态随之改变，并且所有延迟事件得到处理。

我们定义状态{StateName, LockButton}, StateName是之前的锁按钮，LockButton是新的锁按钮：

```
-module(code_lock).
-behaviour(gen_statem).
-define(NAME, code_lock_3).
-export([start_link/2,stop/0]).
-export([button/1,code_length/0,set_lock_button/1]).
-export([init/1,callback_mode/0,terminate/3,code_change/4,format_status/2]).
-export([handle_event/4]).

start_link(Code, LockButton) ->
    gen_statem:start_link({local,?NAME}, ?MODULE, {Code,LockButton}, []).
    
stop() ->
    gen_statem:stop(?NAME).
    
button(Digit) ->
    gen_statem:call(?NAME, {button,Digit}).
    
code_length() ->
    gen_statem:call(?NAME, code_length).
    
set_lock_button(LockButton) ->
    gen_statem:call(?NAME, {set_lock_button,LockButton}).

init({Code,LockButton}) ->
    process_flag(trap_exit, true),
    Data = #{code => Code, remaining => undefined, timer => undefined},
    enter(ok, {locked,LockButton}, Data, []).

callback_mode() ->
    handle_event_function.

handle_event({call,From}, {set_lock_button,NewLockButton},{StateName,OldLockButton}, Data) ->
    {next_state,{StateName,NewLockButton},Data,[{reply,From,OldLockButton}]};
handle_event({call,From}, code_length,{_StateName,_LockButton}, #{code := Code}) ->
    {keep_state_and_data,[{reply,From,length(Code)}]};
handle_event(EventType, EventContent,{locked,LockButton}, #{code := Code, remaining := Remaining} = Data) ->
    case {EventType,EventContent} of
        {internal,enter} ->
            do_lock(),
            {keep_state,Data#{remaining := Code}};
        {{call,From},{button,Digit}} ->
            case Remaining of
                [Digit] -> % Complete
                    next_state({open,LockButton}, Data,[{reply,From,ok}]);
                [Digit|Rest] -> % Incomplete
                    {keep_state,Data#{remaining := Rest},
                    [{reply,From,ok}]};
                [_|_] -> % Wrong
                    {keep_state,Data#{remaining := Code},[{reply,From,ok}]}
            end 
    end;
handle_event(EventType, EventContent,{open,LockButton}, #{timer := Timer} = Data) ->
    case {EventType,EventContent} of
        {internal,enter} ->
            Tref = erlang:start_timer(10000, self(), lock),
            do_unlock(),
            {keep_state,Data#{timer := Tref}};
        {info,{timeout,Timer,lock}} ->
            next_state({locked,LockButton}, Data, []);
        {{call,From},{button,Digit}} ->
            if
                Digit =:= LockButton ->
                    erlang:cancel_timer(Timer),
                    next_state({locked,LockButton}, Data,[{reply,From,locked}]);
                true ->
                    {keep_state_and_data,[postpone]}
            end 
    end.
next_state(State, Data, Actions) ->
    enter(next_state, State, Data, Actions).
enter(Tag, State, Data, Actions) ->
    {Tag,State,Data,[{next_event,internal,enter}|Actions]}.
do_lock() ->
    io:format("Locked~n", []).
do_unlock() ->
    io:format("Open~n", []).
terminate(_Reason, State, _Data) ->
    State =/= locked andalso do_lock(),
    ok.
code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.

format_status(Opt, [_PDict,State,Data]) ->
    StateData = {State, maps:filter(
                            fun (code, _) -> false;
                                (remaining, _) -> false;
                                (_, _) -> true
                            end,
                        Data)},
    case Opt of
        terminate ->
            StateData;
        normal ->
            [{data,[{"State",StateData}]}]
    end.
```

对于物理密码锁这不是合适的模型，因为button回挂起直到锁被锁上，但是对于通用API来说这并不少见。

##### 9.4.18 休眠

如果你在一个节点运行很多服务器，它们在某些状态时可能会空闲一段时间，这些服务器占用的对内存是个问题。这些进程的内存空间可以通过调用 *proc_lib:hibernate/3* 来休眠。

*注意：休眠进程是很消耗资源，具体查看erlang:hibernate/3，你不会想要每个事件之后都调用它。*

这个例子我们在{open,_}状态休眠，因为正常情况下直到超时转移到{locked,}状态：

```
handle_event(
    EventType, EventContent,
    {open,LockButton}, #{timer := Timer} = Data) ->
        case {EventType,EventContent} of
            {internal,enter} ->
                Tref = erlang:start_timer(10000, self(), lock),
                do_unlock(),
                {keep_state,Data#{timer := Tref},[hibernate]};
```

当进入open状态时，最后一行返回的[hibernate]行为列表是唯一的变化。如果任何事件到达处于{open,_}的进程，我们不必担心重新进入休眠，因此任何时间到达后服务器会保持唤醒状态。

为了改变这一点，我们需要在更多的地方加入hibernate行为。例如，状态独立的set_lock_button和code_length操作加入hibernate会导致代码杂乱。

这个服务器没有用堆内存需要用到休眠。为了从休眠获得优势，你的服务器需要在回调执行中产生垃圾，那样的服务器例子就是一个垃圾例子。
