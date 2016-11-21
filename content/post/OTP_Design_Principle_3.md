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

子进程的启动和监控通过 *子进程策略(child specifications)* 列表来确定。子进程按列表顺序启动，按相反顺序终止。

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

返回列表的ChildSpecs变量是子进程策略的列表。

##### 9.6.3 监视器标志

这是监视器标志位的类型定义：

```
sup_flags() = #{strategy => strategy(), % optional
				intensity => non_neg_integer(), % optional
				period => pos_integer()} % optional
strategy() = one_for_all | one_for_one | rest_for_one | simple_one_for_one
```

*   *strategy* 表示重启策略
*	*intensity* 和 *period* 表明 *maximum restart intensity*

##### 9.6.4 重启策略

