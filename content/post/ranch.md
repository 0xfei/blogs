+++
date = "2016-12-08T00:10:44+08:00"
title = "tcp连接池ranch"
draft = true
description = "tcp acceptor pool ranch"
tags = ["Erlang/OTP", "code", "ranch"]
topics = ["Erlang/OTP", "code", "ranch"]
+++


[ranch](https://github.com/ninenines/ranch) 是Erlang/OTP实现的 TCP 连接池，我在看cowboy源码的时候意外发现了这个组件。让人欣喜的是它本身就非常强大，只需要短短几行代码，就可以实现强大的tcp服务器功能，而socket管理的细节都被ranch封装起来。

运行anch_app:start(_,_)启动服务，然后就可以使用自己实现的服务。晚上看了一下它的源码，非常短，但是很成熟，是学习的好例子。

ranch的启动大致如下流程：

```
ranch_app:start(_,_) %% application
-> ranch_sup:start_link() %% supervisor 
-> ranch_sup:init() %% supervisor callback
-> ets:new(ranch_server, ...) %% one_for_one, ranch_server
-> ranch_server:start_link() %% gen_server
```

ranch_server提供tcp连接池管理，其它模块通过和ranch_server交互，维护自己的连接信息、socket选项等。

<!--more-->

类似supervisor，ranch_server由进程提供的ref标示，只要保证唯一就行。导出了一系列操作函数：

```
-export([set_new_listener_opts/3]).
-export([cleanup_listener_opts/1]).
-export([set_connections_sup/2]).
-export([get_connections_sup/1]).
-export([set_addr/2]).
-export([get_addr/1]).
-export([set_max_connections/2]).
-export([get_max_connections/1]).
-export([set_protocol_options/2]).
-export([get_protocol_options/1]).
-export([count_connections/1]).
```

几乎都是简单的ets表操作。注意_connections_sup，这是每个连接的supervisor，同样记录在ets表中。server会monitor这些监控进程，它并不会直接干涉具体的工作进程。

*set_connections_sup时直接monitor新的进程，是不是先把老的监控进程退出会更好？或许这个调用更多的由调用者负责？老的monitor退出时，因为server收到‘DOWN’消息时会删除表项。*

ranch_server只是简单的维护中心，具体的工作内容在其它模块中。这里先看一个官方提供的sample。

要使用ranch，首先调用ranch:start_listener监听一个端口，它接收一个标识符、初始连接数目、发送模块（branch提供）、发送参数、协议模块（自己实现）和协议参数。发送模块目前有ranch_tcp、ranch_ssl，可以根据规范自己添加。

确认发送模块已经加载并导出name函数后，用ranch_sup——整个系统的根监控者，启动子进程：

```
{{ranch_listener_sup, Ref}, {ranch_listener_sup, start_link, [Ref, NumAcceptors, Transport, TransOpts, Protocol, ProtoOpts]}, permanent, infinity, supervisor, [ranch_listener_sup]}.
```

创建一个低层监控器，ranch_listener_sup，并接收全部这些参数。ranch_listener_sup又会启动更低层的两个supervisor：ranch_conns_sup和ranch_acceptors_sup，用rest_for_one模式，因为连接和接收消息是有顺序的。

最后如果有socket参数，会把它交给ranch_acceptors_sup监控，我猜测是用于可能的socket继承。

```
Children = supervisor:which_children(Pid),
{_, AcceptorsSup, _, _} = lists:keyfind(ranch_acceptors_sup, 1, Children),
Transport:controlling_process(Socket, AcceptorsSup)
```

完整的启动过程还需要参考ranch_conns_sup.erl和ranch_acceptors_sup.erl两个模块，这里分别介绍它们的工作流程。

ranch_conns_sup并非基于supervisor行为模式，而是直接调用proc_lib的函数，让自己的行为合乎supervisor规范。这是很常见的做法，可以自定义消息的处理流程。proc_lib:start_link启动调用init的新进程，在init中，首先设置ref的connection监控，通过ranch_server记录到ets表。随后进入loop循环，处理协议模块的启动（为soket指定数据接收监控器）、休眠（因为最大连接的限制）、连接的激活和移除等。需要理解它是端口级别的监控器，每个服务要有一个ranch_conns_sup负责监控。

ranch_acceptors_sup紧随ranch_conns_sup启动，它是supervisor行为模式的端口级别的监控器。init回调中首先回调用户协议的listen函数，创建服务的监听socket。然后它会创建指定数量的ranch_acceptor worker进程，来具体的接收消息。

```
Procs = [{{acceptor, self(), N}, 
          {ranch_acceptor, start_link, [LSocket, Transport, ConnsSup]}, 
          permanent, brutal_kill, worker, []} || N <- lists:seq(1, NumAcceptors)],
{ok, {{one_for_one, 1, 5}, Procs}}.
```

ranch_acceptor进程调用协议模块的accept回调，等待连接；并将返回的客户端套接字交给ConnsSup控制，并调用ranch_conns_sup:start_protocol启动数据接收：

```
-spec start_protocol(pid(), inet:socket()) -> ok.
start_protocol(SupPid, Socket) ->
	SupPid ! {?MODULE, start_protocol, self(), Socket},
	receive SupPid -> ok end.
```

ranch_conns_sup 的 loop循环中

```
{?MODULE, start_protocol, To, Socket} ->
	try Protocol:start_link(Ref, Socket, Transport, Opts) of
		{ok, Pid} ->
			shoot(State, CurConns, NbChildren, Sleepers, To, Socket, Pid, Pid);
			...
```

这里省去了错误处理，总之会回调协议的start_link函数，在用户回调的init函数中，首先调用ranch:accept_ack确认tcp通道打开：

```
-spec accept_ack(ref()) -> ok.
accept_ack(Ref) ->
	receive {shoot, Ref, Transport, Socket, AckTimeout} ->
		Transport:accept_ack(Socket, AckTimeout)
	end.
```

这个{shoot, ...}消息由ranch_conns_sup:shoot发送：

```
shoot(State=#state{ref=Ref, transport=Transport, ack_timeout=AckTimeout, max_conns=MaxConns},
		CurConns, NbChildren, Sleepers, To, Socket, SupPid, ProtocolPid) ->
	case Transport:controlling_process(Socket, ProtocolPid) of
		ok ->
			ProtocolPid ! {shoot, Ref, Transport, Socket, AckTimeout},
			put(SupPid, active),
			CurConns2 = CurConns + 1,
			if CurConns2 < MaxConns ->
					To ! self(),
					loop(State, CurConns2, NbChildren + 1, Sleepers);
				true ->
					loop(State, CurConns2, NbChildren + 1, [To|Sleepers])
			end;
       ...
	end.
```

这里把套接字控制权交给协议模块的pid，即用户实现的模块。这里可以看到，如果连接数超过MaxConns，则加入Sleepers列表；等有连接退出，即ranch_conns_sup:start_protocol的调用者、accept调用者ranch_acceptor进程，会从Sleeper中取出pid，并发回响应。这里真心感觉到Erlang的强大，极其方便的控制大量进程。

至此用户模块已经可以自由的控制socket，包括设置半阻塞选项等，方便数据收发。