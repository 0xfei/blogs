
---
title: "Erlang学习——语法和特性"
date: 2014-08-19T09:13:30+08:00
draft: true
categories: ["编程语言"]
tags: ["编程语言"]
topics: ["编程语言"]
---

Erlang真的很像Prolog，阿姆斯特朗博士在他的著作《Programming Erlang》开头解释为何要学习Erlang时，提到了Erlang的五大特点：

<!--more-->


```

Here are five reasons why you should learn Erlang:
	 You want to write programs that run faster when you run them on a multicore computer.
	 You want to write fault-tolerant applications that can be modified without taking them out of service.
	 You've heard about **functional programming** and you're wondering whether the techniques really work.
	 You want to use a language that has been battle tested in real large-scale industrial products that has great libraries and an active user community.
	 You don't want to wear your fingers out by typing lots of lines of code.

```


Erlang在多处理器时代有更好的前途，有很好的容错机制，是一种函数式编程语言，同时学习了Prolog的不少特性，可在运行时更改代码而不影响程序运行。

语法确实有点儿奇怪，也和传统的函数式编程语言如Lisp等差距很大，《七周七语言》有一章专门讲Erlang，介绍了函数、列表、高阶函数等基础语法，我写了写上面的几个例题。字符串计数和列表匹配比较有意思。


```

-module(seven).

-export([number_string/1]).
-export([output_two/1]).

number_string([Head|Tail]) -> 
    number_string([Head|Tail], 0).

number_string([Head|Tail], Number) -> 
    case Tail of
	"" -> 
	    if 
		[Head] == " " -> 
	            Number;
		true -> 
		    Number + 1
	    end;
	_  -> 
	    if
		[Head] == " " -> 
		    number_string(Tail, Number+1);
		true -> 
		    number_string(Tail, Number)
	    end
    end.

output_two(List) -> 
    [io:format("~p: ~p~n", [X, Y]) || {X, Y} 

```


这只是Erlang的基本语法，没有什么太值得称道的。

Erlang极少有可能崩溃，无线程，使用消息同步机制，能很轻松就是先安全、健壮、高效的并发，这是其他编程语言梦寐以求的。不过Go和apple的swift应该也具备了这些特性，不知道跟Erlang比起来如何。看起来它就是专为服务器而生的语言。

一个小的消息收发例子：


```

-module('cs').

-export([start/0]).
-export([pre_client/0, pre_server/2]).


pre_server(N, ClientID) -> 
    link(ClientID),
    server(N, ClientID).

server(N, ClientID) -> 
    io:format("The N is ~p~n", [N]),
    if    N == 0 -> 
            io:format("Exit when N is ~p~n", [N]),
            exit(server_finish);
        N > 0 -> 
	    ClientID ! {i_am_server, self()},
	    receive
	        i_am_client -> 
		   io:format("I'm server, receive client.~n");
		_ -> 
		   io:format("Recieve others~n")
	    end,
	server(N-1, ClientID)
    end.


pre_client() -> 
    process_flag(trap_exit, true),
    client().
	
client() -> 
    receive
 	finish -> 
	    io:format("I'm client, and going to finish~n");
	    {i_am_server, ServerID} -> 
		ServerID ! i_am_client,
		io:format("I'm client, receive from server.~n"),
		client();
	    {'EXIT', From, Reason} -> 
		io:format("~w~n", [{'EXIT', From, Reason}])
	end.

start() -> 
    %%register(client, spawn(cs, pre_client, [])),
    ClientID = spawn(cs, pre_client, []),
    spawn(cs, pre_server, [5, ClientID]).


```
