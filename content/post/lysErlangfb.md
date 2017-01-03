+++
title = "Learn you some Erlang for great good!"
date = "2016-12-31T14:47:10+08:00"
draft = true
description = "Learn you some Erlang for great good!"
tags = ["Erlang/OTP", "Book", "reading"]
topics = ["Erlang/OTP", "Book", "reading"]
+++

[《Erlang趣学指南》](http://learnyousomeerlang.com/content) ，国内最新引进翻译的Erlang书，实际上13年就已经写好了。虽然如此，依旧是市面上最新的相关书籍。“趣”真说不上，作者的搞笑有点刻意，他的图画的倒是很有趣。这本书比它看起来要难很多，言简意赅，涉及到Erlang的方方面面。例子也都很实际。作者把所有章节都放了出来，可以免费看。

花了两天时间抽空看完，记录一些容易忽略的小知识，也学习它的有用的例子。

<!--more-->

atom很容易让人忽略的陷阱是它会保存在atom表里，并且不会被GC回收。所以尽量不要使用list_to_atom等，使用约定俗成的原子是好习惯。

Erlang中，不同类型可以比较，number < atom < reference < fun < port < pid < tuple < list < bit string。作为动态语言，Erlang又有强制类型的特性，比如 a + 1 会出错。

非严格列表（improper list）除了看起来比较屌，几乎不会带来任何好处：

```
21> [A|B] = [1|a].
[1|a]
22> A.
1
23> B.
a
24> [C|_] = B.
** exception error: no match of right hand side value a
```

和列表速构类似，二进制速构很强大，但是却很容易忽略。注意语法略有不同 <= 和 <-。

```
30> << <<V:32>> || <<V>> <= <<1,2,3,4,5>> >>.
<<0,0,0,1,0,0,0,2,0,0,0,3,0,0,0,4,0,0,0,5>>
```

另一个常用的数据类型是record记录，Erlang的记录有时候让人迷惑，比如 R = #state{} ，模式匹配的时候可以不关心记录的具体内容，随后又可以用 R#state.key 取数据；也可以做进一步匹配 R = #state{key=Value}等。

```
-record(state, {key = value,
				name = '0x01f'
			}
	).

record_test() ->
	R1 = #state{key=new_value, name='not_0x01f'},
	R2 = R1#state{key=old_value},
	R3 = R2#state{key = value},
	R4 = R3#state{name='0x01f'},
	R4 = #state{},
	R5 = #state{},
	io:format("~p ~p~n", [R5#state.key, R5#state.name]).
```

总之记住一点，Erlang的变量只能一次赋值，记录也是如此。所以只能 R#state.name 这样访问数据，而不能修改。

K-V型的数据类型有proplists（据说效率堪忧）、dict、maps、gb_tree，根据数据量大小选择合适的类型；多进程时可选的就比较少了，几乎只有ets/dets和mnesia，但是效率和稳定性毋庸置疑。Erlang还有array类型，使用索引get和set值，但是用的非常少，效率也不行，这种逻辑本质上不太符合Erlang设计理念。没用过的还有queue、set（四种不同的集合）、digraph 等。

编译选项可以通过-compile模块属性设置，例如{d,Macro,Value}、export_all等。也可以在erlc时指定。

异常在某些情况可以当做“goto”使用，下面的例子，函数 c 通过erlang:throw/0生成错误，直接退回到a的错误处理部分。

```
a() ->
	io:format("Before b()~n"),
	try b() of
		_ -> ok
	catch
		throw:V ->
			io:format("throw ~p~n", [V])
	end,
	io:format("After b()~n").
	
b() ->
	io:format("Before c()~n"),
	c(),
	io:format("After c()~n").
	
c() ->
	io:format("Before raise()~n"),
	erlang:throw({throw, test}),
	io:format("After raise()~n").
```

这本是提到error和exit的区别在于是否包含发生错误的栈数据，但是在我的测试中，只要捕获异常，都可以通过erlang:get_stacktrace/0得到栈；崩溃时也会显示信息。这点以后研究源码的时候注意。throw倒是和error、exit有明显不同，catch子句中不带冒号时，默认捕获throw信息。

受保护的语句不支持尾递归，但是try..of/catch子句中，of和catch之间不受保护的语句可以尾递归。所以基本原则还是及早触发和捕获问题。

作为函数式编程语言的重要特性之一，高阶函数在Erlang中很常用。最常用的作为lists模块某些函数参数、闭包等。

```
1> F = fun(A) -> fun(X) -> X > A end end.
#Fun<erl_eval.6.52032458>
2> K = F(10).
#Fun<erl_eval.6.52032458>
3> K(1).
false
4> K(11).
true 
```

R17开始，Erlang 支持匿名具名函数，可用于递归：

```
F = fun 
		Loop([H|T]) -> io:format("~p~n", [H]), Loop(T); 
		Loop([]) -> ok 
	end.
```

#### 分布式Erlang

并发（concurrency）和并行（parallelism）的区别是老生常谈，翻译成这样实在是误导人。并发更多的是概念和逻辑层面的，表示多个任务可以独立运行，互不影响；而并行是物理上的，表示多个任务同时运行。Erlang天生就支持并发，但是要在多核机器上发挥作用，还是需要做不少修改，达到真正的并行。

常见的消息刷新方式：

```
flush() ->
	receive 
		A -> flush()
	after 
		0 -> ok
	end.
```

选择性消息接收并不可取，消息数量变多时性能会急剧下降。

exit/2消息可以通过发送kill消息杀掉任意进程，而链接进程收到的消息是'killed'，防止自己也被杀掉。所以VM会对消息做些处理，方便用户编写程序：

```
start() ->
	process_flag(trap_exit, true),
	Pid = spawn_link(?MODULE, flush, []),
	io:format("Pid ~p~n", [Pid]),
	exit(Pid, kill),
	receive
		A -> io:format("~p~n", [A])
	end.

flush() ->
	io:format("flush~n"),
	timer:sleep(10000),
	flush().
```

```
20> test:start().
Pid <0.102.0>
flush
{'EXIT',<0.102.0>,killed}
ok
```

monitor监控进程要比link更有效，监控的进程退出时，会收到{'DOWN', MonitorReference, process, Pid, Reason}消息。demonitor时指定[flush, info]选项，如果进程已经退出，会刷新DOWN消息。

#### OTP

OTP是一套框架，基于进程链接和消息分发，提供编写稳定Erlang程序的框架，只需编写回调即可。 ```gen_server``` 和 ```supervisor``` 用的多，所以我重点看了下 ```gen_fsm``` 状态机行为模式。书中的例子是一个交易模型，允许随时取消交易，并将交易过程分成多个阶段，保证双方一致性。

进入交易阶段，需要利用FSM通知对方，为了避免死锁需要用异步消息，所以多一个阶段，提出和接收交易请求后进入新的等待状态，相当于进入交易前一层缓冲；在此状态也能通过取消回到普通等待。完成协商，并真正确认交易的流程也是类似两个步骤。交易是两方的，一方提供完成后进入协商完成状态——即等待，等另一方的协商完成；另一方的商品变动触发本方重新计入协商状态。

这里还个坑，看下图:

![fsm_race_wait](http://0x01f.com/images/fsm_race_wait.png "fsm_race_wait")

复杂的状态转移确实容易出问题。

这个例子不是简单的实例，而是完整的实用模型。考虑问题很全面，值得认真[看看](http://learnyousomeerlang.com/finite-state-machines)。

