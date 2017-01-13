+++
title = "poolboy——Erlang进程池"
date = "2017-01-13T18:24:11+08:00"
draft = true
description = "A hunky Erlang worker pool factory"
tags = ["Erlang/OTP", "poolboy", "Code"]
topics = ["Erlang/OTP", "poolboy", "Code"]
+++

[poolboy](https://github.com/devinus/poolboy)是Erlang进程池，应用广泛，代码短小精悍（400行不到）。PostgreSQL的Erlang客户端就用到了poolboy，其他DB库也有用，比如redis等。由于足够简洁，所以坑不少，[这篇文章](https://segmentfault.com/a/1190000002894808)做了总结。任何进程池的目的都是很明确的——预先创建特定数目的进程，执行反复的一次性工作，随后接受直接提供服务。用Erlang实现进程池更是易如反掌，只需要按照OTP规范写一个监控树就行。

poolboy使用简单，官方README和例子说的很清楚。简要列一下工作流程：

```
poolboy:start/2(start_link/2) -> poolboy gen_server process
poolboy:init -> poolboy_sup -> N worker_process
poolboy:transaction -> select one worker_process from pool and execute function
```

下面详细看源码。

<!--more-->

poolboy是基于`gen_server`的模块，管理以 `poolboy_sup` 为根的进程池子进程，并提供接口。每个池都是一个poolboy服务器进程，服务器状态结构体和进程池标识如下：

```
-type pool() ::
    Name :: (atom() | pid()) |
    {Name :: atom(), node()} |
    {local, Name :: atom()} |
    {global, GlobalName :: any()} |
    {via, Module :: atom(), ViaName :: any()}.

-record(state, {
    supervisor :: pid(),					%% 监控器的pid
    workers :: [pid()],						%% 工作进程列表
    waiting :: pid_queue(),					%% 等待接受服务的外部进程pid
    monitors :: ets:tid(),					%% 正在执行服务的外部进程，是ets表
    size = 5 :: non_neg_integer(),			%% 默认worker进程数目
    overflow = 0 :: non_neg_integer(),		%% 是否允许额外创建
    max_overflow = 10 :: non_neg_integer(),	%% 最多允许的额外进程
    strategy = lifo :: lifo | fifo			%% 回收进程方式（个人感觉用处不太大）
}).
```

worker进程由用户提供，poolboy用queue管理请求服务却没有空闲worker的进程。

`poolboy:checkout/3` 从池中取一个worker进程：

```
handle_call({checkout, CRef, Block}, {FromPid, _} = From, State) ->
    #state{supervisor = Sup,
           workers = Workers,
           monitors = Monitors,
           overflow = Overflow,
           max_overflow = MaxOverflow} = State,
    case Workers of
        [Pid | Left] ->
            MRef = erlang:monitor(process, FromPid),
            true = ets:insert(Monitors, {Pid, CRef, MRef}),
            {reply, Pid, State#state{workers = Left}};
        [] when MaxOverflow > 0, Overflow < MaxOverflow ->
            {Pid, MRef} = new_worker(Sup, FromPid),
            true = ets:insert(Monitors, {Pid, CRef, MRef}),
            {reply, Pid, State#state{overflow = Overflow + 1}};
        [] when Block =:= false ->
            {reply, full, State};
        [] ->
            MRef = erlang:monitor(process, FromPid),
            Waiting = queue:in({From, CRef, MRef}, State#state.waiting),
            {noreply, State#state{waiting = Waiting}}
    end;
```

逻辑很清晰，如果有空闲进程，直接返回，并修改Monitor ets表和剩余worker进程；允许创建额外进程，就通过 `new_worker` 创建并返回；最后不得已会加入 waiting 队列。

与checkout对应，checkin将使用完的worker放回列表：

```
handle_cast({checkin, Pid}, State = #state{monitors = Monitors}) ->
    case ets:lookup(Monitors, Pid) of
        [{Pid, _, MRef}] ->
            true = erlang:demonitor(MRef),
            true = ets:delete(Monitors, Pid),
            NewState = handle_checkin(Pid, State),
            {noreply, NewState};
        [] ->
            {noreply, State}
    end;
```

waiting队列的进程接收服务的过程在 `handle_checkin` ：

```
handle_checkin(Pid, State) ->
    #state{supervisor = Sup,
           waiting = Waiting,
           monitors = Monitors,
           overflow = Overflow,
           strategy = Strategy} = State,
    case queue:out(Waiting) of
        {{value, {From, CRef, MRef}}, Left} ->
            true = ets:insert(Monitors, {Pid, CRef, MRef}),
            gen_server:reply(From, Pid),
            State#state{waiting = Left};
        {empty, Empty} when Overflow > 0 ->
            ok = dismiss_worker(Sup, Pid),
            State#state{waiting = Empty, overflow = Overflow - 1};
        {empty, Empty} ->
            Workers = case Strategy of
                lifo -> [Pid | State#state.workers];
                fifo -> State#state.workers ++ [Pid]
            end,
            State#state{workers = Workers, waiting = Empty, overflow = 0}
    end.
```

`dismiss_worker` 退出额外的worker进程：

```
dismiss_worker(Sup, Pid) ->
    true = unlink(Pid),
    supervisor:terminate_child(Sup, Pid).
```

请求服务的进程可以设置等待时间，通过 `cancel_waiting` 取消等待：

```
handle_cast({cancel_waiting, CRef}, State) ->
    case ets:match(State#state.monitors, {'$1', CRef, '$2'}) of
        [[Pid, MRef]] ->
            demonitor(MRef, [flush]),
            true = ets:delete(State#state.monitors, Pid),
            NewState = handle_checkin(Pid, State),
            {noreply, NewState};
        [] ->
            Cancel = fun({_, Ref, MRef}) when Ref =:= CRef ->
                             demonitor(MRef, [flush]),
                             false;
                        (_) ->
                             true
                     end,
            Waiting = queue:filter(Cancel, State#state.waiting),
            {noreply, State#state{waiting = Waiting}}
    end;
```

这里处理两种情况：以及接受服务或者还在等待。

最后处理的情况是monitor或link的进程退出：

```
handle_info({'DOWN', MRef, _, _, _}, State) ->
    case ets:match(State#state.monitors, {'$1', '_', MRef}) of
        [[Pid]] ->
            true = ets:delete(State#state.monitors, Pid),
            NewState = handle_checkin(Pid, State),
            {noreply, NewState};
        [] ->
            Waiting = queue:filter(fun ({_, _, R}) -> R =/= MRef end, State#state.waiting),
            {noreply, State#state{waiting = Waiting}}
    end;
handle_info({'EXIT', Pid, _Reason}, State) ->
    #state{supervisor = Sup,
           monitors = Monitors} = State,
    case ets:lookup(Monitors, Pid) of
        [{Pid, _, MRef}] ->
            true = erlang:demonitor(MRef),
            true = ets:delete(Monitors, Pid),
            NewState = handle_worker_exit(Pid, State),
            {noreply, NewState};
        [] ->
            case lists:member(Pid, State#state.workers) of
                true ->
                    W = lists:filter(fun (P) -> P =/= Pid end, State#state.workers),
                    {noreply, State#state{workers = [new_worker(Sup) | W]}};
                false ->
                    {noreply, State}
            end
    end;
```

'DOWN'消息表示外部进程退出，跟 `cancel_waiting` 处理类似；'EXIT'表示worker进程退出，如果该进程正在处理服务，首先结束当前和外部进程的关联，然后在 `handle_worker_exit` 处理新进程和waiting队列的关系。否则直接新建一个worker。

```
handle_worker_exit(Pid, State) ->
    #state{supervisor = Sup,
           monitors = Monitors,
           overflow = Overflow} = State,
    case queue:out(State#state.waiting) of
        {{value, {From, CRef, MRef}}, LeftWaiting} ->
            NewWorker = new_worker(State#state.supervisor),
            true = ets:insert(Monitors, {NewWorker, CRef, MRef}),
            gen_server:reply(From, NewWorker),
            State#state{waiting = LeftWaiting};
        {empty, Empty} when Overflow > 0 ->
            State#state{overflow = Overflow - 1, waiting = Empty};
        {empty, Empty} ->
            Workers =
                [new_worker(Sup)
                 | lists:filter(fun (P) -> P =/= Pid end, State#state.workers)],
            State#state{workers = Workers, waiting = Empty}
    end.
```

这里的处理和 checkin 也有点类似。