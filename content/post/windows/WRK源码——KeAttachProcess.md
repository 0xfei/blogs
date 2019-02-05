
---
title: "WRK源码——KeAttachProcess"
date: 2014-07-25T09:13:30+08:00
draft: true
categories: ["Windows安全"]
tags: ["Windows安全"]
topics: ["Windows安全"]
---

周五没事可做，搞了个脚本跑一下百度可免费下载的歌曲，然后看看WRK。今天看了一下KeAttachProcess。

KeAttachProcess将当前线程加载到特定进程空间。如果进程已经在内存中，那就直接进入进程空间执行代码；否则需要把当前执行线程插入进程的ReadList链表，并进行线程切换。其中涉及到不少中断级的限制和同步操作资源获取，与线程相关的操作则大部分与APC有关。

<!--more-->

```

VOID
KeAttachProcess (
    __inout PRKPROCESS Process
    )
{
    KLOCK_QUEUE_HANDLE LockHandle;
    PRKTHREAD Thread;

    ASSERT_PROCESS(Process);
    ASSERT(KeGetCurrentIrql() <= DISPATCH_LEVEL);

    Thread = KeGetCurrentThread();
    if (Thread-> ApcState.Process != Process) {
        if ((Thread-> ApcStateIndex != 0) ||
            (KeIsExecutingDpc() != FALSE)) { 
            KeBugCheckEx(INVALID_PROCESS_ATTACH_ATTEMPT,
                         (ULONG_PTR)Process,
                         (ULONG_PTR)Thread-> ApcState.Process,
                         (ULONG)Thread-> ApcStateIndex,
                         (ULONG)KeIsExecutingDpc());
        }

        KeAcquireInStackQueuedSpinLockRaiseToSynch(&Thread-> ApcQueueLock,
                                                   &LockHandle);
        KiLockDispatcherDatabaseAtSynchLevel();
        KiAttachProcess(Thread, Process, &LockHandle, &Thread-> SavedApcState);
    }
    return;
}

```


```

VOID
KiAttachProcess (
    __inout PRKTHREAD Thread,
    __in PKPROCESS Process,
    __in PKLOCK_QUEUE_HANDLE LockHandle,
    __out PRKAPC_STATE SavedApcState
    )
{
    PLIST_ENTRY NextEntry;
    PRKTHREAD OutThread;

    ASSERT(Process != Thread-> ApcState.Process);
    ASSERT(Process-> StackCount != MAXULONG_PTR);

    Process-> StackCount += 1;

    KiMoveApcState(&Thread-> ApcState, SavedApcState);
    InitializeListHead(&Thread-> ApcState.ApcListHead[KernelMode]);
    InitializeListHead(&Thread-> ApcState.ApcListHead[UserMode]);
    Thread-> ApcState.KernelApcInProgress = FALSE;
    Thread-> ApcState.KernelApcPending  = FALSE;
    Thread-> ApcState.UserApcPending = FALSE;
    if (SavedApcState == &Thread-> SavedApcState) {
        Thread-> ApcStatePointer[0] = &Thread-> SavedApcState;
        Thread-> ApcStatePointer[1] = &Thread-> ApcState;
        Thread-> ApcStateIndex = 1;
    }

    if (Process-> State == ProcessInMemory) {
        Thread-> ApcState.Process = Process;

        NextEntry = Process-> ReadyListHead.Flink;
        while (NextEntry != &Process-> ReadyListHead) {
            OutThread = CONTAINING_RECORD(NextEntry, KTHREAD, WaitListEntry);
            RemoveEntryList(NextEntry);
            OutThread-> ProcessReadyQueue = FALSE;
            KiReadyThread(OutThread);
            NextEntry = Process-> ReadyListHead.Flink;
        }

        KiUnlockDispatcherDatabaseFromSynchLevel();
        KeReleaseInStackQueuedSpinLockFromDpcLevel(LockHandle);
        KiSwapProcess(Process, SavedApcState-> Process);
        KiExitDispatcher(LockHandle-> OldIrql);

    } else {
        Thread-> State = Ready;
        Thread-> ProcessReadyQueue = TRUE;
        InsertTailList(&Process-> ReadyListHead, &Thread-> WaitListEntry);
        if (Process-> State == ProcessOutOfMemory) {
            Process-> State = ProcessInTransition;
            InterlockedPushEntrySingleList(&KiProcessInSwapListHead,
                                           &Process-> SwapListEntry);
            KiSetInternalEvent(&KiSwapEvent, KiSwappingThread);
        }

        Thread-> WaitIrql = LockHandle-> OldIrql;
        KeReleaseInStackQueuedSpinLockFromDpcLevel(LockHandle);
        KiSetContextSwapBusy(Thread);
        KiUnlockDispatcherDatabaseFromSynchLevel();
        KiSwapThread(Thread, KeGetCurrentPrcb());

        KeAcquireInStackQueuedSpinLockRaiseToSynch(&Thread-> ApcQueueLock,
                                                   LockHandle);

        KiLockDispatcherDatabaseAtSynchLevel();
        Thread-> ApcState.Process = Process;
        KiUnlockDispatcherDatabaseFromSynchLevel();
        KeReleaseInStackQueuedSpinLockFromDpcLevel(LockHandle);
        KiSwapProcess(Process, SavedApcState-> Process);
        KiExitDispatcher(LockHandle-> OldIrql);
    }
    return;
}

```
