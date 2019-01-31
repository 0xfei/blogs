
---
title: "Windows内核定时器"
date: 2014-06-04T09:13:30+08:00
draft: true
categories: ["Windows安全"]
tags: ["Windows安全"]
topics: ["Windows安全"]
---

常用的内核定时器有两种：IoInitializeTimer、IoStartTimer和IoStopTimer三个函数的IO定时器；KeInitializeTimer、KeSetTimer和KeCancelTimer三个函数的DPC定时器。前一套更简单，后一套功能更强大一些。

## IO定时器

IO定时器每隔1s触发一次自己定义的IO例程，可以用来做一些需要特定频率的事情。Windbg下看一下IO定时器三个函数的实现，比较简单。看一下函数原型和必要的结构。

<!--more-->

```

lkd> dt _io_timer
nt!_IO_TIMER
   +0x000 Type             : Int2B
   +0x002 TimerFlag        : Int2B
   +0x004 TimerList        : _LIST_ENTRY
   +0x00c TimerRoutine     : Ptr32     void 
   +0x010 Context          : Ptr32 Void
   +0x014 DeviceObject     : Ptr32 _DEVICE_OBJECT

NTSTATUS  IoInitializeTimer( 
    IN PDEVICE_OBJECT  DeviceObject, 
    IN PIO_TIMER_ROUTINE  TimerRoutine, 
    IN PVOID  Context 
    );


```



```

lkd> uf IoInitializeTimer
nt!IoInitializeTimer:
8056a3da 8bff            mov     edi,edi
8056a3dc 55              push    ebp
8056a3dd 8bec            mov     ebp,esp
8056a3df 56              push    esi
8056a3e0 8b7508          mov     esi,dword ptr [ebp+8]
// 第一个参数，设备对象
8056a3e3 8b5618          mov     edx,dword ptr [esi+18h]
// DEVICE_OBJECT偏移18h处是 +0x018 Timer : Ptr32 _IO_TIMER
8056a3e6 85d2            test    edx,edx
8056a3e8 7530            jne     nt!IoInitializeTimer+0x40 (8056a41a)
// 这里说明每个对象只能有一个IO定时器

nt!IoInitializeTimer+0x10:
8056a3ea 68496f5469      push    69546F49h
8056a3ef 6a18            push    18h
8056a3f1 52              push    edx
8056a3f2 e889bbfdff      call    nt!ExAllocatePoolWithTag (80545f80)
8056a3f7 8bd0            mov     edx,eax
8056a3f9 85d2            test    edx,edx
8056a3fb 7507            jne     nt!IoInitializeTimer+0x2a (8056a404)
// 分配内存并初始化定时器

nt!IoInitializeTimer+0x23:
8056a3fd b89a0000c0      mov     eax,0C000009Ah
8056a402 eb36            jmp     nt!IoInitializeTimer+0x60 (8056a43a)

nt!IoInitializeTimer+0x2a:
8056a404 57              push    edi
8056a405 6a06            push    6
8056a407 59              pop     ecx
8056a408 33c0            xor     eax,eax
8056a40a 8bfa            mov     edi,edx
8056a40c f3ab            rep stos dword ptr es:[edi]
8056a40e 66c7020900      mov     word ptr [edx],9
8056a413 897214          mov     dword ptr [edx+14h],esi
8056a416 895618          mov     dword ptr [esi+18h],edx
8056a419 5f              pop     edi
// esi指向设备对象，这里对设备对象的Timer进行赋值同时进一步初始化Timer

nt!IoInitializeTimer+0x40:
8056a41a 8b450c          mov     eax,dword ptr [ebp+0Ch]
8056a41d 89420c          mov     dword ptr [edx+0Ch],eax
8056a420 8b4510          mov     eax,dword ptr [ebp+10h]
8056a423 894210          mov     dword ptr [edx+10h],eax
8056a426 6844be5480      push    offset nt!IopTimerLock (8054be44)
8056a42b 83c204          add     edx,4
8056a42e b960295580      mov     ecx,offset nt!IopTimerQueueHead (80552960)
8056a433 e8d092fdff      call    nt!ExfInterlockedInsertTailList (80543708)
8056a438 33c0            xor     eax,eax
// 将IO定时器上锁并加入队列，ExfInterlockedInsertTailList实现下面

nt!IoInitializeTimer+0x60:
8056a43a 5e              pop     esi
8056a43b 5d              pop     ebp
8056a43c c20c00          ret     0Ch


```


ExfInterlockedInsertTailList其实就是将Timer加入一个链表中，这里有一个全局变量IopTimerQueueHead，是链表头。


```

lkd> uf ExfInterlockedInsertTailList
nt!ExfInterlockedInsertTailList:
80543708 9c              pushfd
80543709 fa              cli
8054370a 8b4104          mov     eax,dword ptr [ecx+4]
8054370d 890a            mov     dword ptr [edx],ecx
8054370f 894204          mov     dword ptr [edx+4],eax
80543712 895104          mov     dword ptr [ecx+4],edx
80543715 8910            mov     dword ptr [eax],edx
80543717 9d              popfd
80543718 33c1            xor     eax,ecx
8054371a 7402            je      nt!ExfInterlockedInsertTailList+0x16 (8054371e)

nt!ExfInterlockedInsertTailList+0x14:
8054371c 33c1            xor     eax,ecx

nt!ExfInterlockedInsertTailList+0x16:
8054371e c20400          ret     4

```


可以看出定时器的创建很简单。下面看一下IoStartTimer和IoStopTimer。


```

VOID  IoStartTimer(
    IN PDEVICE_OBJECT  DeviceObject
    );
VOID  IoStopTimer(
    IN PDEVICE_OBJECT  DeviceObject
    );

```



```

lkd> uf IoStartTimer
nt!IoStartTimer:
804f0314 8bff            mov     edi,edi
804f0316 55              push    ebp
804f0317 8bec            mov     ebp,esp
804f0319 8b4d08          mov     ecx,dword ptr [ebp+8]
804f031c 8b4118          mov     eax,dword ptr [ecx+18h]
// 获取设备的Timer成员
804f031f 8b89b0000000    mov     ecx,dword ptr [ecx+0B0h]
// 设备对象B0h偏移处成员是DeviceObjectExtension设备扩展
804f0325 f641100f        test    byte ptr [ecx+10h],0Fh
804f0329 7515            jne     nt!IoStartTimer+0x2c (804f0340)

nt!IoStartTimer+0x17:
804f032b fa              cli
804f032c 6683780200      cmp     word ptr [eax+2],0
804f0331 750c            jne     nt!IoStartTimer+0x2b (804f033f)

nt!IoStartTimer+0x1f:
804f0333 66c740020100    mov     word ptr [eax+2],1
804f0339 ff05f4285580    inc     dword ptr [nt!IopTimerCount (805528f4)]
// 设置Timer的TimerFlag
nt!IoStartTimer+0x2b:
804f033f fb              sti

nt!IoStartTimer+0x2c:
804f0340 5d              pop     ebp
804f0341 c20400          ret     4

```



```

lkd> uf IoStopTimer
nt!IoStopTimer:
804f034a 8bff            mov     edi,edi
804f034c 55              push    ebp
804f034d 8bec            mov     ebp,esp
804f034f 8b4508          mov     eax,dword ptr [ebp+8]
804f0352 8b4018          mov     eax,dword ptr [eax+18h]
804f0355 fa              cli
804f0356 6683780200      cmp     word ptr [eax+2],0
804f035b 740b            je      nt!IoStopTimer+0x1e (804f0368)

nt!IoStopTimer+0x13:
804f035d 6683600200      and     word ptr [eax+2],0
804f0362 ff0df4285580    dec     dword ptr [nt!IopTimerCount (805528f4)]
// 和IoStartTimer类似

nt!IoStopTimer+0x1e:
804f0368 fb              sti
804f0369 5d              pop     ebp
804f036a c20400          ret     4

```


再给出一个样例代码：


```

#include <ntddk.h> 

typedef struct _WORK
{
	PIO_WORKITEM pIoWorkItem;
	LONG  nWorkNumber; 
}WORK,*PWORK;

char  g_fTimerStarted;
LONG g_nWorkToDo;

UNICODE_STRING	 g_usDeviceName;
UNICODE_STRING	g_usSymbolicLinkName;

void WorkItemRoutine(PDEVICE_OBJECT pDeviceObject, PVOID pContext)
{
	PWORK pWork; 

	pWork = (PWORK)pContext;
	DbgPrint("WorkItem: Work #%d is done\r\n", pWork-> nWorkNumber);
	IoFreeWorkItem(pWork-> pIoWorkItem);
	ExFreePool(pWork);
}

void TimerRoutine(struct _DEVICE_OBJECT *pDeviceObject, PVOID pContext)
{
  PWORK pWork; 
  PIO_WORKITEM IoWorkItem; 
  if ( g_nWorkToDo )
  {
    IoWorkItem = IoAllocateWorkItem(pDeviceObject);
    if ( IoWorkItem )
    {
      pWork = ExAllocatePool(0, sizeof(WORK));
      if ( pWork != NULL )
      {
        pWork-> pIoWorkItem = IoWorkItem;
	pWork-> nWorkNumber = g_nWorkToDo;
	IoQueueWorkItem(IoWorkItem, WorkItemRoutine,       DelayedWorkQueue, pWork);
	--g_nWorkToDo;
      }
    }
  }
  else
  {
    IoStopTimer(pDeviceObject);
    g_fTimerStarted = FALSE;
  }
}

void DriverUnload(PDRIVER_OBJECT DriverObject)
{
  IoDeleteSymbolicLink(&g_usSymbolicLinkName);
  if ( g_fTimerStarted )
  {
    IoStopTimer(DriverObject-> DeviceObject);
    g_fTimerStarted = FALSE;
    DbgPrint("WorkItem: Timer stopped\r\n");
  }
  IoDeleteDevice(DriverObject-> DeviceObject);
}

NTSTATUS DriverEntry(PDRIVER_OBJECT DriverObject, PUNICODE_STRING RegistryPath)
{
  NTSTATUS status; 
  PDEVICE_OBJECT DeviceObject; 

  RtlInitUnicodeString(&g_usDeviceName,L"\\Device\\IoTimer");
  RtlInitUnicodeString(&g_usSymbolicLinkName, L"\\DosDevices\\IoTimer");

  g_fTimerStarted = FALSE;
  g_nWorkToDo = 5;

  IoCreateDevice(DriverObject, 0, &g_usDeviceName,   FILE_DEVICE_UNKNOWN, 0, TRUE, &DeviceObject);
  status = IoCreateSymbolicLink(&g_usSymbolicLinkName, &g_usDeviceName);
  if ( status )
  {
    DbgPrint("WorkItem: Couldn't create device. Status: %08X\r\n", status);
    IoDeleteDevice(DeviceObject);
  }
  else
  {
    DriverObject-> DriverUnload = (PDRIVER_UNLOAD)DriverUnload;
    status = IoInitializeTimer(DeviceObject, TimerRoutine, 0);
    if ( status != STATUS_SUCCESS)
    {
      DbgPrint("WorkItem: Couldn't initialize timer. \     Status: %08X\r\n", status);
      IoDeleteSymbolicLink(&g_usSymbolicLinkName);
      IoDeleteDevice(DeviceObject);
    }
    else
    {
      IoStartTimer(DeviceObject);
      g_fTimerStarted = 1;
      DbgPrint("WorkItem: Timer started\r\n");
      status = 0;
    }
  }
  return status;
}

```


## DPC定时器

DPC定时器可以实现更精确的时间控制，时间粒度也可以更小，100ns为单位。看一下三个函数原型：


```

VOID  KeInitializeTimerEx(
    IN PKTIMER  Timer,
    IN TIMER_TYPE  Type
    );

typedef enum _TIMER_TYPE {
    NotificationTimer,
    SynchronizationTimer
    } TIMER_TYPE;

typedef struct _KTIMER {
    DISPATCHER_HEADER32  Header;
    ULARGE_INTEGER DueTime;
    LIST_ENTRY TimerListEntry;
    struct _KDPC *Dpc;
    LONG Period;
} KTIMER, *PKTIMER, *PRKTIMER;

BOOLEAN   KeCancelTimer( 
    IN PKTIMER  Timer 
    ); 

BOOLEAN   KeSetTimerEx( 
    IN PKTIMER  Timer, 
    IN LARGE_INTEGER  DueTime, 
    IN LONG  Period  OPTIONAL, 
    IN PKDPC  Dpc  OPTIONAL 
    );


```


可以看到DPC定时器更偏向**全局**，KTIMER是分发器对象（有分发器头的都是分发器对象），可以用KeWaitForSingleObject等函数等待。KeSetTimerEx可以自己设置事件触发定时器，如果指定DPC，那么相应的DPC例程会被执行。


```

ThreadProc proc Param:DWORD
  local dwCounter:DWORD
  local pkThread:PVOID
  local status:NTSTATUS
  local kTimer:KTIMER
  local liDueTime:LARGE_INTEGER
	
  and dwCounter, 0
  invoke DbgPrint, $CTA0("\nTimerWorks: Entering ThreadProc\n")
  invoke KeGetCurrentIrql
  invoke DbgPrint, $CTA0("TimerWorks: IRQL = %d\n"), eax
  invoke KeGetCurrentThread
  mov pkThread, eax
  invoke KeQueryPriorityThread, eax
  push eax
  invoke DbgPrint, $CTA0("TimerWorks: Thread Priority = %d\n"), eax
  pop eax
  inc eax
  inc eax
  invoke KeSetPriorityThread, pkThread, eax
  invoke KeQueryPriorityThread, pkThread
  invoke DbgPrint, $CTA0("TimerWorks: Thread Priority = %d\n"), eax
  invoke KeInitializeTimerEx, addr kTimer, SynchronizationTimer
  or liDueTime.HighPart, -1
  mov liDueTime.LowPart, -50000000
  invoke KeSetTimerEx, addr kTimer, liDueTime.LowPart,\
    liDueTime.HighPart, 1000, NULL
  invoke DbgPrint, $CTA0("TimerWorks: Timer is set. \
    It starts counting in 5 seconds...\n")
  .while dwCounter < 10
    invoke KeWaitForSingleObject, addr kTimer, Executive,\ KernelMode, FALSE, NULL
    inc dwCounter
    invoke DbgPrint, $CTA0("TimerWorks: Counter = %d\n"), dwCounter
    .if g_fStop
    invoke DbgPrint, $CTA0("TimerWorks: \   Stop counting to let the driver to be uloaded\n")
    .break
    .endif
  .endw
  invoke KeCancelTimer, addr kTimer
  invoke DbgPrint, $CTA0("TimerWorks: Timer is canceled. Leaving ThreadProc\n")
  invoke DbgPrint, $CTA0("TimerWorks: Our thread is about to terminate\n")
  invoke PsTerminateSystemThread, STATUS_SUCCESS
  ret
ThreadProc endp


StartThread proc
  local status:NTSTATUS
  local oa:OBJECT_ATTRIBUTES
  local hThread:HANDLE
  invoke DbgPrint, $CTA0("\nTimerWorks: Entering StartThread\n")
  invoke PsCreateSystemThread, addr hThread, \
    THREAD_ALL_ACCESS, NULL, NULL, NULL, ThreadProc, NULL
  mov status, eax
  .if eax == STATUS_SUCCESS
    invoke ObReferenceObjectByHandle, hThread, \  THREAD_ALL_ACCESS, NULL, KernelMode, addr g_pkThread, NULL
    invoke ZwClose, hThread
    invoke DbgPrint, $CTA0("TimerWorks: Thread created\n")
  .else
    invoke DbgPrint, $CTA0("TimerWorks: Can't create Thread. Status: %08X\n"), eax
  .endif
    invoke DbgPrint, $CTA0("TimerWorks: Leaving StartThread\n")
    mov eax, status
    ret
StartThread endp

```


