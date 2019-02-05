
---
title: "PspCidTable简单介绍"
date: 2014-06-05T09:13:30+08:00
draft: true
categories: ["Windows安全"]
tags: ["Windows安全"]
topics: ["Windows安全"]
---

PspCidTable是一个全局句柄表，关于句柄表的信息可以参考我前面一篇文章。它基本符合进程句柄表的所有特质，除了它的成员是对象，而不是对象头。PspCidTable存放的是系统线程和进程对象，包括已经退出的，它的索引就是PID或TID，所以存活的进程和线程的PID和TID是不会重合的。

<!--more-->

因为进程枚举会通过PspCidTable，所以可以通过PspCidTable隐藏进程。可以像分析普通进程句柄表一样分析PspCidTable。我的虚拟机是Win XP sp2。

先看一下它的地址：


```

lkd> x nt!PspCidTable
8055b360 nt!PspCidTable =  <no type information> 

```

在8055b360，句柄表结构是HANDLE_TABLE，dt _handle_table查看它的信息:


```

lkd> dt _handle_table poi(8055b360)
nt!_HANDLE_TABLE
   +0x000 TableCode        : 0xe2713001
   +0x004 QuotaProcess     : (null) 
   +0x008 UniqueProcessId  : (null) 
   +0x00c HandleTableLock  : [4] _EX_PUSH_LOCK
   +0x01c HandleTableList  : _LIST_ENTRY [ 0xe10017b4 - 0xe10017b4 ]
   +0x024 HandleContentionEvent : _EX_PUSH_LOCK
   +0x028 DebugInfo        : (null) 
   +0x02c ExtraInfoPages   : 0n0
   +0x030 FirstFree        : 0x2c0
   +0x034 LastFree         : 0xfdc
   +0x038 NextHandleNeedingPool : 0x1000
   +0x03c HandleCount      : 0n398
   +0x040 Flags            : 1
   +0x040 StrictFIFO       : 0y1

```

可以看到TableCode的最低两位是01，所以是两层句柄表：


```

lkd> dd 0xe2713000
e2713000  e1003000 e2717000 00000000 00000000

```

发现只用到了两个表项。以第一个表项举例：


```

lkd> dqs e1003000
e1003000  fffffffe`00000000
e1003008  00000000`825b9661
e1003010  00000000`825b93e9
e1003018  00000000`825b8021
e1003020  00000000`825b8d21
e1003028  00000000`825b8aa9
e1003030  00000000`825b8831
e1003038  00000000`825b85b9
e1003040  00000000`825b8341
e1003048  00000000`825b7021
e1003050  00000000`825b7da9
e1003058  00000000`825b7b31
e1003060  00000000`825b78b9
e1003068  00000000`825b7641
e1003070  00000000`825b73c9
e1003078  00000000`825b6021

```

随便挑两个看一下：


```

lkd> !object 8234f620
Object: 8234f620  Type: (825b9ca0) Process
    ObjectHeader: 8234f608 (old version)
    HandleCount: 2  PointerCount: 17
lkd> !object 8239eda8
Object: 8239eda8  Type: (825b9ad0) Thread
    ObjectHeader: 8239ed90 (old version)
    HandleCount: 1  PointerCount: 2

```

果然是进程和线程，可以用dt _eprocess和dt _ethread查看具体的信息。

之前看过一个程序，是通过枚举PspCidTable查找所有的EPROCESS结构体的，写的感觉有点儿罗嗦，不过还是实现了功能。


```

#include "ntddk.h"

typedef struct _OBJECT_HEADER {
    union {
        struct {
            LONG PointerCount;
            LONG HandleCount;
        };
        LIST_ENTRY Entry;
    };
    POBJECT_TYPE Type;
    UCHAR NameInfoOffset;
    UCHAR HandleInfoOffset;
    UCHAR QuotaInfoOffset;
    UCHAR Flags;

    union {
        PVOID QuotaBlockCharged;
    };

    PSECURITY_DESCRIPTOR SecurityDescriptor;

    QUAD Body;
} OBJECT_HEADER, *POBJECT_HEADER;

#define OBJECT_TO_OBJECT_HEADER(obj) \
CONTAINING_RECORD( (obj), OBJECT_HEADER, Body )

typedef struct _PROCESS_INFO     
{
	ULONG addr;                  
	int pid;            
	UCHAR name[16];     
	struct _PROCESS_INFO *next;  
}ProcessInfo; 
	
ProcessInfo *head, *p, *q;

ULONG GetPspCidTable()
{
  PUCHAR addr;
  PUCHAR p;
  UNICODE_STRING szNeed;
  ULONG cid;
  RtlInitUnicodeString(&szNeed,
  L"PsLookupProcessByProcessId");
  addr = (PUCHAR)MmGetSystemRoutineAddress(&szNeed);
  for (p=addr; p<addr+PAGE_SIZE; p++)
  {
    if ((*(PUSHORT)p==0x35ff)&&(*(p+6)==0xe8))
    {
      cid = *(PULONG)(p+2);
      return cid;
    }
  }
  return 0;
}

ULONG GetProcessType()
{
  ULONG p;
  ULONG type;
  p = (ULONG)PsGetCurrentProcess();
  p = (ULONG)OBJECT_TO_OBJECT_HEADER(p);
  type = *(PULONG)(p + 0x08);
  return type;
}

VOID Unload(PDRIVER_OBJECT driver)
{
  DbgPrint("Unload!\n");
}

VOID GetInformation(ULONG i)
{
  ProcessInfo *r;
  if (head == NULL)
  {
    if ((head=(ProcessInfo*) ExAllocatePool(NonPagedPool,     sizeof(ProcessInfo)))==NULL)
    {
      return;
    }
    head-> addr = 0x0;
  }
  if (head-> addr == 0)
  {
    head-> addr = i;
    p = head;
  } else
  {
    if ((r=(ProcessInfo*)ExAllocatePool(NonPagedPool,   sizeof(ProcessInfo)))==NULL)
    {
      return;
    }
    p-> next = r;
    r-> addr = i;
    r-> next = NULL;
    p = r;
  }
}

NTSTATUS DriverEntry(PDRIVER_OBJECT driver, 
	PUNICODE_STRING szReg)
{
  ULONG addr, table1, table2, type, processtype;
  ULONG hHandleTable, object;
  ULONG dwTableCode;
  ULONG NextFreeTableEntry;
  ULONG objectheader;
  ULONG flags;
  int i;
  driver-> DriverUnload = Unload;
  addr = GetPspCidTable();
	
  DbgPrint("PspCidTable address: 0x%08x",addr);
  hHandleTable = *(ULONG*)(addr);
  DbgPrint("HANDLE_TABLE : 0x%08x",hHandleTable);
  dwTableCode = *(ULONG*)(hHandleTable);
  DbgPrint("TableCode: 0x%08x\n", dwTableCode);
  if ((dwTableCode & 3)==1)
  {
    dwTableCode = dwTableCode & 0xfffffffc;
    table1 = *(ULONG*)dwTableCode;
    table2 = *(ULONG*)(dwTableCode+4);
  } else
  if ((dwTableCode & 3)==0)
  {
    table1 = dwTableCode;
    table2 = 0;
  }
  DbgPrint("table1: 0x%08x,     table2: 0x%08x\n",table1,table2);
	
  processtype = GetProcessType();
  DbgPrint("type: 0x%08x\n", processtype);
	
  for (i = 0; i <= 0x800; i++)
  {
    if (MmIsAddressValid((PULONG)(table1 + i*2)))
    {
      object = *(PULONG)(table1+2*i);
      if (MmIsAddressValid((PULONG)   (table1 + i*2 + 0x4)))
      {
        NextFreeTableEntry = *(PULONG)(table1 + i*2 + 0x4);
	if (NextFreeTableEntry == 0)
	{
	  object = ((object | 0x80000000)&0xfffffff8);
	  objectheader = (ULONG)     OBJECT_TO_OBJECT_HEADER(object);
	  if (MmIsAddressValid((PULONG)(objectheader+0x08)))
	  {
	    type = *(PULONG)(objectheader+0x08);
	    if (type == processtype)
	    {
	      flags = *(PULONG)((ULONG)object + 0x248);
	      if ((flags&0xC)!=0xC)
	        GetInformation(object);
	    }
	  }
	}
      }
    }
  }
  for (i = 0x801; i<0x4e1c; i++)
  {
    if (MmIsAddressValid((PULONG) (table2 + (i-0x800)*2)))
    {
      object = *(PULONG)(table2 + (i-0x800)*2);
    }
  }
  for (p = head; p; p=p-> next)
  {
    p-> pid = *(int*)(p-> addr + 0x84);
    strcpy(p-> name, (UCHAR*)(p-> addr + 0x174));
  }
  for (p = head; p; p=p-> next)
  {
    DbgPrint("PROCESS 0x%08x,%4d,%s\n",  p-> addr, p-> pid, p-> name);
  }
  p=head;
  q = p-> next;
  while (q!=NULL)
  {
    ExFreePool(p);
    p = q;
    q = p-> next;
  }
  ExFreePool(p);
  return STATUS_SUCCESS;
}

```


解释一下0xC代表什么，0xC是EPROCESS的Flags成员，WRK的ps.h可以看到


```

#define PS_PROCESS_FLAGS_PROCESS_EXITING 0x00000004UL // PspExitProcess entered
#define PS_PROCESS_FLAGS_PROCESS_DELETE 0x00000008UL // Delete process has been issued

```


所以意义不言而喻。
