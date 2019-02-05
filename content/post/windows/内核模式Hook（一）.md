
---
title: "内核模式Hook（一）"
date: 2014-04-27T09:13:30+08:00
draft: true
categories: ["Windows安全"]
tags: ["Windows安全"]
topics: ["Windows安全"]
---

上回提到过，用户模式Hook功能有限，而且容易被发现。到了ring0，Hook仍然是很多技术的基础。这里简单介绍几种常见的内核hook：SSDT Hook、IDT Hook、SYSENTER Hook、IAT Hook、导出表Hook、IRP Hook、Inline Hook和几种门的添加。这并不是很严格的分类方式，各种技术也可能会交叉灵活使用。

<!--more-->

## SSDT Hook
系统服务调度程序KiSystemService利用传入的函数ID号，在SSDT即系统服务调度表，定位函数内存地址；和SSDT相关的另一个表是SSPT，系统服务参数表，指定每个函数的参数信息。KeServiceDescriptorTable是内核导出的全局变量，包括了指向上述两个表的指针以及系统服务个数。


```

#pragma pack(1)
typedef struct ServiceDescriptorEntry {
	unsigned int *ServiceTableBase;
	unsigned int *ServiceCounterTableBase;
	unsigned int NumberOfServices;
	unsigned char *ParamTableBae;
} SSDT_ENTRY;
#pragma pack()
__declspec(dllimport) SSDT_ENTRY KeServiceDescriptorTable;


```

SSDT所在地址是只读的，所以需要进行权限修改，可以将CR0中的写保护位删除，也可以使用MDL，这里介绍MDL（内存描述符表）。MDL用于描述一块内存区域，包括它的起始地址、大小、所属进程等。


```

lkd> dt _mdl
nt!_MDL
   +0x000 Next             : Ptr32 _MDL
   +0x004 Size             : Int2B
   +0x006 MdlFlags         : Int2B
   +0x008 Process          : Ptr32 _EPROCESS
   +0x00c MappedSystemVa   : Ptr32 Void
   +0x010 StartVa          : Ptr32 Void
   +0x014 ByteCount        : Uint4B
   +0x018 ByteOffset       : Uint4B


```

只要将内存标志的MDL_MAPPED_TO_SYSTEM_VA设置为1，就可以对该内存区域进行写操作，所以经典的利用MDL修改内存属性操作如下：


```

NTSTATUS MAKEMyMDL()
{
	MyMDL = MmCreateMdl(
		NULL, 
		KeServiceDescriptorTable.ServiceTableBase, 
		KeServiceDescriptorTable.NumberOfServices*4);
	if (!MyMDL) return STATUS_UNSUCCESSFUL;

	MmBuildMdlForNonPagedPool(MyMDL);
	MyMDL-> MdlFlags |= MDL_MAPPED_TO_SYSTEM_VA;
	NewServiceDescriptorTable = MmMapLockedPages(MyMDL, KernelMode);
	return STATUS_SUCCESS;
}


```

这样就可以修改NewServiceDescriptorTable来修改原来SSDT中的函数，这里我们利用SSDT Hook函数NtQuerySystemInformation来隐藏进程。NtQuerySystemInformation实际上是由ZwQuerySystemInformation调用的，而Zw*函数都很有特点，看一下：


```

lkd> uf ZwQuerySystemInformation
nt!ZwQuerySystemInformation:
804ffb2c b8ad000000      mov     eax,0ADh
804ffb31 8d542404        lea     edx,[esp+4]
804ffb35 9c              pushfd
804ffb36 6a08            push    8
804ffb38 e8e4e90300      call    nt!KiSystemService (8053e521)
804ffb3d c21000          ret     10h


```

这个函数开头第2个字节就是对应的Nt同名函数在SSDT中的索引：


```

lkd> dd KeServiceDescriptorTable
805540a0  80502bbc 00000000 0000011c 80503030
805540b0  00000000 00000000 00000000 00000000
805540c0  00000000 00000000 00000000 00000000
805540d0  00000000 00000000 00000000 00000000
805540e0  00002710 bf80c0b6 00000000 00000000
805540f0  f8b97a80 f80adb60 822fd348 806e3f40
80554100  00000000 00000000 45ccb1e0 00000380
80554110  bffafa20 01cf60e4 00000000 00000000


```


```


lkd> dds 80502bbc l(AD+1)
80502bbc  8059aa74 nt!NtAcceptConnectPort
80502bc0  805e8820 nt!NtAccessCheck
80502bc4  805ec066 nt!NtAccessCheckAndAuditAlarm
80502bc8  805e8852 nt!NtAccessCheckByType
编号从0开始，所以需要AD+1项
80502e60  8060cd6c nt!NtQuerySemaphore
80502e64  805ba96e nt!NtQuerySymbolicLinkObject
80502e68  8060e5be nt!NtQuerySystemEnvironmentValue
80502e6c  8060e586 nt!NtQuerySystemEnvironmentValueEx
80502e70  806095a8 nt!NtQuerySystemInformation


```

所以有几个宏可以方便的进行Hook和Unhook，其中利用了InterlockedExchange原子交换函数。


```

#define SYSTEMSERVICE(func) \
	KeServiceDescriptorTable.ServiceTableBase[ *(PULONG)((PUCHAR)func + 1)]
#define SYSTEMINDEX(func) \
	*(PULONG)((PUCHAR)func + 1)
#define HOOKFUNC(func, new, old) \
	old = (PVOID)InterlockedExchange( (PULONG) \
	&NewServiceDescriptorTable[SYSTEMINDEX(func)], (ULONG)new)
#define UNHOOKFUNC(func, old) \
	InterlockedExchange( (PULONG) \
	&NewServiceDescriptorTable[SYSTEMINDEX(func)], (ULONG)old)


```

ZwQuerySystemInformation可以查询很多进程信息，包括链接了所有进程的一个LIST。只要将SSDT中该函数的地址替换成我们自己的函数地址，然后先调用原本的NtQuerySystemInformation得到进程信息，再进行处理后返回，达到隐藏进程的目的。用到的一些参数需要自己定义。部分代码如下：


```

NTSTATUS NewFunc(
	ULONG SystemInformationClass,
	PVOID SystemInformation,
	ULONG SystemInformationLength,
	PULONG ReturnLength
	)
{
  NTSTATUS status;
  status = OldFunc(
	SystemInformationClass,
	SystemInformation,
	SystemInformationLength,
	ReturnLength );
  if (NT_SUCCESS(status))
  {
    if (SystemInformationClass == 5)
    {
	PSYSTEM_PROCESSES now = (PSYSTEM_PROCESSES)SystemInformation;
	PSYSTEM_PROCESSES prev = NULL;
	while (now)
	{
          if (memcmp(now-> ProcessName.Buffer, L"notepad.exe",      now-> ProcessName.Length) == 0)
	  {
		UserTime.QuadPart += now-> UserTime.QuadPart;
		KernelTime.QuadPart += now-> KernelTime.QuadPart;
		if (prev)
		{
			if (now-> NextEntryDelta)
				prev-> NextEntryDelta += now-> NextEntryDelta;
			else
				prev-> NextEntryDelta = 0;
			}
			else
			{
				if (now-> NextEntryDelta)
				(char*)SystemInformation += now-> NextEntryDelta;
				else
					SystemInformation = NULL;
			}
		}
		else if (now-> ProcessName.Buffer == NULL)
		{
			now-> UserTime.QuadPart += UserTime.QuadPart;
			now-> KernelTime.QuadPart += KernelTime.QuadPart;
		}
		prev = now;
		if (now-> NextEntryDelta) (char*)now += now-> NextEntryDelta;
		else now = NULL;
	}
      }
     else if (SystemInformationClass == 8)
     {
	PSYSTEM_PROCESSOR_TIMES times = (PSYSTEM_PROCESSOR_TIMES)SystemInformation;
	times-> IdleTime.QuadPart += UserTime.QuadPart + KernelTime.QuadPart;
     }
   }
   return status;
}


```

这样打开notpad然后再打开任务管理器，发现notepad被隐藏。


## IDT Hook
IDT即中断描述符表，用于处理中断。当中断发生时系统在IDT中查找中断处理函数的地址。IDT在内存中的地址由寄存器IDTR保存，SIDT指令可以读取IDTR的内容，返回一个结构体：


```

typedef struct _IDTINFO
{
	WORD IDTLimit;
	WORD LowIDTBase;
	WORD HiIDTBase;
} IDTINFO;


```

表示IDT的地址和范围，IDT中的每一项也是一个特殊的结构体类型：


```

#pragma pack(1)
typedef struct _IDTENTRY
{
	WORD LowOffset;
	WORD selector;
	BYTE unused_lo;
	unsigned char unused_hi:5;
	unsigned char DPL:2;
	unsigned char P:1;
	WORD HiOffset;
} IDTENTRY;
#pragma pack()


```

包含中断处理程序的地址、是否有效（P位）以及描述符权限级DPL。系统处理中断时会对RPL、DPL和CPL进行一系列判断，决定是否可以执行该中断函数。可以在windbg中查看IDT的信息：


```

lkd> !pcr
KPCR for Processor 0 at ffdff000:
    Major 1 Minor 1
	NtTib.ExceptionList: b1be3c7c
	    NtTib.StackBase: b1be3df0
	   NtTib.StackLimit: b1be1000
	 NtTib.SubSystemTib: 00000000
	      NtTib.Version: 00000000
	  NtTib.UserPointer: 00000000
	      NtTib.SelfTib: 7ffde000

	            SelfPcr: ffdff000
	               Prcb: ffdff120
	               Irql: 00000000
	                IRR: 00000000
	                IDR: ffffffff
	      InterruptMode: 00000000
	                IDT: 8003f400
	                GDT: 8003f000
	                TSS: 80042000

	      CurrentThread: 8203d4b8
	         NextThread: 00000000
	         IdleThread: 80553840

	          DpcQueue: 
lkd> dqs 8003f400
8003f400  80538e00`0008f23c
8003f408  80538e00`0008f3b4
8003f410  00008500`0058113e
8003f418  8053ee00`0008f784
8003f420  8053ee00`0008f904
8003f428  80538e00`0008fa60
8003f430  80538e00`0008fbd4
8003f438  80548e00`0008023c
8003f440  00008500`00501198
8003f448  80548e00`00080660
8003f450  80548e00`00080780
8003f458  80548e00`000808c0
8003f460  80548e00`00080b1c
8003f468  80548e00`00080e00
8003f470  80548e00`00081508
8003f478  80548e00`00081838
lkd> u 8053f23c
nt!KiTrap00:
8053f23c 6a00            push    0
8053f23e 66c74424020000  mov     word ptr [esp+2],0
8053f245 55              push    ebp
8053f246 53              push    ebx
8053f247 56              push    esi
8053f248 57              push    edi
8053f249 0fa0            push    fs
8053f24b bb30000000      mov     ebx,30h

```

IDT Hook就是要更改每个IDT项的地址偏移。这里Hook IDT之后可以统计每个ISR被调用的次数，使用了一个函数模板，对不同的IDT项替换掉某些信息。


```

IDTINFO IdtInfo;
IDTENTRY *IdtEntries;
DWORD old[256];
DWORD count[256];
BYTE* tables;

#define MIN_IDT 0
#define MAX_IDT 0xFF

char template[] = {
	0x90,                    //nop, debug
	0x60,                    //pushad
	0x9C,                    //pushfd
	0xB8, 0xAA, 0x00, 0x00, 0x00,            //mov eax, AAh
	0x50,                    //push eax
	0x9A, 0x11, 0x22, 0x33, 0x44, 0x08, 0x00,        //call 08:44332211h
	0x58,                    //pop eax
	0x9D,                    //popfd
	0x61,                    //popad
	0xEA, 0x11, 0x22, 0x33, 0x44, 0x08, 0x00          //jmp 08:44332211h
};

void __stdcall NewISR(DWORD nouse)
{
	unsigned long *index;
	unsigned long i;

	__asm mov eax,[ebp+0Ch]
	__asm mov i, eax

	i = i & 0xFF;
	index = &count[i];
	InterlockedIncrement(index);
}

void HookIDT()
{
	int i, offset = 0;
	char* entry;
	for (i = MIN_IDT; i < MAX_IDT; i++)
	{
		old[i] = MAKELONG(IdtEntries[i].LowOffset, IdtEntries[i].HiOffset);
		entry = tables + offset;
		memcpy(entry, template, sizeof(template));
		entry[4] = (BYTE)i;
		*((DWORD*)(&entry[10])) = (DWORD)NewISR;
		*((DWORD*)(&entry[20])) = (DWORD)old[i];
		__asm cli
		IdtEntries[i].LowOffset = (WORD)entry;
		IdtEntries[i].HiOffset = (WORD)((DWORD)entry > > 16);
		__asm sti
		offset += sizeof(template);
	}
}

```

## SYSENTER Hook
sysenter指令实现用户模式快速调用系统调度函数，代替了以前的int 2e中断。SYSENTER利用一组特殊的寄存器MSR实现用户模式向内核模式切换，IA32_SYSENTER_EIP保存了切换之后调用的地址，该寄存器编号是0x176，使用rdmsr和wrmsr可以读写MSR寄存器。


```

lkd> rdmsr 0x176
msr[176] = 00000000`8053e5e0
lkd> u 8053e5e0
nt!KiFastCallEntry:
8053e5e0 b923000000      mov     ecx,23h
8053e5e5 6a30            push    30h
8053e5e7 0fa1            pop     fs
8053e5e9 8ed9            mov     ds,cx
8053e5eb 8ec1            mov     es,cx
8053e5ed 8b0d40f0dfff    mov     ecx,dword ptr ds:[0FFDFF040h]
8053e5f3 8b6104          mov     esp,dword ptr [ecx+4]
8053e5f6 6a23            push    23h


```


```

VOID Unload(PDRIVER_OBJECT driver )
{
	_asm
	{
		mov ecx, 0x176
		xor edx,edx
		mov eax, Old
		wrmsr
	}
}


__declspec(naked) MyKiFastCallEntry()
{
	_asm jmp [OldFunc]
}

NTSTATUS DriverEntry(PDRIVER_OBJECT driver, IN PUNICODE_STRING szReg )
{
	driver-> DriverUnload  = Unload; 

	_asm {
		mov ecx, 0x176
		rdmsr
		mov Old, eax
		mov eax, MyKiFastCallEntry
		wrmsr
	}

	return STATUS_SUCCESS;
}


```


