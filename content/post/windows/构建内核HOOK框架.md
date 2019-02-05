---
title: "构建内核HOOK框架"
date: 2015-01-29T09:13:30+08:00
draft: true
categories: ["Windows安全"]
tags: ["Windows安全"]
topics: ["Windows安全"]
---

分析漏洞或者提取病毒行为，调试器是必不可少的，不过面对rootkit或者其它有驱动辅助的程序，OD或者windbg硬上可能会蓝屏，驱动级的anti还是不容小觑的；此外不少企业或者团队还希望有自己的模拟器，可以去半自动跑一些样本。不管怎么说，内核HOOK框架都是必不可少的。

内核HOOK需要关注的类型并不多：SSDT、ShadowSSDT、IDT内核关口HOOK；文件系统过滤，可以自建控制设备和卷设备过滤，也可以使用MiniFilter；网络控制，TDI/WPF驱动必不可少，甚至需要自己的NDIS驱动；重点当然是内核入口，现在的主流是KiFastCallEntry，我们也是处理了这个点。

<!--more-->

单纯的HOOK这些点以及加一些过滤是没有意义的，重要的还是人为地参与和控制。首先需要重载内核，即加载磁盘文件，而不使用系统正在使用的内核，考虑到磁盘文件可能也有问题，可以根据操作系统版本号加载自己的文件；调试框架也需要自己重新构建，最好自己定义结构采集进程信息，仿造windows的DebugPort；文件和注册表重定位，这个是比较重要的，也是记录行为的关键；驱动和应用层通信，通信可以有很多种方式，可以简单实用DeviceIoControl。

作为一个HOOK框架，要定义好一些回掉函数，由另外的驱动来实现功能并向我们的HOOK框架注册。下面详细说一下使用的技术和实现细节。


### 内核入口

内核入口实现ring3调用到ring0实现功能。Ring3的所有调用，只要经过ntdll，都会调用ntdll.KiFastSystemCall：

```
lkd> uf ntdll!ZwCreateFile	
ntdll!NtCreateFile:	7c92d0ae b825000000      
mov     eax,25h	7c92d0b3 ba0003fe7f      
mov     edx,offset SharedUserData!SystemCallStub (7ffe0300)	7c92d0b8 ff12             
call    dword ptr [edx]	7c92d0ba c22c00           
ret     2Ch
```

这个SharedUserData是可以内核层和应用层都可以访问的区域，在应用层被映射到虚拟地址7FFE0000，内核层映射为FFDF0000：

```
lkd> dt _KUSER_SHARED_DATA		
ntdll!_KUSER_SHARED_DATA		   
+0x300 SystemCall       : Uint4B		   
+0x304 SystemCallReturn : Uint4B
```

这个结构在系统启动的时候被初始化，SystemCall和SystemCallReturn被指定。Ring0和ring3都可以访问这片区域，所以可以通过上述两个函数实现环境切换。

```
ntdll!KiFastSystemCall:
	7c958458 8bd4            mov     edx,esp
	7c95845a 0f34            sysenter
	7c95845c c3              ret 
```

eax保存SSDT中的索引，edx指向ring3栈基址。Sysenter使用几个MSR特殊寄存器设置EIP、CS、ESP、SS等寄存器，这时进入内核。IA32_SYSENTER_EIP指定执行的地址。正是KiFastCallEntry。KiFastCallEntry实现具体的服务例程分发，即调用eax索引指定的SSDT中的函数。

所以KiFastCallEntry是所有应用层调用必经入口，我们只要在这里实现HOOK，就可以拦截所有调用（理论上）。

```
kd> u 8053e614
nt!KiFastCallEntry+0xd4:
8053e614 8b5f0c          mov     ebx,dword ptr [edi+0Ch]
8053e617 33c9            xor     ecx,ecx
8053e619 8a0c18          mov     cl,byte ptr [eax+ebx]
8053e61c 8b3f            mov     edi,dword ptr [edi]
8053e61e 8b1c87          mov     ebx,dword ptr [edi+eax*4]
8053e621 2be1            sub     esp,ecx
8053e623 c1e902          shr     ecx,2
8053e626 8bfc            mov     edi,esp
```

恰好五个字节，可以做个jmp。这时我们已经控制了调用的入口。

### SSDT和ShadowSSDT

前面提到KiFastCallEntry用于实现系统服务分发，nt导出一个全局变量（64位的不再导出）KeServiceDescriptorTable，它类似如下结构：

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

Windbg可以看具体的内容：

```
kd> x nt!KeSer*
80553f60 nt!KeServiceDescriptorTableShadow = <no type information>
80553fa0 nt!KeServiceDescriptorTable = <no type information>
kd> dd 80553fa0 l4
80553fa0  80502b8c 00000000 0000011c 80503000
kd> dds 80502b8c l5
80502b8c  8059a948 nt!NtAcceptConnectPort
80502b90  805e7db6 nt!NtAccessCheck
80502b94  805eb5fc nt!NtAccessCheckAndAuditAlarm
80502b98  805e7de8 nt!NtAccessCheckByType
80502b9c  805eb636 nt!NtAccessCheckByTypeAndAuditAlarm
```

这正是我们需要重点关注的内容，大量的文件、注册表、进程、线程等操作都会调用SSDT中的函数。内核线程结构KTHREAD的ServiceTable成员指向KeServiceDescriptorTable。另一个与SSDT类似的结构是ShadowSSDT。

Windows的图形操作由子系统实现，内核部分表现为win32.sys，实现窗口管理和GDI调用，它导出的Shadow SSDT表向用户层提供系统服务，只有GUI线程才用得到，而GUI线程的ServiceTable指向KeServiceDescriptorTableShadow，所以要HOOK这个结构并不像SSDT那么直接，因为调用DriverEntry的system进程并不会加载win32k.sys，一般都通过搜索内存查找它的地址。比较成熟的方式是在KeAddSystemServiceTable附近查找：

```
nt!KeAddSystemServiceTable+0x1a:
8059779e 8d88603f5580    lea     ecx,nt!KeServiceDescriptorTableShadow (80553f60)[eax]
805977a4 833900          cmp     dword ptr [ecx],0
805977a7 7546            jne     nt!KeAddSystemServiceTable+0x6b (805977ef)
```

SSDT是不可写的，可以通过修改CR0的标记，HOOK之后再改回来；也可以通过MDL映射一份可写区域。具体代码如下：

```
NTSTATUS MAKEMyMDL()	{
    MyMDL = MmCreateMdl(NULL, 
        KeServiceDescriptorTable.ServiceTableBase, 
        KeServiceDescriptorTable.NumberOfServices*4
    );		
    if (!MyMDL) return STATUS_UNSUCCESSFUL;		
    MmBuildMdlForNonPagedPool(MyMDL);		
    MyMDL->MdlFlags |= MDL_MAPPED_TO_SYSTEM_VA;		
    NewServiceDescriptorTable = MmMapLockedPages(MyMDL, KernelMode);		
    return STATUS_SUCCESS;	
}
```

然后就可以将系统调用替换为我们的函数，可以在里头做一些捕获工作。主要用到几个宏：

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

### 重载内核

为什么要重载内核呢？因为有可能系统内核已经被病毒或者各种杀软搞坏了。首先根据系统版本，将ntoskrnl.exe或ntkrnlpa.exe文件加载到内存，这里需要对PE结构比较了解，因为要实现重定位，并且修复导入表。这时我们有了一个干净的内核，就可以在自己的内核上做SSDT和ShadowSSDT Hook了。KiFastCallEntry的hook也会重定位到我们这里。

重定位过程可以参考windows加载dll时做的工作：

```
NTSTATUS FixReloc(PVOID lpBase, PVOID OrigBase)
{
	ULONG count, i;
	PIMAGE_DOS_HEADER pDosHeader = (PIMAGE_DOS_HEADER)lpBase;
	PIMAGE_NT_HEADERS pNtHeaders = (PIMAGE_NT_HEADERS)(pDosHeader->e_lfanew + (PUCHAR)lpBase);
	ULONG RelocOffset = (ULONG)OrigBase - pNtHeaders->OptionalHeader.ImageBase;
	PULONG newAddr;
	IMAGE_DATA_DIRECTORY ImageDataDirectory = pNtHeaders->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC];

	PIMAGE_BASE_RELOCATION pImageBaseRelocation = (PIMAGE_BASE_RELOCATION)(ImageDataDirectory.VirtualAddress + (PUCHAR)lpBase);
	if (!pImageBaseRelocation)
	{
		DbgPrint("FixReloc: pImageBaseRelocation error!\n");
		return STATUS_UNSUCCESSFUL;
	}

	while (pImageBaseRelocation->SizeOfBlock)
	{
		count = (pImageBaseRelocation->SizeOfBlock - 8)/2;
		for (i=0; i<count; i++)
		{
			if ((pImageBaseRelocation->TypeOffset[i] >> 12) == 3)
			{
				newAddr =(PULONG)((pImageBaseRelocation->TypeOffset[i] & 0xFFF) + pImageBaseRelocation->VirtualAddress + (ULONG)lpBase);
				*newAddr = *newAddr + RelocOffset;
			}
		}
		pImageBaseRelocation = (PIMAGE_BASE_RELOCATION)((ULONG)pImageBaseRelocation + pImageBaseRelocation->SizeOfBlock);
	}
	return STATUS_SUCCESS;
}
```

### IDT HOOK

DOS时代学好中断就可以声称掌握系统了，但是现在的windows中，中断作用虽然不再那么明显，但还是值得把关的地方。IDT是中断描述符表的简称，当中断发生时，通过中断号引用中断描述符表的项，从而执行特定中断服务例程（ISR）。IDTR寄存器保存了IDT有关的信息，SIDT指令可以读取到下面的一个结构：

```
typedef struct _IDTINFO
{
	WORD IDTLimit;
	WORD LowIDTBase;
	WORD HiIDTBase;
} IDTINFO;
```

记录了IDT的基址和大小。IDT中的每一项是如下的64位结构：

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

这里的selector指向GDT（全局描述符表）中的一个描述符。每个描述符都用来记录一个段的信息。GDT记录了所有描述符，包括段的基地址和大小，权限信息等。GDTR寄存器保存了GDT的基址，可以通过SGDT获取。

```
kd> !pcr
KPCR for Processor 0 at ffdff000:
    Major 1 Minor 1
	NtTib.ExceptionList: b286b664
	      InterruptMode: 00000000
	                IDT: 8003f400
	                GDT: 8003f000
	                TSS: 80042000
kd> dqs 8003f400 l5
8003f400  80538e00`0008f19c
8003f408  80538e00`0008f314
8003f410  00008500`0058113e
8003f418  8053ee00`0008f6e4
8003f420  8053ee00`0008f864
```

!pcr指令可以查看IDT和GDT，找一下int 3中断的处理函数：段选择子(GDT的索引)为8，说明是内核模式段，基址为0（这里不懂的可以看一下分页模式和分段模式）。ISR就是8053f6e4。如果我们修改这里，就可以在一定程度上控制调试过程。

IDT HOOK通过修改中断服务例程控制中断发生时执行的函数。还可以在IDT表中找一个空白项，实现我们自己的中断处理例程。有一个很好的模板用于实现IDT的inline hook。

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
		IdtEntries[i].HiOffset = (WORD)((DWORD)entry >> 16);
		__asm sti
		offset += sizeof(template);
	}
}
```

### 注册官方回调函数

监控进程创建、模块加载，一方面可以HOOK相关的函数，另一方面可以利用windows提供的回调机制，注册回掉函数。实际上在64不能修改内核时，商业软件的做法都是通过注册回调来实现原来的功能。

主要是三个函数：

```
PsSetCreateProcessNotifyRoutine
PsSetCreateThreadNotifyRoutine
PsSetLoadImageNotifyRoutine
```

具体的函数类型可以查看[MSDN](http://msdn.microsoft.com/en-us/library/windows/hardware/ff559951%28v=vs.85%29.aspx)。

### VT技术

这里的VT专指Intel的硬件虚拟化技术（Hardware Enabled Virtualization， HEV）。VT技术主要是为了解决虚拟机效率问题而开发的技术，在硬件层面支持虚拟化。简单来说，利用intel的VT技术，可以让自己处于一个全新的状态，人们一般戏称ring -1层；在这一层之上可以有多个操作系统，操作系统，各个操作系统都可以互不影响的进行IO访问、系统调用、产生中断等，但是与单个操作系统不同的时这些行为都会被ring-1捕获。VT可以一劳永逸解决各种HOOK可以解决的问题。VT实际上让CPU处于一个被监控状态。

具体化一下之前提到的概念：VMM（Virtual Machine Monitor）即虚拟机监控器，会捕获虚拟机的操作，比如执行某些特权指令，然后模拟执行后返回给虚拟机。使用VT后CPU进入全新的VMX模式，该模式下CPU会有两种状态：VMX root和VMX non-root状态。VMX non-root状态所有指令都可以执行，省去软件模拟的过程，但是某些特权指令还是会被捕获，触发一次#VMExit事件，进入VMX root模式。

为了实现这些功能，intel增加了一些VMX指令：

```
VMXON
VMXOFF
VMLAUNCH
VMCALL
VMRESUME
VMPTRLD
VMPTRST
VMCLEAR
VMREAD
VMWRITE
INVEPT
INVVPID
```

VMXON指令使得CPU进入VMX模式，然后通过VMPTRLD、VMPTRST、VMCLEAR、VMREAD、VMWRITE这些指令初始化一个VMCS，保存状态切换所需的信息，主要有CR*寄存去、段寄存器、通用寄存器 、MSR寄存器、段描述符相关的寄存器（LDTR、GDTR）、EFLAG寄存器等，前面提到过KiFastCallEntry地址保存在IA32_SYSENTER_EIP寄存器中，如果我们可以控制这个寄存器的值，就没必要去hook了。初始化完成之后执行VMLAUNCH指令，进入虚拟机的执行过程。运行期间只要捕获#VMExit事件即可。捕获之后可以通过查看VMCS区域，可以确认是执行什么指令导致的#VMExit。

可以看到VT技术是非常强大的，如果是自己来用，可以完全发挥它的作用，轻松监控和调试所有程序。不能商业化的原因主要还是考虑到一些老旧的CPU不支持。不过迟早VT技术会用到各个领域。

构建好我们的内核HOOK框架之后，就可以编写程序实现具体的功能了。监控软件行为和调试最新漏洞都不是问题。
