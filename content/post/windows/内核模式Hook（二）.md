
---
title: "内核模式Hook（二）"
date: 2014-04-27T09:13:30+08:00
draft: true
categories: ["Windows安全"]
tags: ["Windows安全"]
topics: ["Windows安全"]
---

接着说内核Hook中的IAT Hook、导出表Hook、IRP Hook、Inline hook和几种门的添加。

## IAT Hook
内核模式的IAT Hook要比在ring3下实现方便很多，PsSetImageLoadNotifyRoutine注册一个回调函数，进程加载dll时会调用该函数。函数原型如下：

<!--more-->

```

void MyFunction( IN PUNICODE_STRING szImageName,
        IN DWORD dwProcessId,
        IN PIMAGE_INFO pImageInfo);


```

szImageName是内核加载的模块名，dwProcessId是加载的目标进程ID，PIMAGE_INFO结构包含了加载基址等一些有用信息。


```

typedef struct _IMAGE_INFO {
    union {
        ULONG Properties;
        struct {
            ULONG ImageAddressingMode  : 8;  // Code addressing mode
            ULONG SystemModeImage      : 1;  // System mode image
            ULONG ImageMappedToAllPids : 1;  // Image mapped into all processes
            ULONG ExtendedInfoPresent  : 1;  // IMAGE_INFO_EX available
            ULONG Reserved             : 21;
        };
    };
    PVOID       ImageBase;
    ULONG       ImageSelector;
    SIZE_T      ImageSize;
    ULONG       ImageSectionNumber;
} IMAGE_INFO, *PIMAGE_INFO;

```

有了这些信息，很容易结合PE结构，定位到进程IAT，然后替换到某一项：


```

NTSTATUS Hook(PIMAGE_DOS_HEADER base, HANDLE dwProcessID)
{
	PIMAGE_DOS_HEADER dosHeader;
	PIMAGE_NT_HEADERS pNTHeader;
	PIMAGE_IMPORT_DESCRIPTOR pIID;
	PIMAGE_IMPORT_BY_NAME pImportByName;
	DWORD RVA;
	PDWORD pIAT, pINT;

	int count, index;
	char *dllName = NULL;
	char *dllTarget = "kernel32.dll";
	char *funcTarget = "GetProcAddress";
	PMDL MyMDL;
	PDWORD MappedTable;
	
	dosHeader = base;
	pNTHeader = (PIMAGE_NT_HEADERS)(dosHeader-> e_lfanew + (DWORD)base);

	if (pNTHeader-> Signature != IMAGE_NT_SIGNATURE)
		return STATUS_INVALID_IMAGE_FORMAT;

	RVA = pNTHeader-> OptionalHeader.DataDirectory  [IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress;
	pIID = (PIMAGE_IMPORT_DESCRIPTOR)(RVA + (DWORD)base);

	for (count = 0; pIID[count].Characteristics!=0; count++)
	{
		dllName = (char *)(pIID[count].Name + (DWORD)base);
		pIAT = (PDWORD)((DWORD)base +     (DWORD)pIID[count].FirstThunk);
		pINT = (PDWORD)((DWORD)base +     (DWORD)pIID[count].OriginalFirstThunk);
		for (index=0; pIAT[index]!=0; index++)
		{
		  if ((pINT[index] & IMAGE_ORDINAL_FLAG) != IMAGE_ORDINAL_FLAG)
		  {
		    pImportByName =   (PIMAGE_IMPORT_BY_NAME)(pINT[index] + (DWORD)base);
		    if ((_stricmp(dllName, dllTarget) == 0) &&
		  	(_stricmp(pImportByName-> Name, funcTarget) == 0))
		    {
			MyMDL = MmCreateMdl(NULL, &pIAT[index], 4);
			if (!MyMDL) return STATUS_UNSUCCESSFUL;
			MmBuildMdlForNonPagedPool(MyMDL);
			MyMDL-> MdlFlags |= MDL_MAPPED_TO_SYSTEM_VA;
			MappedTable = MmMapLockedPages(MyMDL, KernelMode);
			RtlCopyMemory((PVOID)KerlsharedMemory, temp, 8);
			RtlCopyMemory((PVOID)(KerlsharedMemory+2),                 (PVOID)&pIAT[index], 4);
			*MappedTable = UsersharedMemory;
			MmUnmapLockedPages(MappedTable,MyMDL);
			IoFreeMdl(MyMDL);
		  }
		 }
	       }
	}
	return STATUS_SUCCESS;
}


```

这里为了扩展和安全，回调函数实际上是另一个函数，该函数利用PEB结构枚举进程模块，找到.exe时调用上面的Hook函数。

最后一个问题是地址问题，内核模式地址和用户进程私有空间地址通信。这里简单的用到了一块共享区，内核地址0xFFDF0000和用户地址0x7FFE0000指向同一地址页面。


```

lkd> dt _kuser_shared_data
nt!_KUSER_SHARED_DATA
   +0x000 TickCountLow     : Uint4B
   +0x004 TickCountMultiplier : Uint4B
   +0x008 InterruptTime    : _KSYSTEM_TIME
   +0x014 SystemTime       : _KSYSTEM_TIME
   +0x020 TimeZoneBias     : _KSYSTEM_TIME
   +0x02c ImageNumberLow   : Uint2B
   +0x02e ImageNumberHigh  : Uint2B
   +0x030 NtSystemRoot     : [260] Uint2B
   +0x238 MaxStackTraceDepth : Uint4B
   +0x23c CryptoExponent   : Uint4B
   +0x240 TimeZoneId       : Uint4B
   +0x244 Reserved2        : [8] Uint4B
   +0x264 NtProductType    : _NT_PRODUCT_TYPE
   +0x268 ProductTypeIsValid : UChar
   +0x26c NtMajorVersion   : Uint4B
   +0x270 NtMinorVersion   : Uint4B
   +0x274 ProcessorFeatures : [64] UChar
   +0x2b4 Reserved1        : Uint4B
   +0x2b8 Reserved3        : Uint4B
   +0x2bc TimeSlip         : Uint4B
   +0x2c0 AlternativeArchitecture : _ALTERNATIVE_ARCHITECTURE_TYPE
   +0x2c8 SystemExpirationDate : _LARGE_INTEGER
   +0x2d0 SuiteMask        : Uint4B
   +0x2d4 KdDebuggerEnabled : UChar
   +0x2d5 NXSupportPolicy  : UChar
   +0x2d8 ActiveConsoleId  : Uint4B
   +0x2dc DismountCount    : Uint4B
   +0x2e0 ComPlusPackage   : Uint4B
   +0x2e4 LastSystemRITEventTickCount : Uint4B
   +0x2e8 NumberOfPhysicalPages : Uint4B
   +0x2ec SafeBootMode     : UChar
   +0x2f0 TraceLogging     : Uint4B
   +0x2f8 TestRetInstruction : Uint8B
   +0x300 SystemCall       : Uint4B
   +0x304 SystemCallReturn : Uint4B
   +0x308 SystemCallPad    : [3] Uint8B
   +0x320 TickCount        : _KSYSTEM_TIME
   +0x320 TickCountQuad    : Uint8B
   +0x330 Cookie           : Uint4B

```


```

lkd> dd 0xFFDF0000
ffdf0000  0085989a 0fa00000 84970c95 0000013e
ffdf0010  0000013e e41f648e 01cf6285 01cf6285
ffdf0020  f1dcc000 ffffffbc ffffffbc 014c014c
ffdf0030  003a0043 0057005c 004e0049 004f0044
ffdf0040  00530057 00000000 00000000 00000000
ffdf0050  00000000 00000000 00000000 00000000
ffdf0060  00000000 00000000 00000000 00000000
ffdf0070  00000000 00000000 00000000 00000000
lkd> dd 0x7FFE0000
7ffe0000  00859afb 0fa00000 8a4304af 0000013e
7ffe0010  0000013e e9cb5ca8 01cf6285 01cf6285
7ffe0020  f1dcc000 ffffffbc ffffffbc 014c014c
7ffe0030  003a0043 0057005c 004e0049 004f0044
7ffe0040  00530057 00000000 00000000 00000000
7ffe0050  00000000 00000000 00000000 00000000
7ffe0060  00000000 00000000 00000000 00000000
7ffe0070  00000000 00000000 00000000 00000000


```

## 导出表Hook
导出表Hook和导入表Hook类似，都是对模块进行解析然后替换到导入或导出的函数地址。不过相比IAT Hook，导出表Hook更方便一些，可以直接枚举模块的导出表然后进行替换，不必注册回调函数等。


```

VOID Hook(BOOLEAN hook)
{
	DWORD base, index, addr = 0, i;
	PIMAGE_DOS_HEADER pDosHeader;
	PIMAGE_NT_HEADERS pNtHeader;
	PIMAGE_EXPORT_DIRECTORY exports;

	PBYTE pFuncName = NULL;
	PDWORD pAddressOfFunction, pAddressOfNames;
	PWORD pAddressOfNameOdrinals;

	base = GetModuleBaseAddress("ntkrnlpa.exe");
	DbgPrint("base: %08x\n", base);

	pDosHeader = (PIMAGE_DOS_HEADER)base;
	pNtHeader = (PIMAGE_NT_HEADERS)(base + pDosHeader-> e_lfanew);
	exports = (PIMAGE_EXPORT_DIRECTORY)(base + 
		pNtHeader-> OptionalHeader.DataDirectory[0].VirtualAddress);

	pAddressOfFunction = (PDWORD)(base +      exports-> AddressOfFunctions);
	pAddressOfNames = (PDWORD)(base + exports-> AddressOfNames);
	pAddressOfNameOdrinals = (PWORD)(base +      exports-> AddressOfNameOrdinals);

	for (i = 0; i < exports-> NumberOfNames; i++)
	{
		index = pAddressOfFunction[i];
		pFuncName = (PBYTE)(base + pAddressOfNames[i]);
		if (_stricmp(pFuncName, FUNCNAME) == 0)
		{
		  addr = base + (DWORD)pAddressOfFunction[index];
		  break;
		}
	}

	if (addr == 0)
	{
		DbgPrint("addr is 0\n");
		return;
	}

	if (hook)
	{
		_asm
		{
			cli;
			mov eax, cr0;
			and eax, not 10000h;
			mov cr0, eax;
			sti;
		}
		DbgPrint("PsGetCurrentProcessId : %08x\n", addr);
		pAddressOfFunction[index]  = (DWORD)NewFunction - base;
		old = addr;
		_asm
		{
			cli;
			mov eax, cr0;
			or eax, 10000h;
			mov cr0, eax;
			sti;
		}
	} else
	{
		_asm
		{
			cli;
			mov eax, cr0;
			and eax, not 10000h;
			mov cr0, eax;
			sti;
		}
		pAddressOfFunction[index]  = (DWORD)old - base;
		_asm
		{
			cli;
			mov eax, cr0;
			or eax, 10000h;
			mov cr0, eax;
			sti;
		}
	}
}


```

这里需要注意的是模块基地址需要自己获取，使用ZwQuerySystemInformation可以获取模块链，然后自己查找。


```

DWORD GetModuleBaseAddress(PBYTE ModuleName)
{
	NTSTATUS status;
	DWORD size = 0, index;
	PDWORD buffer;
	PSYSTEM_MODULE_INFORMATION module;
	DWORD ModuleAddress = 0;

	ZwQuerySystemInformation(
		SystemModuleInformation, 
		NULL, 
		&size,
		&size);
	if (size == 0)
	{
		DbgPrint("SystemModuleInformation length cannot be defined!\n");
		return 0;
	}
	buffer = ExAllocatePool(NonPagedPool, size);
	if (!buffer)
	{
		DbgPrint("ExAllocatePool error!\n");
		return 0;
	}
	status = ZwQuerySystemInformation(
		SystemModuleInformation, 
		buffer, 
		size, 
		0);
	if (status != STATUS_SUCCESS)
	{
		DbgPrint("Query Information error!\n");
		return 0;
	}
	module = (PSYSTEM_MODULE_INFORMATION)((PDWORD)buffer + 1);
	for (index = 0; index < *buffer; index++)
	{
		if (_stricmp(module[index].ImageName + module[index].ModuleNameOffset, 
			ModuleName) == 0)
		{
		  ModuleAddress = (DWORD)module[index].Base;
		  DbgPrint("Module found at: %08x\n", ModuleAddress);
		}
	}
	ExFreePool(buffer);
	return ModuleAddress;
}

```

## IRP Hook
IRP用来描述IO请求的信息，是一个很庞大的数据结构，包括了请求的很多信息。


```

lkd> dt _irp
nt!_IRP
   +0x000 Type             : Int2B
   +0x002 Size             : Uint2B
   +0x004 MdlAddress       : Ptr32 _MDL
   +0x008 Flags            : Uint4B
   +0x00c AssociatedIrp    : __unnamed
   +0x010 ThreadListEntry  : _LIST_ENTRY
   +0x018 IoStatus         : _IO_STATUS_BLOCK
   +0x020 RequestorMode    : Char
   +0x021 PendingReturned  : UChar
   +0x022 StackCount       : Char
   +0x023 CurrentLocation  : Char
   +0x024 Cancel           : UChar
   +0x025 CancelIrql       : UChar
   +0x026 ApcEnvironment   : Char
   +0x027 AllocationFlags  : UChar
   +0x028 UserIosb         : Ptr32 _IO_STATUS_BLOCK
   +0x02c UserEvent        : Ptr32 _KEVENT
   +0x030 Overlay          : __unnamed
   +0x038 CancelRoutine    : Ptr32     void 
   +0x03c UserBuffer       : Ptr32 Void
   +0x040 Tail             : __unnamed


```

有很多文章介绍了这些成员的含义，以及IRP头后面的IRP_STACK_LOCATION数据部分。这里仅介绍IRP Hook的两种方式：IofCallDriver HOOK和修改分发函数。


```

lkd> u IofCallDriver
nt!IofCallDriver:
804ef130 ff2500d35480    jmp     dword ptr [nt!pIofCallDriver (8054d300)]
804ef136 cc              int     3
804ef137 cc              int     3

```

直接hook掉开头的地址就可以了。

HOOK驱动的分发函数也是一种方式。


```

NTSTATUS Hook()
{
	NTSTATUS status;
	UNICODE_STRING usDevTcp;
	WCHAR szDevTcpBuffer[] = L"\\Device\\Tcp";
	RtlInitUnicodeString(
		&usDevTcp, 
		szDevTcpBuffer);

	status = IoGetDeviceObjectPointer(
		&usDevTcp, 
		FILE_READ_DATA, 
		&pFile, 
		&pDevTcp);
	if (status != STATUS_SUCCESS)
		return status;

	pDrvTcp = pDevTcp-> DriverObject;
	old = (FUNCTION)InterlockedExchange(
		(PULONG)&pDrvTcp-> MajorFunction[IRP_MJ_DEVICE_CONTROL], 
		(ULONG)MyFunc);

	return status;
}

```

## Inline Hook
Inline Hook是一种很奇妙的想法：将函数的开头字节修改，然后执行自己的函数，最后再跳回来。

我这里Hook了MmGetSystemRoutineAddress，让它查找函数地址之前输出函数的名称。


```

char old[10];
char outString[] = "Function: %ws\n";

__declspec(naked) MyMmGetSystemRoutineAddress()
{
	__asm{
		cli;
		push    ebp;
		mov     ebp,esp;
		sub     esp,20h;
		mov eax, [ebp + 8]
		push eax
		mov eax, offset outString;
		push eax;
		mov eax, 80528e92h;
		call eax;
		pop eax;
		sti;
		_emit 0xEA;
		_emit 0xAA;
		_emit 0xAA;
		_emit 0xAA;
		_emit 0xAA;
		_emit 0x80;
		_emit 0x00;
	}
}

void Hook()
{
	int i;
	UNICODE_STRING uniName;
	ULONG Address = 0, newEntry;
	unsigned char * realAddress;
	unsigned char * funcAddress;
	ULONG tempAddress;
	char temp[] = {
		0xEA,
		0xAA,
		0xAA,
		0xAA,
		0xAA,
		0x80,
		0x00,
		0x90
	};

	RtlInitUnicodeString(&uniName, L"MmGetSystemRoutineAddress");
	Address = (ULONG)MmGetSystemRoutineAddress(&uniName);
	if (Address == 0) 
	{
		DbgPrint("Cannot find address!\n");
		return;
	}
	realAddress = (unsigned char*)Address;
	newEntry = Address + NEWENTRYOFFSET;

	tempAddress = (ULONG)ExAllocatePool(NonPagedPool, 256);
	funcAddress = (unsigned char *)tempAddress;
	for (i =0; i<256; i++)
	{
		funcAddress[i] = 
		  ((unsigned char *)MyMmGetSystemRoutineAddress)[i];
	}
	*((unsigned long *)(&temp[1])) = (unsigned long)funcAddress;
	for (i=0; i<200; i++)
	{
		if ( (funcAddress[i] == 0xAA) &&
			(funcAddress[i+1] == 0xAA) &&
			(funcAddress[i+2] == 0xAA) &&
			(funcAddress[i+3] == 0xAA) )
		{
			*(ULONG*)(&funcAddress[i]) = newEntry;
			break;
		}
	}
	for (i = 0; i<NEWENTRYOFFSET; i++)
	{
		old[i] = realAddress[i];
		realAddress[i] = temp[i];
	}
}


```

这种替换的想法看起来还是很爽的。看一下Hook之后的情况：


```

lkd> u MmGetSystemRoutineAddress
nt!MmGetSystemRoutineAddress:
805a4732 ea009f3c828000  jmp     0080:823C9F00
805a4739 90              nop
805a473a 53              push    ebx
805a473b 56              push    esi
805a473c 57              push    edi
805a473d 6800475a80      push    offset nt!MiDereferenceImports+0xaa (805a4700)
805a4742 8d45e8          lea     eax,[ebp-18h]
805a4745 33f6            xor     esi,esi
lkd> u 823C9F00 L50
823c9f00 fa              cli
823c9f01 55              push    ebp
823c9f02 8bec            mov     ebp,esp
823c9f04 83ec20          sub     esp,20h
823c9f07 8b4508          mov     eax,dword ptr [ebp+8]
823c9f0a 50              push    eax
823c9f0b b88017cef8      mov     eax,0F8CE1780h
823c9f10 50              push    eax
823c9f11 b8928e5280      mov     eax,offset nt!DbgPrint (80528e92)
823c9f16 ffd0            call    eax
823c9f18 58              pop     eax
823c9f19 fb              sti
823c9f1a ea3a475a808000  jmp     0080:805A473A

```

## 调用门
调用门是GDT中的一种特殊描述符，当跨段调用时执行。调用门的结构如下：


```

typedef struct _CALLGATE {
	WORD offsetl;
	WORD selector;
	BYTE count;
	BYTE type;
	WORD offseth;
} CALLGATE, *PCALLGATE;


```

用SGDT获取GDTR中的地址，然后枚举所有描述符，查找未使用的项，然后添加一个。


```

NTSTATUS AddCallGate(DWORD MyCALLGATE)
{
	char s[256];
	char gdt[6];
	DWORD base; 
	PCALLGATE pCallGate = NULL;

	now = 8;

	_asm sgdt gdt;
	base = *(DWORD*)(gdt + 2);

	while (now < GDT_LIMIT)
	{
		pCallGate = (PCALLGATE)(base + now);
		if ((pCallGate-> type & 0x80) == 0)
		{
			_snprintf(s, 256, "%08x\n", now|3);
			DbgPrint(s);
			pCallGate-> type = GATE_TYPE;
			pCallGate-> offsetl = (WORD)((DWORD)MyCALLGATE & 0xFFFF);
			pCallGate-> selector = 0x08;
			pCallGate-> offseth = (WORD)((DWORD)MyCALLGATE > > 16);
			pCallGate-> count = 0;
			DbgPrint("Add call gate!\n");
			break;
		}
		now += 8;
	}

	return STATUS_SUCCESS;
}

```

中断门跟调用门类似，只是在IDT中存储，获取和修改方式一样。
