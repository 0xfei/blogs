
---
title: "extern和_declspec(dllimport)的区别"
date: 2014-06-04T09:13:30+08:00
draft: true
categories: ["Windows安全"]
tags: ["Windows安全"]
topics: ["Windows安全"]
---

写shadow ssdt hook的时候出现一个比较奇怪的错误，看下面的代码：

<!--more-->


```

#include "ntddk.h"

#pragma pack(1)
typedef struct _SystemServiceTable 
{
	unsigned int *ServiceTableBase;
	unsigned int *ServiceCounterTableBase;
	unsigned int NumberOfServices;
	unsigned char* ParamTableBase;
} SYSTEM_SERVICE_TABLE, *PSYSTEM_SERVICE_TABLE;

typedef struct _SERVICE_DESCRIPTOR_TABLE
{
	SYSTEM_SERVICE_TABLE ntoskrnl;
	SYSTEM_SERVICE_TABLE win32k;
	SYSTEM_SERVICE_TABLE table3;
	SYSTEM_SERVICE_TABLE table4;
} SERVICE_DESCRIPTOR_TABLE, *PSERVICE_DESCRIPTOR_TABLE;
#pragma pack()

SERVICE_DESCRIPTOR_TABLE KeSystemServiceDescriptorTable;
``````<span style="text-decoration: underline;">```_declspec(dllimport) ```PSYSTEM_SERVICE_TABLE KeServiceDescriptorTable;</span>
_declspec(dllimport) KeAddSystemServiceTable(ULONG, ULONG, ULONG, ULONG, ULONG);

SYSTEM_SERVICE_TABLE* GetServiceDescriptorShadowTableAddress()
{
	unsigned char *check = (unsigned char*)KeAddSystemServiceTable;
	int i;
	SYSTEM_SERVICE_TABLE *rc = 0;
	DbgPrint("KeAddSystemServiceTable: 0x%p\n", check);
	DbgPrint("KeServiceDescriptorTable: 0x%p\n", KeServiceDescriptorTable);
	for (i=0; i<100; i++)
	{
		__try
		{
			rc = *(SYSTEM_SERVICE_TABLE**)check;
			if (!MmIsAddressValid(rc)
				|| (rc == KeServiceDescriptorTable)
				|| (memcmp(rc, KeServiceDescriptorTable, sizeof (*rc)) != 0))
			{
				if (MmIsAddressValid(rc))
					DbgPrint("Something: %08x\n", rc);
				check ++;
				rc = 0;
			}
		}
		__except(EXCEPTION_EXECUTE_HANDLER)
		{
			rc = 0;
		}
		if (rc) break;
	}
	DbgPrint("Ans: %08x\n", rc);
	return rc;
}

void Unload(PDRIVER_OBJECT driver)
{
	DbgPrint("Unload!\n");
}

NTSTATUS DriverEntry(PDRIVER_OBJECT driver, PUNICODE_STRING szReg)
{
	DbgPrint("DriverEntry\n");
	GetServiceDescriptorShadowTableAddress();
	driver-> DriverUnload = Unload;
	return STATUS_SUCCESS;
}

```


总是得不到正确的地址，看出来是哪里错了嘛？

加下划线的那一行，我用了dllimport，实际上_declspec(dllimport)导入的是导出变量的值，而这里需要的是导出变量的地址，所以需要用extern。改掉之后就可以找到KeServiceDescriptorTableShadow的地址了。
