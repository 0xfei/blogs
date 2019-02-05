
---
title: "win32系统调用"
date: 2014-07-29T09:13:30+08:00
draft: true
categories: ["Windows安全"]
tags: ["Windows安全"]
topics: ["Windows安全"]
---

win32 API只是Windows在ring3提供的编程接口，ring0的实现接口并未暴露给用户，3到0的调用过程也并未文档化，所以需要自己反汇编来学习如何实现。这里以CreateFile为例，学习win32的系统调用。

写一个简单的win32程序，创建/打开一个文件：

<!--more-->

```

#include <windows.h> 

int main()
{
	CreateFile("1", FILE_ALL_ACCESS, FILE_SHARE_READ, 
		NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
	return 0;
}

```

OD观察它的调用流程：CreateFileA —— CreateFileW —— ntdll!ZwCreateFile —— KiFastSystemCall —— sysenter，然后进入内核模式。


这就是用户模式的全部调用。

ntdll!Zw***和ntdll!Nt***是同一函数，只是名称不同；而**A则一般要转化为**W，内核中都是使用unicode字符，所以平时编程也应该尽量使用UNICODE函数。

KiFastSystemCall是一个stub函数，即**桩**函数，作一些过渡性的工作，windbg下看ZwCreateFile如下：


```

lkd> uf ntdll!ZwCreateFile
ntdll!NtCreateFile:
7c92d0ae b825000000      mov     eax,25h
7c92d0b3 ba0003fe7f      mov     edx,offset SharedUserData!SystemCallStub (7ffe0300)
7c92d0b8 ff12            call    dword ptr [edx]
7c92d0ba c22c00          ret     2Ch

```

SharedUserData是用户模式和内核模式共享的一块地址：7ffe0000和ffdf0000，32位下大小好像是4KB，可以实现0和3的数据共享。是一个_KUSER_SHARED_DATA结构


```

lkd> dt _KUSER_SHARED_DATA
ntdll!_KUSER_SHARED_DATA
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

执行SYSENTER指令之前，eax保存了内核函数在SSDT中的编号：0x25，edx保存了栈指针，因此可以通过edx找到函数参数。sysenter指令介绍请移步。

现在进入内核，执行nt!KiFastCallEntry。KiFastCallEntry分发到对应的系统服务。KiFastCallEntry所做的工作非常多.

另外值得看一下0x2e的中断处理函数：


```

lkd> !idt 2e

Dumping IDT:

2e:	8053e521 nt!KiSystemService

```

是KiSystemService而不是KiFastCallEntry，这两个函数用汇编实现，都用于查找SSDT或Shadow SSDT的服务，从而实现系统调用。

最后看一下内核中的ZwCreateFile函数。


```

lkd> uf nt!ZwCreateFile
nt!ZwCreateFile:
804ff08c b825000000      mov     eax,25h
804ff091 8d542404        lea     edx,[esp+4]
804ff095 9c              pushfd
804ff096 6a08            push    8
804ff098 e884f40300      call    nt!KiSystemService (8053e521)
804ff09d c22c00          ret     2Ch

```

是一个stub函数，还是需要查找SSDT序号来实现功能。
