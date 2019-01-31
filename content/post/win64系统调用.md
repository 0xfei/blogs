
---
title: "win64系统调用"
date: 2014-07-31T09:13:30+08:00
draft: true
categories: ["Windows安全"]
tags: ["Windows安全"]
topics: ["Windows安全"]
---

win64系统调用比win32要简洁不少，因为寄存器增多，切换起来更方便了。还是看CreateFile的例子，直接看ntdll!ZwCreateFile。

<!--more-->


```

lkd> uf ntdll!NtCreateFile
ntdll!NtCreateFile:
00000000`76f50400 4c8bd1          mov     r10,rcx
00000000`76f50403 b852000000      mov     eax,52h
00000000`76f50408 0f05            syscall
00000000`76f5040a c3              ret
lkd> uf ntdll!ZwCreateFile
ntdll!NtCreateFile:
00000000`76f50400 4c8bd1          mov     r10,rcx
00000000`76f50403 b852000000      mov     eax,52h
00000000`76f50408 0f05            syscall
00000000`76f5040a c3              ret

```


rcx保存在r10中，eax保存SSDT的下标，然后执行syscall指令。这个指令在Intel上只支持64位，官方文档是这样说的：


```

SYSCALL saves the RIP of the instruction following SYSCALL to RCX and loads a new 
RIP from the IA32_LSTAR (64-bit mode). Upon return, SYSRET copies the value 
saved in RCX to the RIP. 
SYSCALL saves RFLAGS (lower 32 bit only) in R11. It then masks RFLAGS with an 
OS-defined value using the IA32_FMASK (MSR C000_0084). The actual mask value 
used by the OS is the complement of the value written to the IA32_FMASK MSR. 
None of the bits in RFLAGS are automatically cleared (except for RF). SYSRET 
restores RFLAGS from R11 (the lower 32 bits only).
Software should not alter the CS or SS descriptors in a manner that violates the 
following assumptions made by SYSCALL/SYSRET:
	 The CS and SS base and limit remain the same for all processes, including the 
operating system (the base is 0H and the limit is 0FFFFFFFFH).
	 The CS of the SYSCALL target has a privilege level of 0.
	 The CS of the SYSRET targethas a privilege level of 3.
SYSCALL/SYSRET do not check for violations of these assumptions.

```


将RIP保存在RCX中，将IA32_LSTAR寄存器的值载入RIP作为RING0将要执行的地址。具体的变化如下：


```

IF (CS.L != 1 ) or (IA32_EFER.LMA !=1) or (IA32_EFER.SCE !=1)
(* Not in 64-Bit Mode or SYSCALL/SYSRET not enabled in IA32_EFER *)
THEN #UD; FI;
RCX &larr;RIP;
RIP &larr;LSTAR_MSR;
R11 &larr;EFLAGS;
EFLAGS &larr;(EFLAGS MASKED BY IA32_FMASK);
CPL &larr;0;
CS(SEL) &larr;IA32_STAR_MSR[47:32];
CS(DPL) &larr;0;
CS(BASE) &larr;0;
CS(LIMIT) &larr;0xFFFFF;
CS(GRANULAR) &larr;1;
SS(SEL) &larr;IA32_STAR_MSR[47:32] + 8;
SS(DPL) &larr;0;
SS(BASE) &larr;0;
SS(LIMIT) &larr;0xFFFFF;
SS(GRANULAR) &larr;1;

```

涉及到几个寄存器：IA32_LSTAR，IA32_STAR_MSR，IA32_FMASK，它们的地址分别是0xC0000082、0xC0000081、0xC0000084，还有一个IA32_CSTAR用于兼容模式，地址是0xC0000083。


```

kd> rdmsr C0000081
msr[c0000081] = 00230010`00000000
kd> rdmsr C0000082
msr[c0000082] = fffff800`03e7bec0
kd> rdmsr C0000084
msr[c0000084] = 00000000`00004700
kd> u fffff80003e7bec0
nt!KiSystemCall64:
fffff800`03e7bec0 0f01f8          swapgs
fffff800`03e7bec3 654889242510000000 mov   qword ptr gs:[10h],rsp
fffff800`03e7becc 65488b2425a8010000 mov   rsp,qword ptr gs:[1A8h]
fffff800`03e7bed5 6a2b            push    2Bh
fffff800`03e7bed7 65ff342510000000 push    qword ptr gs:[10h]
fffff800`03e7bedf 4153            push    r11
fffff800`03e7bee1 6a33            push    33h
fffff800`03e7bee3 51              push    rcx
kd> rdmsr C0000083
msr[c0000083] = fffff800`03e7bc00
kd> u fffff80003e7bc00
nt!KiSystemCall32:
fffff800`03e7bc00 0f01f8          swapgs
fffff800`03e7bc03 654889242510000000 mov   qword ptr gs:[10h],rsp
fffff800`03e7bc0c 65488b2425a8010000 mov   rsp,qword ptr gs:[1A8h]
fffff800`03e7bc15 6a2b            push    2Bh
fffff800`03e7bc17 65ff342510000000 push    qword ptr gs:[10h]
fffff800`03e7bc1f 4153            push    r11
fffff800`03e7bc21 6a23            push    23h
fffff800`03e7bc23 51              push    rcx


```

KiSystemCall64或KiSystemCall32分发找到系统服务。

再看一下内核中调用ZwCreateFile会发生什么。


```

kd> uf nt!ZwCreateFile
Flow analysis was incomplete, some code may be missing
nt!ZwCreateFile:
fffff800`03e75f00 488bc4          mov     rax,rsp
fffff800`03e75f03 fa              cli
fffff800`03e75f04 4883ec10        sub     rsp,10h
fffff800`03e75f08 50              push    rax
fffff800`03e75f09 9c              pushfq
fffff800`03e75f0a 6a10            push    10h
fffff800`03e75f0c 488d05dd270000  lea     rax,[nt!KiServiceLinkage (fffff800`03e786f0)]
fffff800`03e75f13 50              push    rax
fffff800`03e75f14 b852000000      mov     eax,52h
fffff800`03e75f19 e9225f0000      jmp     nt!KiServiceInternal (fffff800`03e7be40)

nt!KiServiceInternal:
fffff800`03e7be40 4883ec08        sub     rsp,8
fffff800`03e7be44 55              push    rbp
fffff800`03e7be45 4881ec58010000  sub     rsp,158h
fffff800`03e7be4c 488dac2480000000 lea     rbp,[rsp+80h]
fffff800`03e7be54 48899dc0000000  mov     qword ptr [rbp+0C0h],rbx
fffff800`03e7be5b 4889bdc8000000  mov     qword ptr [rbp+0C8h],rdi
fffff800`03e7be62 4889b5d0000000  mov     qword ptr [rbp+0D0h],rsi
fffff800`03e7be69 fb              sti
fffff800`03e7be6a 65488b1c2588010000 mov   rbx,qword ptr gs:[188h]
fffff800`03e7be73 0f0d8bd8010000  prefetchw [rbx+1D8h]
fffff800`03e7be7a 0fb6bbf6010000  movzx   edi,byte ptr [rbx+1F6h]
fffff800`03e7be81 40887da8        mov     byte ptr [rbp-58h],dil
fffff800`03e7be85 c683f601000000  mov     byte ptr [rbx+1F6h],0
fffff800`03e7be8c 4c8b93d8010000  mov     r10,qword ptr [rbx+1D8h]
fffff800`03e7be93 4c8995b8000000  mov     qword ptr [rbp+0B8h],r10
fffff800`03e7be9a 4c8d1d3d010000  lea     r11,[nt!KiSystemServiceStart (fffff800`03e7bfde)]
fffff800`03e7bea1 41ffe3          jmp     r11

```

有三个函数非常显眼：KiServiceLinkage、KiServiceInternal、KiSystemServiceStart。KiServiceLinkage就是一句ret，KiServiceInternal做了一些简单处理后调用了KiSystemServiceStart。KiSystemServiceStart又调用了KiSystemCall64，说到底还是这哥们儿在发挥作用。

KiSystemCall64又调用了KiSystemServiceRepeat等发挥作用。KiSystemServiceRepeat这个函数很关键，是查找SSDT和Shadow SSDT的关键所在。


