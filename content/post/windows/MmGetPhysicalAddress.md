---
title: "分析MmGetPhysicalAddress"
date: 2014-04-06T02:43:49+08:00
draft: true
categories: ["Windows安全"]
tags: ["Windows安全"]
topics: ["Windows安全"]
---

现在的操作系统大多使用分页机制和分段机制实现虚拟内存管理。具体来说分段机制实现了段地址到线性地址的转换，而分页机制则实现了线性地址到物理地址的转换。

Windows xp平台的虚拟地址就是线性地址，可以自己去看看GDT中第CS项的段描述符的基地址，是0。本文通过逆向MmGetPhysicalAddress来学习Windows和X86的分页机制，平台是windows xp sp2，使用windbg。

<!--more-->

### 背景知识

需要了解几个结构。

页目录表（PDT）。页目录表是存放二级页表所在物理地址页码的数组。

页表（PPT）。页表是存放物理页码的数组。

页目录表表项(Page Directory Table Entry),PDE，页目录表中的数据类型。

页表表项(Page Table Entry),PTE，页表中的数据类型。一个页表表项表示一个物理页面，4KB或者2MB，取决于是否启用LargePage。

简单来说，线性地址到物理地址就是通过页目录表和页表完成的。CR3中保存了PDT的物理页码，线性地址最高10位表示页目录表偏移，进而找到PTT；线性地址中10位标识PTT偏移，找到页表；线性地址低12位是页表偏移，得到物理地址。

看一下VA为0的PDE和PTE。

```
lkd> !pte
                    VA 00000000
PDE at C0600000            PTE at C0000000
contains 0000000017645067  contains 0000000000000000
pfn 17645     ---DA--UWEV   not valid
```

### windbg查看MmGetPhysicalAddress

使用windbg的uf命令反汇编函数。逆向MmGetPhysicalAddress发现其实代码很少，80行都不到。

```
lkd> uf MmGetPhysicalAddress
nt!MmGetPhysicalAddress:
805063a4 8bff            mov     edi,edi
805063a6 55              push    ebp
805063a7 8bec            mov     ebp,esp
805063a9 53              push    ebx
805063aa 56              push    esi
805063ab 57              push    edi
805063ac 8b7d08          mov     edi,dword ptr [ebp+8]
805063af 8bcf            mov     ecx,edi
805063b1 c1e912          shr     ecx,12h
805063b4 81e1f83f0000    and     ecx,3FF8h
805063ba 8b81000060c0    mov     eax,dword ptr [ecx-3FA00000h]
805063c0 8b89040060c0    mov     ecx,dword ptr [ecx-3F9FFFFCh]
805063c6 be81000000      mov     esi,81h
805063cb 8bd0            mov     edx,eax
805063cd 23d6            and     edx,esi
805063cf 33db            xor     ebx,ebx
805063d1 3bd6            cmp     edx,esi
805063d3 751f            jne     nt!MmGetPhysicalAddress+0x50 (805063f4)

nt!MmGetPhysicalAddress+0x31:
805063d5 85db            test    ebx,ebx
805063d7 751b            jne     nt!MmGetPhysicalAddress+0x50 (805063f4)

nt!MmGetPhysicalAddress+0x35:
805063d9 0facc80c        shrd    eax,ecx,0Ch
805063dd c1e90c          shr     ecx,0Ch
805063e0 8bcf            mov     ecx,edi
805063e2 c1e90c          shr     ecx,0Ch
805063e5 25ffffff03      and     eax,3FFFFFFh
805063ea 81e1ff010000    and     ecx,1FFh
805063f0 03c1            add     eax,ecx
805063f2 eb3f            jmp     nt!MmGetPhysicalAddress+0x8f (80506433)

nt!MmGetPhysicalAddress+0x50:
805063f4 83e001          and     eax,1
805063f7 33c9            xor     ecx,ecx
805063f9 0bc1            or      eax,ecx
805063fb 7424            je      nt!MmGetPhysicalAddress+0x7d (80506421)

nt!MmGetPhysicalAddress+0x59:
805063fd 8bcf            mov     ecx,edi
805063ff c1e909          shr     ecx,9
80506402 81e1f8ff7f00    and     ecx,7FFFF8h
80506408 8b91040000c0    mov     edx,dword ptr [ecx-3FFFFFFCh]
8050640e 81e900000040    sub     ecx,40000000h
80506414 8b01            mov     eax,dword ptr [ecx]
80506416 8bc8            mov     ecx,eax
80506418 83e101          and     ecx,1
8050641b 33f6            xor     esi,esi
8050641d 0bce            or      ecx,esi
8050641f 7506            jne     nt!MmGetPhysicalAddress+0x83 (80506427)

nt!MmGetPhysicalAddress+0x7d:
80506421 33c0            xor     eax,eax
80506423 33d2            xor     edx,edx
80506425 eb1f            jmp     nt!MmGetPhysicalAddress+0xa2 (80506446)

nt!MmGetPhysicalAddress+0x83:
80506427 0facd00c        shrd    eax,edx,0Ch
8050642b c1ea0c          shr     edx,0Ch
8050642e 25ffffff03      and     eax,3FFFFFFh

nt!MmGetPhysicalAddress+0x8f:
80506433 33c9            xor     ecx,ecx
80506435 0fa4c10c        shld    ecx,eax,0Ch
80506439 c1e00c          shl     eax,0Ch
8050643c 81e7ff0f0000    and     edi,0FFFh
80506442 03c7            add     eax,edi
80506444 8bd1            mov     edx,ecx

nt!MmGetPhysicalAddress+0xa2:
80506446 5f              pop     edi
80506447 5e              pop     esi
80506448 5b              pop     ebx
80506449 5d              pop     ebp
8050644a c20400          ret     4
```

逆向分析就是这样，看起来所有的指令都懂，稍一分析就容易乱。要想了解这段代码中的跳转，需要看一下PDT的结构。

```
lkd> dt _hardware_pte
nt!_HARDWARE_PTE
   +0x000 Valid            : Pos 0, 1 Bit
   +0x000 Write            : Pos 1, 1 Bit
   +0x000 Owner            : Pos 2, 1 Bit
   +0x000 WriteThrough     : Pos 3, 1 Bit
   +0x000 CacheDisable     : Pos 4, 1 Bit
   +0x000 Accessed         : Pos 5, 1 Bit
   +0x000 Dirty            : Pos 6, 1 Bit
   +0x000 LargePage        : Pos 7, 1 Bit
   +0x000 Global           : Pos 8, 1 Bit
   +0x000 CopyOnWrite      : Pos 9, 1 Bit
   +0x000 Prototype        : Pos 10, 1 Bit
   +0x000 reserved0        : Pos 11, 1 Bit
   +0x000 PageFrameNumber  : Pos 12, 26 Bits
   +0x000 reserved1        : Pos 38, 26 Bits
   +0x000 LowPart          : Uint4B
   +0x004 HighPart         : Uint4B
```

很容易看出来是64位的union联合体。启用了PAE，物理地址扩展。

### 前期处理过程分析

这里我直接给出注释和代码结合的格式。前期的判断处理过程：

```
805063ac 8b7d08          mov     edi,dword ptr [ebp+8]                        
// edi为参数一，目测是传入的线性地址
805063af 8bcf            mov     ecx,edi                                      
805063b1 c1e912          shr     ecx,12h                                      
// 右移18位，即取高14位
805063b4 81e1f83f0000    and     ecx,3FF8h                                    
// 3FF8二进制为11111111111000，ecx = 线性地址的21-31位+3个0
805063ba 8b81000060c0    mov     eax,dword ptr [ecx-3FA00000h]
// ecx - BASE肯定会溢出，所以eax = [C0600000 + ecx]
805063c0 8b89040060c0    mov     ecx,dword ptr [ecx-3F9FFFFCh]
// 同上，ecx = [C0600000 + 4 + ecx],
// C0600000是VA为0的PDE的位置
// eax是PDE低21位，根据线性地址高11位确定
// ecx是PDE高32位
// ecx:eax = PDE = (线性地址 >> 18)&amp;3FF8h + C0600000


805063c6 be81000000      mov     esi,81h
// 81二进制为10000001
805063cb 8bd0            mov     edx,eax
805063cd 23d6            and     edx,esi                                      
// 取第7位和第0位，分别是LargePage位和Valid位
805063cf 33db            xor     ebx,ebx
805063d1 3bd6            cmp     edx,esi
// 判断是否启用LargePage
805063d3 751f            jne     nt!MmGetPhysicalAddress+0x50 (805063f4)

nt!MmGetPhysicalAddress+0x31:
805063d5 85db            test    ebx,ebx
805063d7 751b            jne     nt!MmGetPhysicalAddress+0x50 (805063f4)
// nt!MmGetPhysicalAddress+0x50不启用LargePage
```

### 启用LargePage的情况

这里根据是否启用LargePage分成两种情况，为了方便查看和分析，我把它们剪切开进行分析:

```
nt!MmGetPhysicalAddress+0x35:
805063d9 0facc80c        shrd    eax,ecx,0Ch                                   
// eax右移12位，同时ecx的低12位充当eax高12位
// 这一步的作用首先是去除eax的低12个标志位
// 然后将ecx的低12位有效位和eax合成
// 事实上，ecx只有4位是有效的，其他的都是保留位。

805063dd c1e90c          shr     ecx,0Ch
805063e0 8bcf            mov     ecx,edi
// ecx赋值为edi，即线性地址
805063e2 c1e90c          shr     ecx,0Ch
// ecx右移12位
805063e5 25ffffff03      and     eax,3FFFFFFh
// 取eax低26位
805063ea 81e1ff010000    and     ecx,1FFh
// 取ecx低9位，即线性地址 12~20位
805063f0 03c1            add     eax,ecx
// eax = eax + ecx 得到PTE
805063f2 eb3f            jmp     nt!MmGetPhysicalAddress+0x8f (80506433)

&hellip;&hellip;

nt!MmGetPhysicalAddress+0x8f:
80506433 33c9            xor     ecx,ecx
80506435 0fa4c10c        shld    ecx,eax,0Ch
// eax的高12位充当ecx低12位
80506439 c1e00c          shl     eax,0Ch
// eax左移12位
8050643c 81e7ff0f0000    and     edi,0FFFh
// 取edi低12位
80506442 03c7            add     eax,edi
// eax = eax + edi
80506444 8bd1            mov     edx,ecx
```

Windows xp下PAE开启且LargePage标志为1时线性地址到物理地址转化关系。由线性地址高11位确定PDE，结合线性地址中间9位确定PTE，低12位是偏移地址。

### 不启用LargePage的情况

```
nt!MmGetPhysicalAddress+0x50:
805063f4 83e001          and     eax,1
805063f7 33c9            xor     ecx,ecx
805063f9 0bc1            or      eax,ecx
805063fb 7424            je      nt!MmGetPhysicalAddress+0x7d (80506421)

nt!MmGetPhysicalAddress+0x59:
805063fd 8bcf            mov     ecx,edi
805063ff c1e909          shr     ecx,9
// 右移9位
80506402 81e1f8ff7f00    and     ecx,7FFFF8h
// 11111111111111111111000
// 取12～31位且另9～11位为0

80506408 8b91040000c0    mov     edx,dword ptr [ecx-3FFFFFFCh]
// 也是找PDE，PDE = C0000004 + ecx
// edx = PTE高32位 = (线性地址 >> 9)&amp;7FFFF8 + C0000004

8050640e 81e900000040    sub     ecx,40000000h
// ecx = C0000000 + ecx = C0000000
// ecx = PTE = (线性地址 >> 9)&amp;7FFFF8 + C0000000
// C0000000是PTE基址

80506414 8b01            mov     eax,dword ptr [ecx]
80506416 8bc8            mov     ecx,eax
80506418 83e101          and     ecx,1
8050641b 33f6            xor     esi,esi
8050641d 0bce            or      ecx,esi
8050641f 7506            jne     nt!MmGetPhysicalAddress+0x83 (80506427)
// 判断是否valid

nt!MmGetPhysicalAddress+0x7d:
80506421 33c0            xor     eax,eax
80506423 33d2            xor     edx,edx
80506425 eb1f            jmp     nt!MmGetPhysicalAddress+0xa2 (80506446)

nt!MmGetPhysicalAddress+0x83:
80506427 0facd00c        shrd    eax,edx,0Ch
// eax右移12位移出标志位，然后另edx低12位充当eax高12位

8050642b c1ea0c          shr     edx,0Ch
8050642e 25ffffff03      and     eax,3FFFFFFh
// 取eax低26位

nt!MmGetPhysicalAddress+0x8f:
80506433 33c9            xor     ecx,ecx
80506435 0fa4c10c        shld    ecx,eax,0Ch
// eax的高12位充当ecx低12位
80506439 c1e00c          shl     eax,0Ch
// eax左移12位
8050643c 81e7ff0f0000    and     edi,0FFFh
// 取edi低12位
80506442 03c7            add     eax,edi
// eax = eax + edi
80506444 8bd1            mov     edx,ecx
```

未启用LargePage的时候，根据线性地址12～31位确定PTE，然后与线性地址0～11位相加得到物理地址。

### 最后

在我开始分析它的时候并没有觉得会很麻烦，自认为自己了解了转换关系。但是当我开始一句一句查看的时候，才觉得自己还是不那么确切的清楚每个概念。很是惭愧啊。

这里有[一篇不错的文章](http://bbs.pediy.com/showthread.php?t=61327)，分析了MmIsValidAddress，我分析这个函数到有些迷茫的时候，那篇文章提供了足够的信息让我想明白一些问题。
