
---
title: "rootkit相关硬件知识"
date: 2014-04-10T09:13:30+08:00
draft: true
categories: ["Windows安全"]
tags: ["Windows安全"]
topics: ["Windows安全"]
---

Intel x86通过环实现访问控制，ring0～ring3四个级别，不同特权的程序运行在不同的环，高环程序不能访问低环数据，强行访问可能产生中断；同样，有些指令只能在特定环执行，如cli、sti、in、out只能运行在ring0。这是在硬件层面的权限控制，但是Windows直到NT系列内核才真正将CPU环机制利用起来，为了兼容，只使用ring0和ring3，内核模式在ring0，用户模式在ring3。操作系统以Intel X86环机制为基础，实现自己的访问控制，只允许某些特定ring3代码访问ring0。

<!--more-->

由于访问控制的根本还是需要CPU支持，所以讨论ring0不得不涉及大量的硬件特性。

CPU会制定某些策略，决定中断处理、硬件信号处理、软件崩溃处理、用户模式和内核模式通信、多线程切换执行操作等。操作系统需要处理这些事件，但是CPU总是最先捕获，并且以特定方式告知操作系统。为了知道需要在特定事件发生时执行什么操作，CPU维持一些表，表中含有某些软件例程。这些表只能保存在内存中，所以CPU只有使用特定寄存器来记录表在内存的基地址。

这样的表有GDT，LDT，IDT，页目录等，操作系统也会保存一些表，如SSDT和Shadow SSDT。

## 内存页
全部内存划分成页面，页面大小为4K或4M或2M，具体取决于是否启用PAE和LargePage是否启用。

进程访问某个页面，CPU会进行一系列权限检查。首先检查段描述符的描述符权限级别（DPL），如果DPL低于当前进程的权限级别，拒绝访问；然后对页目录U位进行检查，该位为0只允许ring0、ring1和ring2级别程序访问；最后检查页表的U位以及页表是否可写可执行等。

具体的操作系统实现并不一定完全按照上面的模式，因为Windows只使用了ring0和ring3两个级别，检查起来相对简单一些。

Windows使用分页机制实现虚拟地址到物理地址转换，通过页目录表、页目录、页表和页内偏移找到物理地址。多个进程使用多个页目录表，即多个进程有各自的CR3值，线程切换的时候CR3的值也可能变化，取决于是否是跨进程的线程切换。因此可以互不影响的使用0～7fffffff虚拟地址（这里仅对于32操作系统）。页目录表地址在进程的KPROCESS结构中。

## Windows的进程和线程
Windows内核对线程进行调度，并不直接调度进程。进程是线程共享数据的一种方式，线程共享CR3值、访问令牌、SID、句柄、物理页面等。这与linux有很大的不同。

## 描述符表
Intel x86有GDT全局描述符表、LDT局部描述符表和IDT中断描述符表。

全局描述符表GDT记录了所有描述符，包括段的基地址和大小，还有权限信息等。CPU的GDTR寄存器保存了GDT的基址，可以通过SGDT获取寄存器数据，通过LGDT修改GDTR寄存器。Windows和linux并没有使用LDT。段选择子或者段寄存器可以作为GDT索引，来查找特定段。GDT中还有一种称为调用门的描述符，用来实现跨段调用。

中断描述符表IDT记录了中断发生时需要调用的函数，是256个8字节项组成的数组。CPU的IDTR寄存器保存IDT基地址，多核处理器有多个IDTR，通过LIDT和SIDT读取和修改IDTR。当中断发生时，通过中断号引用中断描述符表的项，从而执行特定中断服务例程（ISR），这些项称为中断门。通过中断门，用户进程可以调用内核模式的例程，通过0x2E中断。IDT除了中断门还有任务门和陷阱门，陷阱门与中断门区别仅在于是否能被可屏蔽中断中断，任务门并不被Windows大量使用。

## 系统服务调度表
系统服务调度表用于查询处理特定系统函数，Windows使用0x2E中断或者sysenter执行系统调用。执行系统调用时内核中调用KiSystemService函数，读取EAX中的系统调用编号，进而在SSDT中查找，EDX中保存用户层传来的参数。

## CR0和EFLAGS
CR0称为控制寄存器，保存了内存访问保护机制相关的一些信息位，如是否启用内存写保护。通过修改CR0可以关闭写保护，这也是rootkit中最常见的操作之一。

EFLAGS记录了当前IO特权级、中断标志、陷阱标志等，也是rootkit常常修改和利用的寄存器。
