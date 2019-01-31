
---
title: "ProbeForRead和ProbeForWrite"
date: 2014-07-17T09:13:30+08:00
draft: true
categories: ["Windows安全"]
tags: ["Windows安全"]
topics: ["Windows安全"]
---

WRK中这两个函数的实现：

<!--more-->


```

VOID
ProbeForWrite (
    __inout_bcount(Length) PVOID Address,
    __in SIZE_T Length,
    __in ULONG Alignment
    )
{
    ULONG_PTR EndAddress;
    ULONG_PTR StartAddress;

#define PageSize PAGE_SIZE

    if (Length != 0) {
        ASSERT((Alignment == 1) || (Alignment == 2) ||
               (Alignment == 4) || (Alignment == 8) ||
               (Alignment == 16));
        StartAddress = (ULONG_PTR)Address;
        if ((StartAddress & (Alignment - 1)) == 0) {
            EndAddress = StartAddress + Length - 1;
            if ((StartAddress <= EndAddress) &&
                (EndAddress < MM_USER_PROBE_ADDRESS)) {
                //
                // If this is a Wow64 process, then the native page is 4K, which
                // could be smaller than the native page size/
                //
                EndAddress = (EndAddress & ~(PageSize - 1)) + PageSize;
                do {
                    *(volatile CHAR *)StartAddress = *(volatile CHAR *)StartAddress;
                    StartAddress = (StartAddress & ~(PageSize - 1)) + PageSize;
                } while (StartAddress != EndAddress);
                return;
            } else {
                ExRaiseAccessViolation();
            }
        } else {
            ExRaiseDatatypeMisalignment();
        }
    }
    return;
}

VOID
ProbeForRead(
    __in_bcount(Length) VOID *Address,
    __in SIZE_T Length,
    __in ULONG Alignment
    )
{
    PAGED_CODE();
    ASSERT((Alignment == 1) || (Alignment == 2) ||
           (Alignment == 4) || (Alignment == 8) ||
           (Alignment == 16));
    if (Length != 0) {
        if (((ULONG_PTR)Address & (Alignment - 1)) != 0) {
            ExRaiseDatatypeMisalignment();
        } else if ((((ULONG_PTR)Address + Length) > (ULONG_PTR)MM_USER_PROBE_ADDRESS) ||
                   (((ULONG_PTR)Address + Length) < (ULONG_PTR)Address)) {
            *(volatile UCHAR * const)MM_USER_PROBE_ADDRESS = 0;
        }
    }
}

```

可以看到这两个函数仅仅是检测传入的地址是否对齐、是否在用户地址空间，*Write的还检测是否可写，如果出现意外就抛出异常，所以应该配合__try和__except使用。如果传入内核地址，将会产生异常，try同时会捕获该异常。

这两个函数的使用常常会造成一些危险，因为它们的检测非常简单，而过度依赖或者认为它们很强大，会使恶意软件绕过防护。比如捕获到对齐错误时，即STATUS_DATATYPE_MISALIGNMENT异常，这时不做进一步检查直接放行，就会出问题；对length==0的检查也一样容易出问题。
