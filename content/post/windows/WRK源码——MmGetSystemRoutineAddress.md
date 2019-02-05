
---
title: "WRK源码——MmGetSystemRoutineAddress"
date: 2014-07-24T09:13:30+08:00
draft: true
categories: ["Windows安全"]
tags: ["Windows安全"]
topics: ["Windows安全"]
---

MmGetSystemRoutineAddress 只能查找HAL.dll 和 ntoskrnl.exe导出的函数。查找过程很简单，遍历PsLoadedModuleList链表，找到每一个节点对应的KLDR_DATA_TABLE_ENTRY结构，找到BaseDllName = hal.dll 和 ntoskrnl.exe 的基址，最后查找导出表。

<!--more-->

代码如下：


```

NTKERNELAPI
PVOID
MmGetSystemRoutineAddress (
    __in PUNICODE_STRING SystemRoutineName
    )
{
    PKTHREAD CurrentThread;
    NTSTATUS Status;
    PKLDR_DATA_TABLE_ENTRY DataTableEntry;
    ANSI_STRING AnsiString;
    PLIST_ENTRY NextEntry;
    UNICODE_STRING KernelString;
    UNICODE_STRING HalString;
    PVOID FunctionAddress;
    LOGICAL Found;
    ULONG EntriesChecked;

    ASSERT (KeGetCurrentIrql() == PASSIVE_LEVEL);

    EntriesChecked = 0;
    FunctionAddress = NULL;

    KernelString.Buffer = (const PUSHORT) KERNEL_NAME;
    KernelString.Length = sizeof (KERNEL_NAME) - sizeof (WCHAR);
    KernelString.MaximumLength = sizeof KERNEL_NAME;

    HalString.Buffer = (const PUSHORT) HAL_NAME;
    HalString.Length = sizeof (HAL_NAME) - sizeof (WCHAR);
    HalString.MaximumLength = sizeof HAL_NAME;

    do {
        Status = RtlUnicodeStringToAnsiString (&AnsiString,
                                               SystemRoutineName,
                                               TRUE);
        if (NT_SUCCESS (Status)) {
            break;
        }
        KeDelayExecutionThread (KernelMode, FALSE, (PLARGE_INTEGER)&MmShortTime);
    } while (TRUE);

    CurrentThread = KeGetCurrentThread ();
    KeEnterCriticalRegionThread (CurrentThread);
    ExAcquireResourceSharedLite (&PsLoadedModuleResource, TRUE);

    NextEntry = PsLoadedModuleList.Flink;
    while (NextEntry != &PsLoadedModuleList) {

        Found = FALSE;

        DataTableEntry = CONTAINING_RECORD(NextEntry,
                                           KLDR_DATA_TABLE_ENTRY,
                                           InLoadOrderLinks);
        if (RtlEqualUnicodeString (&KernelString,
                                   &DataTableEntry-> BaseDllName,
                                   TRUE)) {
            Found = TRUE;
            EntriesChecked += 1;
        }
        else if (RtlEqualUnicodeString (&HalString,
                                        &DataTableEntry-> BaseDllName,
                                        TRUE)) {
            Found = TRUE;
            EntriesChecked += 1;
        }

        if (Found == TRUE) {
            FunctionAddress = MiFindExportedRoutineByName (DataTableEntry-> DllBase,
                                                           &AnsiString);
            if (FunctionAddress != NULL) {
                break;
            }
            if (EntriesChecked == 2) {
                break;
            }
        }
        NextEntry = NextEntry-> Flink;
    }

    ExReleaseResourceLite (&PsLoadedModuleResource);
    KeLeaveCriticalRegionThread (CurrentThread);

    RtlFreeAnsiString (&AnsiString);

    return FunctionAddress;
}


```


```

PVOID
MiFindExportedRoutineByName (
    IN PVOID DllBase,
    IN PANSI_STRING AnsiImageRoutineName
    )
{
    USHORT OrdinalNumber;
    PULONG NameTableBase;
    PUSHORT NameOrdinalTableBase;
    PULONG Addr;
    LONG High;
    LONG Low;
    LONG Middle;
    LONG Result;
    ULONG ExportSize;
    PVOID FunctionAddress;
    PIMAGE_EXPORT_DIRECTORY ExportDirectory;

    PAGED_CODE();

    ExportDirectory = (PIMAGE_EXPORT_DIRECTORY) RtlImageDirectoryEntryToData (
                                DllBase,
                                TRUE,
                                IMAGE_DIRECTORY_ENTRY_EXPORT,
                                &ExportSize);
    if (ExportDirectory == NULL) {
        return NULL;
    }

    NameTableBase = (PULONG)((PCHAR)DllBase + (ULONG)ExportDirectory-> AddressOfNames);
    NameOrdinalTableBase = (PUSHORT)((PCHAR)DllBase + (ULONG)ExportDirectory-> AddressOfNameOrdinals);

    Low = 0;
    Middle = 0;
    High = ExportDirectory-> NumberOfNames - 1;

    while (High > = Low) {
        Middle = (Low + High) > > 1;
        Result = strcmp (AnsiImageRoutineName-> Buffer,
                         (PCHAR)DllBase + NameTableBase[Middle]);
        if (Result < 0) {
            High = Middle - 1;
        }
        else if (Result > 0) {
            Low = Middle + 1;
        }
        else {
            break;
        }
    }

    if (High < Low) {
        return NULL;
    }
    OrdinalNumber = NameOrdinalTableBase[Middle];
    if ((ULONG)OrdinalNumber > = ExportDirectory-> NumberOfFunctions) {
        return NULL;
    }
    Addr = (PULONG)((PCHAR)DllBase + (ULONG)ExportDirectory-> AddressOfFunctions);
    FunctionAddress = (PVOID)((PCHAR)DllBase + Addr[OrdinalNumber]);
    ASSERT ((FunctionAddress <= (PVOID)ExportDirectory) ||
            (FunctionAddress > = (PVOID)((PCHAR)ExportDirectory + ExportSize)));
			
    return FunctionAddress;
}


```


