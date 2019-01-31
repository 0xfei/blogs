
---
title: "通过SECTION对象得到文件名"
date: 2014-10-28T09:13:30+08:00
draft: true
categories: ["Windows安全"]
tags: ["Windows安全"]
topics: ["Windows安全"]
---

进程创建、dll加载，都要用到section。我们可以hook相关的函数，比如ZwMapViewOfSection，替换section。可以通过section_object得到对应的文件对象。

<!--more-->

```

lkd> dt _eprocess 8248da78
ntdll!_EPROCESS
   +0x000 Pcb              : _KPROCESS
   ....
   +0x138 SectionObject    : 0xe1ad2158 Void
   +0x13c SectionBaseAddress : 0x01000000 Void

lkd> dt _section_object 0xe1ad2158
nt!_SECTION_OBJECT
   +0x000 StartingVa       : (null) 
   +0x004 EndingVa         : (null) 
   +0x008 Parent           : (null) 
   +0x00c LeftChild        : (null) 
   +0x010 RightChild       : (null) 
   +0x014 Segment          : 0xe11166d8 _SEGMENT_OBJECT

lkd> dt _SEGMENT_OBJECT 0xe11166d8
nt!_SEGMENT_OBJECT
   +0x000 BaseAddress      : 0x822c7e08 Void
   +0x004 TotalNumberOfPtes : 0x97
   +0x008 SizeOfSegment    : _LARGE_INTEGER 0x97
   +0x010 NonExtendedPtes  : 0x97000
   +0x014 ImageCommitment  : 0
   +0x018 ControlArea      : 0x00000420 _CONTROL_AREA
   +0x01c Subsection       : 0x822c7eb8 _SUBSECTION
   +0x020 LargeControlArea : (null) 
   +0x024 MmSectionFlags   : (null) 
   +0x028 MmSubSectionFlags : (null)
 
lkd> dt _CONTROL_AREA 0x822c7e08
nt!_CONTROL_AREA
   +0x000 Segment          : 0xe11166d8 _SEGMENT
   +0x004 DereferenceList  : _LIST_ENTRY [ 0x0 - 0x0 ]
   +0x00c NumberOfSectionReferences : 1
   +0x010 NumberOfPfnReferences : 0x7b
   +0x014 NumberOfMappedViews : 1
   +0x018 NumberOfSubsections : 5
   +0x01a FlushInProgressCount : 0
   +0x01c NumberOfUserReferences : 2
   +0x020 u                : __unnamed
   +0x024 FilePointer      : 0x82101ad0 _FILE_OBJECT
   +0x028 WaitingForDeletion : (null) 
   +0x02c ModifiedWriteCount : 0
   +0x02e NumberOfSystemCacheViews : 0

lkd> dt _file_object 0x82101ad0
ntdll!_FILE_OBJECT
   +0x000 Type             : 0n5
   +0x002 Size             : 0n112
   +0x004 DeviceObject     : 0x82539900 _DEVICE_OBJECT
   +0x008 Vpb              : 0x8246e288 _VPB
   +0x00c FsContext        : 0xe22cf990 Void
   +0x010 FsContext2       : 0xe22cfae8 Void
   +0x014 SectionObjectPointer : 0x8235dc5c _SECTION_OBJECT_POINTERS
   +0x018 PrivateCacheMap  : (null) 
   +0x01c FinalStatus      : 0n0
   +0x020 RelatedFileObject : (null) 
   +0x024 LockOperation    : 0 ''
   +0x025 DeletePending    : 0 ''
   +0x026 ReadAccess       : 0x1 ''
   +0x027 WriteAccess      : 0 ''
   +0x028 DeleteAccess     : 0 ''
   +0x029 SharedRead       : 0x1 ''
   +0x02a SharedWrite      : 0 ''
   +0x02b SharedDelete     : 0x1 ''
   +0x02c Flags            : 0x44042
   +0x030 FileName         : _UNICODE_STRING "\WinDDK\7600.16385.1\Debuggers\windbg.exe"
   +0x038 CurrentByteOffset : _LARGE_INTEGER 0x0
   +0x040 Waiters          : 0
   +0x044 Busy             : 0
   +0x048 LastLock         : (null) 
   +0x04c Lock             : _KEVENT
   +0x05c Event            : _KEVENT
   +0x06c CompletionContext : (null) 


lkd> dt _SEGMENT_OBJECT 0xe11166d8
nt!_SEGMENT_OBJECT
   +0x000 BaseAddress      : 0x822c7e08 Void
这个BaseAddress指向一个CONTROL_AREA结构。


```

据说微软的符号表出了问题，SECTION_OBJECT的最后一个成员不是SEGMENT_OBJECT结构，而是SEGMENT结构。这么说是有道理的，我们看看把它解析成SEGMENT的情况：


```

lkd> dt _segment 0xe11166d8
nt!_SEGMENT
   +0x000 ControlArea      : 0x822c7e08 _CONTROL_AREA
   +0x004 TotalNumberOfPtes : 0x97
   +0x008 NonExtendedPtes  : 0x97
   +0x00c WritableUserReferences : 0
   +0x010 SizeOfSegment    : 0x97000
   +0x018 SegmentPteTemplate : _MMPTE
   +0x020 NumberOfCommittedPages : 0
   +0x024 ExtendInfo       : (null) 
   +0x028 SystemImageBase  : (null) 
   +0x02c BasedAddress     : 0x01000000 Void
   +0x030 u1               : __unnamed
   +0x034 u2               : __unnamed
   +0x038 PrototypePte     : 0xe1116718 _MMPTE
   +0x040 ThePtes          : [1] _MMPTE
lkd> dt _CONTROL_AREA 0x822c7e08
nt!_CONTROL_AREA
   +0x000 Segment          : 0xe11166d8 _SEGMENT
   +0x004 DereferenceList  : _LIST_ENTRY [ 0x0 - 0x0 ]
   +0x00c NumberOfSectionReferences : 1
   +0x010 NumberOfPfnReferences : 0x7b
   +0x014 NumberOfMappedViews : 1
   +0x018 NumberOfSubsections : 5
   +0x01a FlushInProgressCount : 0
   +0x01c NumberOfUserReferences : 2
   +0x020 u                : __unnamed
   +0x024 FilePointer      : 0x82101ad0 _FILE_OBJECT
   +0x028 WaitingForDeletion : (null) 
   +0x02c ModifiedWriteCount : 0
   +0x02e NumberOfSystemCacheViews : 0


```

顺眼了不少，不知道微软怎么搞的。
