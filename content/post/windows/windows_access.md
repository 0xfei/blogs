---
title: "Windows访问控制"
date: 2014-03-23T09:13:30+08:00
draft: true
description: "security and HTTPS"
categories: ["Windows安全"]
tags: ["Windows安全"]
topics: ["Windows安全"]
---

Access Control Module，访问控制模型。是Windows用来约束线程对安全对象访问的控制模型。可以分为两部分，线程的访问令牌access token和安全对象的安全描述符security descriptor。

<!--more-->

## 访问令牌access token

令牌也是一个内核对象，用于表示进程或线程的安全上下文。包括特权和一些标识，成功登陆系统的用户，将由系统分配一个访问令牌，该用户创建的所有进程继承该令牌。

access token主要包含这些信息：

* 用户账户的Sid；
* 用户所属组的Sid；
* 登陆Sid标识当前登陆会话；
* 用户和用户组特权列表；
* 用户创建的进程的默认DACL；
* access token类型和来源；
* 其他信息；

我们用windbg的 dt !_token命令看一下：

```
dt _token
nt!_TOKEN
   +0x000 TokenSource      : _TOKEN_SOURCE
   +0x010 TokenId          : _LUID
   +0x018 AuthenticationId : _LUID
   +0x020 ParentTokenId    : _LUID
   +0x028 ExpirationTime   : _LARGE_INTEGER
   +0x030 TokenLock        : Ptr32 _ERESOURCE
   +0x038 AuditPolicy      : _SEP_AUDIT_POLICY
   +0x040 ModifiedId       : _LUID
   +0x048 SessionId        : Uint4B
   +0x04c UserAndGroupCount : Uint4B
   +0x050 RestrictedSidCount : Uint4B
   +0x054 PrivilegeCount   : Uint4B
   +0x058 VariableLength   : Uint4B
   +0x05c DynamicCharged   : Uint4B
   +0x060 DynamicAvailable : Uint4B
   +0x064 DefaultOwnerIndex : Uint4B
   +0x068 UserAndGroups    : Ptr32 _SID_AND_ATTRIBUTES
   +0x06c RestrictedSids   : Ptr32 _SID_AND_ATTRIBUTES
   +0x070 PrimaryGroup     : Ptr32 Void
   +0x074 Privileges       : Ptr32 _LUID_AND_ATTRIBUTES
   +0x078 DynamicPart      : Ptr32 Uint4B
   +0x07c DefaultDacl      : Ptr32 _ACL
   +0x080 TokenType        : _TOKEN_TYPE
   +0x084 ImpersonationLevel : _SECURITY_IMPERSONATION_LEVEL
   +0x088 TokenFlags       : Uint4B
   +0x08c TokenInUse       : UChar
   +0x090 ProxyData        : Ptr32 _SECURITY_TOKEN_PROXY_DATA
   +0x094 AuditData        : Ptr32 _SECURITY_TOKEN_AUDIT_DATA
   +0x098 OriginatingLogonSession : _LUID
   +0x0a0 VariablePart     : Uint4B
```

可以看到Token还是包含了很多信息的。刚才我们说到Token的种类，其实Token可以分为两种：主Token和模拟Token。主Token是进程默认Token，模拟Token往往是线程为了执行某些任务模拟其他线程的Token，得到某些权限。接下来看一个具体的Token，windows xp sp2下的notepad。

```
!process 0 1 notepad.exe
PROCESS 8237dc70  SessionId: 0  Cid: 0834    Peb: 7ffde000  ParentCid: 0754
    DirBase: 09c804a0  ObjectTable: e112b7f8  HandleCount:  44.
    Image: notepad.exe
    VadRoot 820e2460 Vads 66 Clone 0 Private 208. Modified 13. Locked 0.
    DeviceMap e1c51418
    Token                             e265a030
    ElapsedTime                       00:00:15.219
    UserTime                          00:00:00.015
    KernelTime                        00:00:00.062
    QuotaPoolUsage[PagedPool]         62772
    QuotaPoolUsage[NonPagedPool]      2640
    Working Set Sizes (now,min,max)  (948, 50, 345) (3792KB, 200KB, 1380KB)
    PeakWorkingSetSize                953
    VirtualSize                       31 Mb
    PeakVirtualSize                   36 Mb
    PageFaultCount                    1001
    MemoryPriority                    BACKGROUND
    BasePriority                      8
    CommitCharge                      404

lkd-> !token e265a030
_TOKEN e265a030
TS Session ID: 0
User: S-1-5-21-790525478-1844823847-1801674531-500
Groups: 
 00 S-1-5-21-790525478-1844823847-1801674531-513
    Attributes - Mandatory Default Enabled 
 01 S-1-1-0
    Attributes - Mandatory Default Enabled 
 02 S-1-5-32-544
    Attributes - Mandatory Default Enabled Owner 
 03 S-1-5-32-545
    Attributes - Mandatory Default Enabled 
 04 S-1-5-4
    Attributes - Mandatory Default Enabled 
 05 S-1-5-11
    Attributes - Mandatory Default Enabled 
 06 S-1-5-5-0-64280
    Attributes - Mandatory Default Enabled LogonId 
 07 S-1-2-0
    Attributes - Mandatory Default Enabled 
Primary Group: S-1-5-21-790525478-1844823847-1801674531-513
Privs: 
 00 0x000000017 SeChangeNotifyPrivilege           Attributes - Enabled Default 
 01 0x000000008 SeSecurityPrivilege               Attributes - 
 02 0x000000011 SeBackupPrivilege                 Attributes - 
 03 0x000000012 SeRestorePrivilege                Attributes - 
 04 0x00000000c SeSystemtimePrivilege             Attributes - 
 05 0x000000013 SeShutdownPrivilege               Attributes - 
 06 0x000000018 SeRemoteShutdownPrivilege         Attributes - 
 07 0x000000009 SeTakeOwnershipPrivilege          Attributes - 
 08 0x000000014 SeDebugPrivilege                  Attributes - 
 09 0x000000016 SeSystemEnvironmentPrivilege      Attributes - 
 10 0x00000000b SeSystemProfilePrivilege          Attributes - 
 11 0x00000000d SeProfileSingleProcessPrivilege   Attributes - 
 12 0x00000000e SeIncreaseBasePriorityPrivilege   Attributes - 
 13 0x00000000a SeLoadDriverPrivilege             Attributes - Enabled 
 14 0x00000000f SeCreatePagefilePrivilege         Attributes - 
 15 0x000000005 SeIncreaseQuotaPrivilege          Attributes - 
 16 0x000000019 SeUndockPrivilege                 Attributes - Enabled 
 17 0x00000001c SeManageVolumePrivilege           Attributes - 
 18 0x00000001d SeImpersonatePrivilege            Attributes - Enabled Default 
 19 0x00000001e SeCreateGlobalPrivilege           Attributes - Enabled Default 
Authentication ID:         (0,10166)
Impersonation Level:       Anonymous
TokenType:                 Primary
Source: User32             TokenFlags: 0x89 ( Token in use )
Token ID: 4a319a           ParentToken ID: 0
Modified ID:               (0, 4a319c)
RestrictedSidCount: 0      RestrictedSids: 00000000
```

也可以看到notepad可能拥有的权限，Enabled表示被允许。如果有些特权不在列表中，那么是不可能被拥有的，除非采用一些特殊的手段。

## 安全描述符security descriptor

如果写过Windows程序的话会知道，在创建内核对象的时候需要一个安全描述符。安全描述符用于控制对象的安全访问。

安全描述符的结构可以用windbg dt _security_descriptor查看。

```
lkd-> dt _security_descriptor
nt!_SECURITY_DESCRIPTOR
   +0x000 Revision         : UChar
   +0x001 Sbz1             : UChar
   +0x002 Control          : Uint2B
   +0x004 Owner            : Ptr32 Void
   +0x008 Group            : Ptr32 Void
   +0x00c Sacl             : Ptr32 _ACL
   +0x010 Dacl             : Ptr32 _ACL
```

关键的成员是SACL和DACL。DACL我们在上文提到过。为了说清楚DLAC以及访问控制模型的精华部分，不得不多提一些东西。

### SID

SID即security identifier，系统为每个account分配一个唯一的SID。这里的account包括用户、用户组等，并不是狭义的用户。你只要知道一个用户就对应一个SID，比如当用户登陆的时候会根据SID判断用户是否正确输入密码等。

### ACL

ACL即Access Control List，访问控制列表，是由ACE即Access Control Entry访问控制入口组成的链表。ACE包含允许访问所属对象的所有账户的SID及该SID所有用的对该对象的操作权限。结构如下：

```
lkd-> dt _acl
nt!_ACL
   +0x000 AclRevision      : UChar
   +0x001 Sbz1             : UChar
   +0x002 AclSize          : Uint2B
   +0x004 AceCount         : Uint2B
   +0x006 Sbz2             : Uint2B
```

安全描述符有两个ACL结构，SACL用于审计audit，而DACL是我们关心的重点，用于控制线程对对象的访问。线程的SID继承自创建线程所属进程的用户。那么就有了如下的访问规则：

1.DACL==NULL，线程拥有对象的全部访问权限；
2.AceCount=0，即没有ACE，则拒绝所有访问；
3.遍历所有ACE，如果找不到与线程所属用户SID相关的ACE，拒绝访问；
4.找到ACE，进行权限判断。

这就是访问控制模型的大致理论。通过安全描述符和访问令牌，相互对应达到对内核对象的访问控制。

## enable 权限，即提权

上面的notepad的token，可以看到有的特权是enabled，表示被允许。如果我们想让用户拥有某些默认不被允许的特权，就需要提权操作。其实就是几个API的使用。

OpenProcessToken打开进程的Token对象，三个参数含义显而易见。DesiredAccess表示期望的权限。对于添加特权，我们需要的权限至少是TOKEN_QUERY | TOKEN_ADJUST_PRIVILEGES。进程对象需要以PROCESS_QUERY_INFORMATION打开。

```
BOOL WINAPI OpenProcessToken(
  _In_   HANDLE ProcessHandle,
  _In_   DWORD DesiredAccess,
  _Out_  PHANDLE TokenHandle
);
```

LookupPrivilegeValue用于打开特定系统lpName权限的LUID，即locally unique identifier，用于标识系统中的某个特权。lpSystemName为Null表示本地系统。

```
BOOL WINAPI LookupPrivilegeValue(
  _In_opt_  LPCTSTR lpSystemName,
  _In_      LPCTSTR lpName,
  _Out_     PLUID lpLuid
);
```

AdjustTokenPrivileges用于调整token特权，第二个参数有点儿奇怪，如果是True的话表示禁止所有特权。PTOKEN_PRIVILEGES相当于一个已计数的LUID_AND_ATTRIBUTES数组。LUID_AND_ATTRIBUTES表示希望允许或禁止LUID代表的特权。

```
BOOL WINAPI AdjustTokenPrivileges(
  _In_       HANDLE TokenHandle,
  _In_       BOOL DisableAllPrivileges,
  _In_opt_   PTOKEN_PRIVILEGES NewState,
  _In_       DWORD BufferLength,
  _Out_opt_  PTOKEN_PRIVILEGES PreviousState,
  _Out_opt_  PDWORD ReturnLength
);

typedef struct _TOKEN_PRIVILEGES {
	DWORD               PrivilegeCount;
	LUID_AND_ATTRIBUTES Privileges[ANYSIZE_ARRAY];
} TOKEN_PRIVILEGES, *PTOKEN_PRIVILEGES;

typedef struct _LUID_AND_ATTRIBUTES {
	LUID  Luid;
	DWORD Attributes;
} LUID_AND_ATTRIBUTES, *PLUID_AND_ATTRIBUTES;
我们这里的attributes为SE_PRIVILEGE_ENABLED，表示允许特权
```

有这三个API就可以提权了，这里给出一个样例:

```
BOOL EnablePrivilege(PTCHAR Privilege)
{
	BOOL rc = FALSE;
    HANDLE hToken;
    LUID luid;
    TOKEN_PRIVILEGES tokenPrivilege;
	DWORD dwProcID = GetCurrentProcessId();
	HANDLE hProc = OpenProcess(PROCESS_ALL_ACCESS,FALSE,dwProcID);
	
    rc = OpenProcessToken(
            hProc,
            TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY,
            hToken);

    if (rc)
    {
        rc = LookupPrivilegeValue(NULL, Privilege, luid);
        if (rc)
        {
            tokenPrivilege.PrivilegeCount = 1;
            tokenPrivilege.Privileges[0].Luid = luid;
            tokenPrivilege.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;

            rc = AdjustTokenPrivileges(
                    hToken, 
                    FALSE, 
                    tokenPrivilege, 
                    sizeof(tokenPrivilege), 
                    NULL, 
                    NULL);
        }
    }
    if (hToken) CloseHandle(hToken);
	if(hProc)	CloseHandle(hProc);

    return rc;
}
```

理解访问控制模型，提权代码看着也舒服多了。

## 添加特权

在刚开始我们就提到，如果有些特权不在token列表中，那么需要一些额外手段，才能添加到列表。添加到列表才可以进行提权。添加特权还需要理解另外的概念。

### 本地安全策略

Local Security Authority，即LSA，是一个受保护的子系统，用于认证和登录用户以及调整本地权限信息。它通过一组LSA policy objects记录了本地安全策略信息。包括以下对象：

1.Policy对象保存全局策略信息；
2.TrustedDomain保存受信任域信息；
3.Account保存用户、组、本地账户信息；
4.Private Data保存密码等加密信息。

这里我们用到的时Policy对象，保存全局策略信息。LsaOpenPolicy建立与LSA的通信，得到Policy对象。

```
NTSTATUS LsaOpenPolicy（
	_In_ 	PLSA_UNICODE_STRING SystemName,
	_In_ 	PLSA_OBJECT_ATTRIBUTES ObjectAttributes,
	_In_ 	ACCESS_MASK DesiredAccess,
	_InOut_ PLSA_HANDLE PolicyHandle
);

typedef struct _LSA_UNICODE_STRING {
	USHORT Length;
	USHORT MaximumLength;
	PWSTR  Buffer;
} LSA_UNICODE_STRING, *PLSA_UNICODE_STRING;

ObjectAttributes：	为0即可。
DesiredAccess ： 	这里需要GENERIC_READ|GENERIC_WRITE|GENERIC_ALL GENERIC_EXECUTE
```

使用policy对象可以进一步使用操作本地安全策略的其他相关函数，注意这个函数只有Administrator用户才能调用。LsaClosePolicy用于关闭打开的Policy对象。MSDN有一个打开policy并关闭的简单程序：

```
#include <windows.h>
#define TARGET_SYSTEM_NAME L"mysystem"

LSA_HANDLE GetPolicyHandle()
{
  LSA_OBJECT_ATTRIBUTES ObjectAttributes;
  WCHAR SystemName[] = TARGET_SYSTEM_NAME;
  USHORT SystemNameLength;
  LSA_UNICODE_STRING lusSystemName;
  NTSTATUS ntsResult;
  LSA_HANDLE lsahPolicyHandle;

  // Object attributes are reserved, so initialize to zeros.
  ZeroMemory(ObjectAttributes, sizeof(ObjectAttributes));

  //Initialize an LSA_UNICODE_STRING to the server name.
  SystemNameLength = wcslen(SystemName);
  lusSystemName.Buffer = SystemName;
  lusSystemName.Length = SystemNameLength * sizeof(WCHAR);
  lusSystemName.MaximumLength = (SystemNameLength+1) * sizeof(WCHAR);

  // Get a handle to the Policy object.
  ntsResult = LsaOpenPolicy(
        lusSystemName,    //Name of the target system.
        ObjectAttributes, //Object attributes.
        POLICY_ALL_ACCESS, //Desired access permissions.
        lsahPolicyHandle  //Receives the policy handle.
    );

  if (ntsResult != STATUS_SUCCESS)
  {
    // An error occurred. Display it as a win32 error code.
    wprintf(L"OpenPolicy returned %lu\n",
      LsaNtStatusToWinError(ntsResult));
    return NULL;
  } 
  return lsahPolicyHandle;
}
```

### 查询用户SID并添加特权

查询用户SID需要用到LookupAccountName，查询MSDN后就会发现很简单的一个API。

```
BOOL WINAPI LookupAccountName(
  _In_opt_   LPCTSTR lpSystemName,
  _In_       LPCTSTR lpAccountName,
  _Out_opt_  PSID Sid,
  _Inout_    LPDWORD cbSid,
  _Out_opt_  LPTSTR ReferencedDomainName,
  _Inout_    LPDWORD cchReferencedDomainName,
  _Out_      PSID_NAME_USE peUse
);
typedef enum _SID_NAME_USE { 
  SidTypeUser            = 1,
  SidTypeGroup,
  SidTypeDomain,
  SidTypeAlias,
  SidTypeWellKnownGroup,
  SidTypeDeletedAccount,
  SidTypeInvalid,
  SidTypeUnknown,
  SidTypeComputer,
  SidTypeLabel
} SID_NAME_USE, *PSID_NAME_USE;
表明sid类型
```

添加特权需要用到LsaAddAccountRights：

```
NTSTATUS LsaAddAccountRights(
  _In_  LSA_HANDLE PolicyHandle,
  _In_  PSID AccountSid,
  _In_  PLSA_UNICODE_STRING UserRights,
  _In_  ULONG CountOfRights
);

typedef struct _LSA_UNICODE_STRING {
	USHORT Length;
	USHORT MaximumLength;
	PWSTR  Buffer;
} LSA_UNICODE_STRING, *PLSA_UNICODE_STRING;

```

## 最后

本文提到的内容大部分都来源于MSDN，但是MSDN的信息相对太分散，所以看了很久很久之后写了这篇文章，有什么问题希望大家能提出来。对于Policy的那一部分说实话我也不是非常了解，因此描述上可能有不妥，请见谅。
