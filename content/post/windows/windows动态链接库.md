
---
title: "windows动态链接库"
date: 2014-10-27T09:13:30+08:00
draft: true
categories: ["Windows安全"]
tags: ["Windows安全"]
topics: ["Windows安全"]
---

dll加载时会映射到进程地址空间，成为进程的一部分，就像一个箱子被扔到屋里，成为了屋子的一部分。编写dll有一些基本的原则，比如一定要有成对的管理函数：申请和释放、获取和解除等，好的dll应该有好的封闭性和完整性。进程加载导入表中的dll有一个固定的顺序：

<!--more-->

1、可执行文件所在的目录；2、系统目录；3、windows目录；4、进程当前目录；5、PATH环境变量所指向的目录。

HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\KnowDLLs注册表项中列出了系统已知DLL，当LoadLibrary的dll名以.dll结尾时，会去除.dll，然后在该注册表项中查找对应项，并加载该KEY的value。

说道LoadLibrary，LoadLibraryEx用的并不多，这个扩展函数加了两个参数：hFile和dwFlags，dwFlags有一些标志值得注意，DONT_RESOLVE_DLL_REFERENCES标志告诉系统不要调用DllMain；LOAD_LIBRARY_AS_DATEFILE表示将该dll作为数据文件映射，可以用来加载资源文件或者exe文件；LOAD_LIBRARY_AS_DATE_FILE_EXCLUSIVE是前一个标志的独占版；LOAD_LIBRARY_AS_IMAGE_RESOURCE和**_AS_DATEFILE基本一样，不过这个标志会自动修复相对虚拟地址；LOAD_WITH_ALTERED_SEARCH_PATH会改变dll的查找方式。

FreeLibraryAndExitThread是一个不错的函数，不过用的并不多。

dll加载另一个需要注意的地方是序列化，如果进程的两个线程都载入某个dll，那么dllmain会收到两次DLL_THREAD_ATTACH，但是进程的一个全局锁（或者关键段）会用于同步，这样做到序列化。所以如果在DLL_THREAD_ATTACH或DLL_PROCESS_ATTACH中创建线程并等待该线程句柄触发，将会导致死锁。

延迟载入DLL是指在进程调用某个API时再载入对应的dll，进程初始化就省去了不少麻烦。/Lib:DelayImp.lib /DelayLoad:MyDelayLoadDll.dll，加入这两个连接器开关可以启用延迟加载。需要设置项目属性。DelayImp.lib导出了__delayLoadHelper2函数，将其嵌入可执行文件，将延迟加载的dll放入延迟载入段。可执行文件执行到某个延迟载入dll中的API时，实际上会调用__delayLoadHelper2函数，期间可能会抛出异常，VcppException(ERROR_SEVERITY_ERROR, ERROR_MOD_NOT_FOUND)和VcppException(ERROR_SEVERITY_ERROR, ERROR_PROC_NOT_FOUND)，异常过滤函数DelayLoadDllExceptionFilter检查这两个异常并返回EXCEPTION_EXECUTE_HANDLER，同时提供一个DelayLoadInfo结构，该结构包含了所需信息，就在EXCEPTION_POINTERS结构的ExceptionRecord-> ExceptionInformation。

```
typedef struct DelayLoadInfo {
 DWORD cb; // size of structure
 PCImgDelayDescr pidd; // raw form of data (everything is there)
 FARPROC * ppfn; // points to address of function to load
 LPCSTR szDll; // name of dll
 DelayLoadProc dlp; // name or ordinal of procedure
 HMODULE hmodCur; // the hInstance of the library we have loaded
 FARPROC pfnCur; // the actual function that will be called
 DWORD dwLastError;// error received (if an error notification)
 } DelayLoadInfo, * PDelayLoadInfo;

typedef FARPROC (WINAPI *PfnDliHook)(
 unsigned dliNotify,
 PDelayLoadInfo pdli
 );
```

有两个全局变量：

```
ExternC
PfnDliHook __pfnDliNotifyHook2;

ExternC
PfnDliHook __pfnDliFailureHook2;
```

我们可以给这两个值赋值，很明显这是两个HOOK值，供我们控制更多。

/Delay:unload可以允许我们卸载延迟加载的dll，调用BOOL WINAPI __FUnloadDelayLoadedDLL2(LPCSTR szDll)。

#prgma comment(linker, "/export: A = dll.b")，申明一个函数转发器。

另外，rebase和bind程序可以很大的优化程序。


