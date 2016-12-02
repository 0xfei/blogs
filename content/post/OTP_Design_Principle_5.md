+++
title = "9 OTP设计原则——第五部分"
date = "-----111-2016-11-30T19:55:17+08:00"
draft = true
description = "OTP Design Priciple"
tags = ["Erlang/OTP", "translation"]
topics = ["Erlang/OTP", "translation"]
+++


#### 9.11 发布镜像（release）

这一章节结合 SASL 的man page中的 *rel(4)*, *systools(3)* 和 *script(4)* 部分阅读。（之前的翻译我都把release当做“发行”或者“发布”，单独讨论还是叫“发布镜像”好一些，更像个专业名词。）

##### 9.11.1 发布镜像的概念

写完应用后，你希望创建一个完整的系统，包含自己的应用和Erlang/OTP应用的部分子集。这就是一个 **发布镜像** 。

需要创建一个 *发布镜像源文件* 指定哪些应用包含在这个发布镜像中。

源文件用于生成 *boot脚本* 和 *发行包* ，安装发行包的系统就是 **目标系统** 。如何利用发行包创建目标系统会在 *系统原理* 部分讲述。

<!--more-->

##### 9.11.2 镜像源文件（release source file）

.rel文件即定义发布镜像的源文件。这个文件用于定义发布镜像的名字和依赖的Erlang运行时版本和组成应用的版本；

```
{release, {Name,Vsn}, {erts, EVsn},
 [{Application1, AppVsn1},
  ...
  {ApplicationN, AppVsnN}
  ]
 }.
```

Name、Vsn、EVsn和AppVsn是字符串。

文件必须以Rel.rel命名，Rel必须是单独的名字。

每个 Application（原子）和 AppVsn 是镜像包含的应用名和版本，最小的Erlang/OTP镜像包括 Kernel 和 STDLIB 应用，它们一定会出现在列表中。

如果镜像可能升级，他必须要看 SASL（System Architecture Support Libraries） 应用。

**例子：** ch_app应用的.app文件如下：

```
{application, ch_app,
 [{description, "Channel allocator"},
  {vsn, "1"},
  {modules, [ch_app, ch_sup, ch3]},
  {registered, [ch3]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {ch_app,[]}}
 ]
}.
```

.rel文件必须包括kernel、stdlib和sasl。如下ch_rel-1.rel:

```
{release,
 {"ch_rel", "A"},
 {erts, "5.3"},
 [{kernel, "2.9"},
  {stdlib, "1.12"},
  {sasl, "1.10"},
  {ch_app, "1"}
 ]
}.
```

##### 9.11.3 生成boot脚本

SASL应用中的 *systools* 包含创建和检查发行镜像的函数。这些函数会读取 rel 和 .app 文件，进行语法和依赖性检查。*systools:make_script/1,2* 函数用于生成boot脚本。

```
1> systools:make_script("ch_rel-1", [local]).
ok
```

上面调用会创建一个boot脚本，包括可读的 ch_rel-1.script和运行时系统使用的二进制版本 ch_rel-1.boot。

* ch_rel-1 是.rel文件的文件名
* local 是一个选项，表示应用所在的目录会用于boot脚本，替换 $ROOT/lib。

这是最常用的本地测试生成boot脚本方法。

用boot脚本启动Erlang/OTP时，所有.rel文件中的应用会被自动加载和启动：

```
% erl -boot ch_rel-1
Erlang (BEAM) emulator version 5.3
Eshell V5.3 (abort with ^G)
1>
=PROGRESS REPORT==== 13-Jun-2003::12:01:15 ===
supervisor: {local,sasl_safe_sup}
started: [{pid,<0.33.0>},
		  {name,alarm_handler},
		  {mfa,{alarm_handler,start_link,[]}},
		  {restart_type,permanent},
		  {shutdown,2000},
		  {child_type,worker}]
...
=PROGRESS REPORT==== 13-Jun-2003::12:01:15 ===
		application: sasl
		started_at: nonode@nohost
...
=PROGRESS REPORT==== 13-Jun-2003::12:01:15 ===
		application: ch_app
		started_at: nonode@nohost
```

##### 9.11.4 创建一个发行版本

*systools:make_tar/1,2* 利用.rel文件创建包含特定应用压缩包，即 **发行包** 。

```
1> systools:make_script("ch_rel-1").
ok
2> systools:make_tar("ch_rel-1").
ok
```

发行包默认包括：

* .app文件
* .rel文件 
* 所有应用的目标文件，按应用目录结构组织
* 二进制boot脚本 start.boot

```
% tar tf ch_rel-1.tar
lib/kernel-2.9/ebin/kernel.app
lib/kernel-2.9/ebin/application.beam
...
lib/stdlib-1.12/ebin/stdlib.app
lib/stdlib-1.12/ebin/beam_lib.beam
...
lib/sasl-1.10/ebin/sasl.app
lib/sasl-1.10/ebin/sasl.beam
...
lib/ch_app-1/ebin/ch_app.app
lib/ch_app-1/ebin/ch_app.beam
lib/ch_app-1/ebin/ch_sup.beam
lib/ch_app-1/ebin/ch3.beam
releases/A/start.boot
releases/A/ch_rel-1.rel
releases/ch_rel-1.rel
```

发行包创建钱，新的boot脚本生成。在发行包中，所有应用目录放在lib下。因为不知道发行包的安装位置，所以不要硬编码路径。

发行镜像源文件 mysystem.rel 会复制到tar文件中。最开始，文件只存在发行目录下，用于release_handler解压文件。解压之后，release_handler会自动复制到releases/FIRST。第一次安装的时候文件也会复制到tar文件中，不需要手动复制。

如果存在一个 relup 文件和系统配置文件 sys.config，它们同样会包含在发行包中。

可以通过选项让发行包包括源文件和ERTS二进制文件。

##### 9.11.5 目录结构

release handler从发行包安装得代码的目录结构如下：

```
$ROOT/lib/App1-AVsn1/ebin
					/priv
		/App2-AVsn2/ebin
					/priv
		...
		/AppN-AVsnN/ebin
					/priv
		/erts-EVsn/bin
		/releases/Vsn
		/bin
```

* lib - 应用目录
* erts-EVsn/bin - Erlang运行时系统可执行文件
* releases/Vsn - .rel文件和boot脚本start.boot
* bin - 顶层Erlang运行时系统可执行文件

#### 9.12 发布镜像处理

##### 9.12.1 发布镜像处理基本概念

Erlang编程语言的一个重要特性是运行时改变模块代码的能力，在Erlang引用手册中提到的 *代码替换* 。

基于这个特性，OTP应用 SASL 提供了一个框架用于运行时在不同的版本间升级和回退。这就是 **发布镜像处理** 。

这个框架包括：

* 离线支持 - systools用于生成脚本和创建发行包
* 在线支持 - release_handler 用于解包和安装发行包

基于Erlang/OTP的开启发布处理最小系统，包括Kernel、STDLIB和SASL应用。

###### 镜像处理流程

**Step 1)** 创建一个发布镜像
**Step 2)** 安装到目标系统
**Step 3)** 调整代码用于生产环境
**Step 4)** 某个时间点，做了版本升级
**Step 5)** 对每个调整的应用，创建一个应用升级文件 .appup。用于描述如何升级版本和回退
**Step 6)** 基于.appup文件，创建镜像升级文件 relup
**Step 7)** 新的发布镜像创建成功并放到目标系统
**Step 8)** 用release handler解压新的镜像
**Step 9)** 新版本镜像安装完成。通过执行 relup 中的指令。模块可以被安装、删除、重新安装、重启等。某些情况，可能需要重启整个虚拟机。

* 如果安装失败，系统可能重启。接着使用老版本。
* 安装成功，新版本成为默认版本，系统重启之后会使用新版本。

###### 发布处理的其他方面

*Appup Cookbook* 包括典型的升级/回退 .appup 文件例子。然而，好多地方会使得发布处理变得混乱：

* 混乱或者环形依赖：
	* 节点之间
	* 进程之间
	* 模块之间
* 升级期间，不相关的进程继续正常的执行流程。可能导致超时或者其他错误。

因此推荐代码尽可能少的改动，尽量保持向下兼容。
