+++
title = "9 OTP设计原则——第四部分"
date = "2016-11-29T20:30:48+08:00"
draft = true
description = "OTP Design Priciple"
categories = ["Erlang/OTP"]
tags = ["Erlang/OTP", "翻译文章"]
topics = ["Erlang/OTP"]
+++


#### 9.8 应用

这一章节结合 Kernel 的man page中的 *app(4)* 和 *application(3)* 部分阅读。

##### 9.8.1 应用概念

当你写完代码实现某个特定功能之后，你希望把它变成一个 **应用** —— 可以被启动停止的组件单元，同时也能被其他系统复用。

为了做到这一点，创建一个 *应用回调模块* ，还需要描述这个应用如何启动和停止的文件。

因此，需要一个 **应用描述说明** ，并放到 *应用源文件* 中。除了这些，这个文件还表明应用由哪些模块组成以及回调模块的名称。

如果你用 *systools* —— Erlang/OTP的代码打包工具（查看 *Releases* ），为每个应用准备的代码会放到特殊的目录，遵从预定义的 *目录结构*。

<!--more-->

##### 9.8.2 应用回调模块

如何启动和终止应用，即监控树，用下面的两个回调函数实现：

```
start(StartType, StartArgs) -> {ok, Pid} | {ok, Pid, State}
stop(State)
```

* start 在启动应用时调用，通过启动顶层监控器创建监控树。应该返回顶层监控器的pid和一个选项，State，默认是[]。它会传给stop程序。
* StartType 通常来说是原子 normal。其他值用于接管或者失败，具体阅读 *分布式应用* 。
* StartArgs 在 *应用源文件* 中的 mod 键中给出。
* stop/1 在应用停止 **之后** 被调用，做必要的清理工作。实际的应用停止，实际上是监控树的停止，会自动处理，具体描述在 *启动和停止应用* 部分。

打包一个用监视器行为模式实现的监控树的应用回调模块代码如下：

```
-module(ch_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
	ch_sup:start_link().
stop(_State) ->
	ok.
```

库应用不能被启动或停止，因此不需要应用回调模块。

##### 9.8.3 应用源文件

为了定义完整应用，要把 **应用描述信息** 放到 **应用源文件** 中，或者说放到一个.app文件：

```
{application, Application, [Opt1, ... , OptN]}.
```

* Application，是应用的名字原子。这个源文件名必须是Application.app。
* 每个 Opt 是一个元组 {Key, Value}，定义了应用的属性。所有键都是可选的，没有明确说明的情况会使用默认值。

库文件libapp的.app文件看起来像这样：

```
{application, libapp, []}.
```

ch_app监控树的最小.app文件ch_app.app看起来是这样：

```
{application, ch_app,[{mod, {ch_app,[]}}]}.
```

关键字mod指定了回调模块和参数，即sc_app和[]。表示启动进程会调用下面的代码：

```
ch_app:start(normal, []).
```

停止时下面的代码调用：

```
ch_app:stop([]).
```

当使用Erlang/OTP打包工具systools时，description、vsn、modules、registered和applications简直也要指定：

```
{application, ch_app,
 [{description, "Channel allocator"},
  {vsn, "1"},
  {modules, [ch_app, ch_sup, ch3]},
  {registered, [ch3]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {ch_app,[]}}
 ]}.
```

* description - 是简短的字符串描述文字，默认是空
* vsn - 是字符串形式的版本号，默认是空
* modules - 应用引入的所有模块。systools生成boot脚本和tar文件时要用到这个列表。一个模块只能在一个应用中定义，默认是空。
* registered - 应用中的所有进程注册名。systools用这个列表检测应用间命名冲突。
* applications - 这个应用的依赖应用，即必须已经启动的应用。systools用这个列表生成boot脚本。默认是空列表。注意所有应用都至少依赖Kernel和STDLIB应用。

*注意：查看Kernel的man page中的 *app* 部分了解更详细的配置语法和内容。 *

##### 9.8.4 目录结构

用systools打包代码时，每个应用的代码放到不同的目录，lib/Application-Vsn，Vsn是版本号。

即使不用systools，也很有必要了解目录结构，因为Erlang/OTP的代码打包就是按照OTP规范来设计的目录结构。如果一个应用有多个版本，代码服务器(Kernel man page *code(3)* )自动选取版本号高的目录中的代码。

目录结构也可以用于开发环境，省略版本号。

应用目录有以下子目录：

* src - 包含Erlang源代码
* ebin - 包含Erlang生成文件，即beam文件。.app文件也在这里
* priv - 应用所需的特殊代码。C可执行文件放在这里。*code：priv_dir/1* 函数用于访问这个目录
* include - 用于文件包含

##### 9.8.5 应用控制器

Erlang运行时系统启动时，一些进程作为Erlang应用的一部分也会启动起来。其中一个就是 **应用控制器** 进程，注册为 *application_controller* 。

应用控制器协调所有对进程的操作。可以用 *application* 模块中的函数来交互，查看Kernel的manpage 中 *application(3)* 。例如应用的加载、卸载、启动、停止。

##### 9.8.6 应用的加载和卸载

应用启动之前需要先加载。应用控制器从.app文件中读取并存储信息：

```
1> application:load(ch_app).
ok
2> application:loaded_applications().
[{kernel,"ERTS CXC 138 10","2.8.1.3"},
 {stdlib,"ERTS CXC 138 10","1.11.4.3"},
 {ch_app,"Channel allocator","1"}]
```

如果应用停止之后不再启动，那么可以把它卸载。有关信息会从应用控制器内部数据中删除：

```
3> application:unload(ch_app).
ok
4> application:loaded_applications().
[{kernel,"ERTS CXC 138 10","2.8.1.3"},
 {stdlib,"ERTS CXC 138 10","1.11.4.3"}]
```

*注意：加载/卸载应用并不会加载/卸载应用使用的代码。代码加载按照一般的方式完成。*

##### 9.8.7 启动和停止应用

应用通过以下调用启动：

```
5> application:start(ch_app).
ok
6> application:which_applications().
[{kernel,"ERTS CXC 138 10","2.8.1.3"},
 {stdlib,"ERTS CXC 138 10","1.11.4.3"},
 {ch_app,"Channel allocator","1"}]
```

如果应用没有加载，应用控制器会调用 *application:load/1* 加载。它检查 applications 键值，确保依赖的应用已经启动。

应用控制器随后为这个应用创建一个 *应用管理（application master）* 。application master是应用的所有程序的组leader。它通过调用应用回调模块的 *start/2* 启动应用，并传入 .app 文件的 mod 键值。

如果要停止应用，但是不卸载，调用如下：

```
7> application:stop(ch_app).
ok
```

application master通过通知顶层监视器停止应用。顶层监视器通知所有子进程关闭。整个监控树按照启动的相反顺序终止。application master调用应用回调模块的 *stop/1*。

##### 9.8.8 配置应用

应用通过配置参数配置。配置参数是{Par, Val}的元组列表，通过.app文件中的env简直说明：

```
{application, ch_app,
 [{description, "Channel allocator"},
 {vsn, "1"},
 {modules, [ch_app, ch_sup, ch3]},
 {registered, [ch3]},
 {applications, [kernel, stdlib, sasl]},
 {mod, {ch_app,[]}},
 {env, [{file, "/usr/local/log"}]}
 ]}.
```

Par是一个原子。Val可以是任何Erlang项。应用可以调用 *application:get_env(App, Par)* 等一系列函数获取配置参数值，具体查看Kernel的 *application(3)* 内容。

**例子** ：

```
% erl
Erlang (BEAM) emulator version 5.2.3.6 [hipe] [threads:0]
Eshell V5.2.3.6 (abort with ^G)
1> application:start(ch_app).
ok
2> application:get_env(ch_app, file).
{ok,"/usr/local/log"}
```

.app文件中的值可以被 **系统配置文件** 覆盖。这个文件包含相关应用的配置参数：

```
[{Application1, [{Par11,Val11},...]},
 ...,
 {ApplicationN, [{ParN1,ValN1},...]}].
```

系统配置项通过调用Name.config，Name是Erlang启动时指定的参数 -config Name。具体细节查看 Kernel 页面的 *config(4)* 内容。

**例子** ：

假设test.config有下面的内容：

```
[{ch_app, [{file, "testlog"}]}].
```

*file* 的值会覆盖 .app 文件中的键值：

```
% erl -config test
Erlang (BEAM) emulator version 5.2.3.6 [hipe] [threads:0]
Eshell V5.2.3.6 (abort with ^G)
1> application:start(ch_app).
ok
2> application:get_env(ch_app, file).
{ok,"testlog"}
```

如果用了 *发行控制* ，只有一个系统配置文件，这个文件调用 sys.config 使用。

.app文件中的值和系统配置中的值可以通过命令行覆盖：

```
% erl -ApplName Par1 Val1 ... ParN ValN
```

**例子** ：

```
% erl -ch_app file '"testlog"'
Erlang (BEAM) emulator version 5.2.3.6 [hipe] [threads:0]
Eshell V5.2.3.6 (abort with ^G)
1> application:start(ch_app).
ok
2> application:get_env(ch_app, file).
{ok,"testlog"}
```

##### 9.8.9 应用启动类型

**启动类型** 在启动应用时定义：

```
application:start(Application, Type)
```

*application:start(Application)* 和调用 *application:start(Application, temporary)* 一样。Type也可以是 *permanent* 或 *transient* ：

* 一个permanent应用终止时，其他应用和运行时系统都会终止
* 如果transient应用以normal方式终止，会被reported，其它应用不会终止。如果以其它原因退出，同样运行时系统和其他应用会退出。
* 如果一个temporary应用终止，其他应用和运行时系统不受影响。

任何应用都可以通过调用 *application:stop/1* 停止，其他应用不受影响。

transient模式实际中很少使用，因为监控树终止时，原因是shutdown而不是normal。


#### 9.9 应用引用

##### 9.9.1 介绍

应用可以引用其他应用。被引用的应用有自己的应用目录和.app文件，但是它作为另一个应用监控树的一部分启动。

应用只能被一个应用引用。

被引用的应用可以引用其他应用。

没有被其他应用引用的应用被称为 **主应用** 。

应用控制器加载主应用时自动加载被引用的应用，但是不会启动它们。相反，被引用应用的顶层监控器会被包含应用中的一个监视器启动。

这意味着运行时，一个被包含的应用是主应用的一部分，被包含应用中的进程会认为自己属于主应用。

##### 9.9.2 引用应用的定义

应用通过.app文件中的included_applications键定义：

```
{application, prim_app,
 [{description, "Tree application"},
  {vsn, "1"},
  {modules, [prim_app_cb, prim_app_sup, prim_app_server]},
  {registered, [prim_app_server]},
  {included_applications, [incl_app]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {prim_app_cb,[]}},
  {env, [{file, "/usr/local/log"}]}
 ]}.
```

##### 9.9.3 启动时的进程同步

被引用的应用监控树作为应用应用监控树的一部分启动。如果需要同步引用和被引用的应用，可以通过定义 *启动阶段* 实现。

启动阶段划分通过.app文件中的 *start_phases* 键值定义，是一组{Phase, PhaseArgs}元组列表定义，Phase是原子，PhaseArgs是Erlang项。

包含应用的应用mod键的值必须被设置为 {application_starter, [Module, StartArgs]}，Module是应用回调模块名，StartArgs作为Module:start/2的参数:

```
{application, prim_app,
 [{description, "Tree application"},
  {vsn, "1"},
  {modules, [prim_app_cb, prim_app_sup, prim_app_server]},
  {registered, [prim_app_server]},
  {included_applications, [incl_app]},
  {start_phases, [{init,[]}, {go,[]}]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {application_starter,[prim_app_cb,[]]}},
  {env, [{file, "/usr/local/log"}]}
 ]}.

{application, incl_app,
 [{description, "Included application"},
  {vsn, "1"},
  {modules, [incl_app_cb, incl_app_sup, incl_app_server]},
  {registered, []},
  {start_phases, [{go,[]}]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {incl_app_cb,[]}}
 ]}.
```

启动有引用应用的主应用时，主应用正常启动：

* 应用控制器为这个应用建一个application master
* 应用控制器调用 Module:start(normal, StartArgs)启动顶层监视器

然后，对于主应用和每个被引用的应用，从上到下、从左到右调用Module:start_phase(Phase, Type, PhaseArgs)。如果应用没有定义阶段，它不会被调用。

被引用的应用.app文件有以下要求：

* {mod, {Module, StartArgs}} 选项。指定应用回调模块Module，StartArgs忽略，因为Module:start仅会调用主应用。
* 如果引用的应用也应用其他应用，{mod, {application_starter, [Module, StartArgs]}}也要有。
* {start_phases, [{Phase, PhaseArgs}]}，指定阶段的集合必须作为主应用阶段定义的子集。

当启动上面定义的prim_app，应用控制器在application:start(prim_app)返回之前调用下面的回调函数:

```
application:start(prim_app)
=> prim_app_cb:start(normal, [])
=> prim_app_cb:start_phase(init, normal, [])
=> prim_app_cb:start_phase(go, normal, [])
=> incl_app_cb:start_phase(go, normal, [])
ok
```

#### 分布式应用

##### 9.10.1 介绍

在多个Erlang节点的分布式系统，以分布式的方式控制应用很有必要。如果应用运行的某个节点down机，这个应用会被另一个节点重启。

这样的应用被称为 **分布式应用** 。注意应用的控制是分布式的。这种场景中的所有应用都可以分布式处理，丽日，使用其他节点上的服务。

因为分布式应用可以在节点间转移，需要必要的寻址机制确保其他节点可以被应用寻址。Kernel中的global和pg2模块用于这个目的。

##### 9.10.2 分布式应用定义

分布式应用由应用控制器和分布式应用管理进程——dist_ac。两个进程都是Kernel应用的一部分。分布式应用由Kernel应用的配置项定义，使用下面的配置参数：

distributed = [{Application, [TimeOut,], NodeDesc}]

* 定义Application在哪个节点运行
* >NodeDesc = [Node | {Node, ..., Node}] 是优先级排列的节点名，元组中的节点顺序没定义。
* Timeout = integer() 表示在其他节点重启应用要等待的毫秒，默认是0。

要让分布式的应用控制工作正常，节点必须协商在哪里启动应用。使用下面的Kernel配置项定义：

* sync_nodes_mandatory = [Node] - 定义必须启动的节点
* sync_nodes_optional = [Node] - 定义可以启动的节点
* sync_nodes_timeout = integer() | infinity - 定义等待其他节点启动的时间

启动时，等待通过sync_nodes_mandatory和sync_nodes_optional配置项指定的节点启动。最多等待时间由sync_nodes_timeout指定。

**例子** ：

myapp应用运行在节点 cp1@cave。如果节点挂掉，会被cp2@cave或cp3@cave重启。cp1.conf配置如下：

```
[{kernel,
 [{distributed, [{myapp, 5000, [cp1@cave, {cp2@cave, cp3@cave}]}]},
  {sync_nodes_mandatory, [cp2@cave, cp3@cave]},
  {sync_nodes_timeout, 5000}
 ]
}].
```

cp2@cave和cp3@cave配置项类似，除了sync_nodes_mandatory。

*注意：相关节点的distributed值和sync_nodes_timeout值必须相同，否则可能发生预期之外的行为。*

##### 9.10.3 分布式应用的启动和停止

所有相关节点启动之后，分布式系统在所有节点调用application:start(Application)开始运行。

一个boot脚本可以用于应用的自动启动。

应用在distributed配置项指定的第一个可以使用的节点启动。应用master会被创建，并调用下面函数：

```
Module:start(normal, StartArgs)
```

**例子**：

继续之前提到的例子：

```
> erl -sname cp1 -config cp1
> erl -sname cp2 -config cp2
> erl -sname cp3 -config cp3
```

应用通过application:stop(Application)停止。

##### 9.10.4 容错

节点挂掉时，其他节点会试图重启它。通过distributed键值指定的第一个节点。通过调用：

```
Module:start(normal, StartArgs)
```

一个例外是如果应用有start_phases键，应用通过下面的方式调用：

```
Module:start({failover, Node}, StartArgs)
```

Node是终止的节点名。

**例子：**


##### 9.10.5 接管

优先级更高的节点启动时接管应用启动所在的节点。

```
Module:start({takeover, Node}, StartArgs)
```

