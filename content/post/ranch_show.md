+++
date = "2016-12-21T00:28:44+08:00"
title = "ranch运行时的进程信息"
draft = true
description = "tcp acceptor pool ranch"
tags = ["Erlang/OTP", "code", "ranch"]
topics = ["Erlang/OTP", "code", "ranch"]
+++

![Alt text](http://0x01f.com/images/ranch1.png)

<!--more-->

上图是用observer监控cowboy的例子file_server时的截图，很好的展示了cowboy结合ranch运行时的进程情况。

```<0.596.0>``` 和 ```<0.597.0>``` 分别是 ```ranch_conns_sup``` 和 ```ranch_acceptor_sup``` 。

![Alt text](http://0x01f.com/images/ranch2.png)

![Alt text](http://0x01f.com/images/ranch3.png)

最后的一列worker进程是阻塞在accept调用的ranch_acceptor进程。

![Alt text](http://0x01f.com/images/ranch4.png)

当cowboy建立http服务器时，这组进程结构被创建起来。而当连接到来时，某个阻塞的 ```ranch_acceptor``` 进程被激活，并调用 ```ranch_conns_sup:start_protocol``` 。在 ```loop``` 中等待 ```{start_protocol,...}``` 消息的进程 ```<0.596.0>``` 创建新的用户进程，即 ```cowboy_clear``` 。

![Alt text](http://0x01f.com/images/ranch5.png)

这是浏览器访问 ```http://localhost:8080``` 时的进程情况，可以看到新创建了 ```<0.707.0>``` 进程，而它阻塞在 ```cowboy_http``` 的 ```loop``` 循环。

![Alt text](http://0x01f.com/images/ranch6.png)