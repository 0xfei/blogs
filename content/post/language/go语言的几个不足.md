---
title: "go的几点不足"
date: 2017-11-27T09:13:30+08:00
draft: true
categories: ["编程语言"]
tags: ["编程语言"]
topics: ["编程语言"]
---

使用go开发快一个月了，之前只是写写demo，真正在线上使用，才发现诸多不便（go真的兼具简洁和强大，还轮不到说它的坏话），吐槽一些别扭的地方。

1、if + 表达式。

只能表达式开头，如果想在特定条件下执行 “计算表达式 + 判断表达式结果”，似乎只能在外层加if。而C和C++可以随意加条件，任何有返回值的表达式都可以放到if的判断条件，加上逗号表达式，功能很强大。而go的这点不足，在维护代码时非常不方便。

<!--more-->

例子：

```
if v := check(stat); v != nil {
...
}

if ok {
if v := ...
}
```

而不能直接

```
if ok && (v := check(stat); v != nil) {
}
```

2、channel关闭判断

这个可以不算，后来发现`可以通过两个返回值判断是否关闭： v, closed := <-c。`

3、不支持真正的多态

4、循环引用不支持

5、没有inline、宏 默认参数

6、类型别名和类型命名，本来可以一模一样，非要脱了裤子放屁。

7、interface，看起来非常方便，但是非常混乱；动态监测，性能不足。你没办法一眼看出某个struct是不是某个interface，这种不方便是个大坑。所谓非侵入式，坑爹。

8、函数返回值变量，这个设计绝对是很好的，接口变得很清晰；但是菜鸟程序员往往选择滥用，最后根本看不懂函数要返回什么。
