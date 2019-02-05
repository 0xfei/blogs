---
title: "spdlog阅读记录"
date: 2018-04-13T09:13:30+08:00
draft: true
categories: ["后端架构"]
tags: ["后端架构", "源码分析"]
topics: ["后端架构", "源码分析"]
---

`spdlog_impl.h`: 用户接口

`registry.h`: logger封装接口

```
using registry =  registry_t<std::mutex> 
class registry_t {
private:
std::unordered_map<std::string, std::shared_ptr<logger>> _loggers; // name -> logger
}
```

<!--more-->

基于`simple_file_sink(file_sinks.h)`，调用create创建 `async_logger（async_logger.h）`

`sink_it`真正的写log，继承自`logger`。调用`async_log_helper`的`log`和`flush`函数。

`log_msg.h`: `log_msg`日志基础item

msg_log结构放到队列，后台线程循环取出，调用sink处理：缓存或者flush。放入循环数组，出队指针和入队指针维护，配合计数器，实现无锁队列。


#### 调用解释：

`// _loggers map clear`

```
spdlog::drop_all();
```

`// 设置registry._async_q_size`

```
spdlog::set_async_mode(8192*4); 
```

`// 创建Sink并注册到_loggers`

```
auto public_logger = spdlog::basic_logger_mt("public_logger", notice_file.c_str());
```

`// 设置logger._formatter 日志格式`

```
public_logger->set_pattern("NOTICE: [%Y-%m-%d %H:%M:%S] %v");
```

`// _loggers中获取logger`

```
auto logger = spdlog::get("public_logger");
```

`// 打印日志`

```
logger->info("{}:{} {}", cplusutils::servbase_basename(__FILE__), __LINE__, log_info.str());
```
