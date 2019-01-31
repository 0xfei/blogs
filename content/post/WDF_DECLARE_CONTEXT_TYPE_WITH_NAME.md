
---
title: "WDF_DECLARE_CONTEXT_TYPE_WITH_NAME"
date: 2014-12-24T09:13:30+08:00
draft: true
categories: ["Windows安全"]
tags: ["Windows安全"]
topics: ["Windows安全"]
---

WDF提供的这个宏用于实现面向对象编程中常见的存取器函数（我都忘了这个的确切名字是什么了，不同语言有不同的称谓），就是调用某个函数来获取某个结构的某个成员。完整定义如下：

<!--more-->

```

#define WDF_DECLARE_CONTEXT_TYPE_WITH_NAME(_contexttype, _castingfunction) \
                                                                        \
WDF_DECLARE_TYPE_AND_GLOBALS(                                           \
    _contexttype,                                                       \
    WDF_GET_CONTEXT_TYPE_INFO(_contexttype),                            \
    NULL,                                                               \
    WDF_TYPE_DEFAULT_SECTION_NAME)                                      \
                                                                        \
WDF_DECLARE_CASTING_FUNCTION(_contexttype, _castingfunction)

```

WDF_DECLARE_TYPE_AND_GLOBALS宏实现结构定义，WDF_DECLARE_CASTING_FUNCTION实现绑定。


```

#define WDF_DECLARE_TYPE_AND_GLOBALS(_contexttype, _UniqueType, _GetUniqueType, _section)\
                                                                        \
typedef _contexttype* WDF_TYPE_NAME_POINTER_TYPE(_contexttype);         \
                                                                        \
WDF_EXTERN_C __declspec(allocate( _section )) __declspec(selectany) extern const WDF_OBJECT_CONTEXT_TYPE_INFO WDF_TYPE_NAME_TO_TYPE_INFO(_contexttype) =  \
{                                                                       \
    sizeof(WDF_OBJECT_CONTEXT_TYPE_INFO),                               \
    #_contexttype,                                                      \
    sizeof(_contexttype),                                               \
    _UniqueType,                                                        \
    _GetUniqueType,                                                     \
};                                                                      \

```

ddddddddddddddddddddddd


```

#define WDF_GET_CONTEXT_TYPE_INFO(_contexttype) \
    (&WDF_TYPE_NAME_TO_TYPE_INFO(_contexttype))

```


```

#define WDF_TYPE_NAME_TO_TYPE_INFO(_contexttype) \
      _WDF_ ## _contexttype ## _TYPE_INFO

```

做了一个结构名的变化，定义了_WDF_type_TYPE_INFO。

WDF_TYPE_DEFAULT_SECTION_NAME是.data，说明数据是放在.data节的。


```

#define WDF_DECLARE_CASTING_FUNCTION(_contexttype, _castingfunction)    \
                                                                        \
__drv_aliasesMem                                                        \
WDF_EXTERN_C                                                            \
WDF_TYPE_NAME_POINTER_TYPE(_contexttype)                                \
FORCEINLINE                                                             \
_castingfunction(                                                       \
   __in WDFOBJECT Handle                                                     \
   )                                                                    \
{                                                                       \
    return (WDF_TYPE_NAME_POINTER_TYPE(_contexttype))                   \
        WdfObjectGetTypedContextWorker(                                 \
            Handle,                                                     \
            WDF_GET_CONTEXT_TYPE_INFO(_contexttype)-> UniqueType         \
            );                                                          \
}


```

这里头有一个常用的宏，类似_IN_和_OUT_，__drv_aliasesMem。MSDN的某个角落里这样写到：

<pre>Functions that take a pointer and alias it (thus avoiding a leak) should be annotated with __drv_aliasesMem. If you create a function that inserts an object into a global structure, or passes it to a system function that does that, you should add the __drv_aliasesMem annotation.</pre>
整个宏算是比较清晰了。
