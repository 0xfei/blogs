
---
title: "windows结构化异常处理"
date: 2014-10-21T09:13:30+08:00
draft: true
categories: ["Windows安全"]
tags: ["Windows安全"]
topics: ["Windows安全"]
---

一篇记录。在我把《windows核心编程》放回家之前打算再读一遍，倒着来，果然收获不小，能更清晰的了解到这本书讲得多么好。昨天晚上看了最后面的结构化异常处理，这里写个小心得。

<!--more-->

结构化异常处理和C++的异常处理没有关系，比如Java等其他语言也有自己的异常处理。SEH是windows操作系统支持的特性，主要依靠编译器和操作系统，与语言无关。简单来说，windows异常处理涉及这几个方面：__try/__finally来保证不得不做的清理，__try/__except保护代码段，__except的异常过滤函数根据异常发生时压入栈中的EXCEPTION_RECORD信息处理并返回EXCEPTION_EXECUTE_HANDLER或EXCEPTION_CONTINUE_EXECUTION或EXCEPTION_CONTINUE_SEARCH，以及多层调用的展开和未处理异常的处理。

__try/__finally的规则最简单，__finally{ ... }的代码一定会执行，除非__try{ ... }块执行时进程或线程终止。也就是说，在__try{ ... }块中执行break、continue、goto、return等等，都会先到__fianlly中执行完它的代码块。如果finally{ ... }中也有return，那么相应的返回值就会被替换，这个也叫局部展开。所以好的编码规范是，不要在__finally中执行过多代码，尽量只做可预计的清理工作；而__try块中也尽量用"__leave;"指令伪顺序的走到__finally代码中。可以在__finally{...}中使用AbnormalTermination()内联函数判断是否正常退出。

__try/__except是异常处理的重点，__try只能和一个__except或者__finally成对出现，这个一些语言本身的异常处理机制有很大不同。它的形式如下：


```

__try
{
	/*
		DoSomething
	*/
}
__except(CheckExcept() /* return EXCEPTION_EXECUTE_HANDLER
			EXCEPTION_CONTINUE_SEARCH
		     or EXCEPTION_CONTINUE_EXECUTION
		       */)
{
	/*
		DoSomething if CheckExcept return EXCEPTION_EXECUTE_HANDLER
	*/
}

```

CheckExcept()称为异常过滤函数，检查异常发生时的信息，返回三个值中的一个。异常发生时会有一个异常错误代码，GetExceptionCode函数返回该值，这个函数只能在异常过滤函数和异常处理部分访问；操作系统会在异常栈中压入EXCEPTION_RECORD、CONTEXT和EXCEPTION_POINTERS结构，GetExceptionInformation函数可以得到这些信息，不过只能在异常过滤程序中访问。如果返回EXCEPTION_EXECUTE_HANDLER，则执行__except代码块中的代码。如果A函数调用B，B函数中有__try/__finally，但是__try块发生了异常，__finally是不能处理异常的，所以按栈返回上一层，假设__except可以处理这个异常，即__except的异常过滤函数返回了EXCEPTION_EXECUTE_HANDLER，那么这时候会返回B函数中执行__finally的代码。这个过程称为全局展开。注意只有异常过滤函数返回EXCEPTION_EXECUTE_HANDLER时才执行全局展开，其他情况__finally中的代码不会得到执行。

如果异常过滤函数返回EXCEPTION_CONTINUE_EXECUTION，则执行完异常过滤程序之后程序回到异常发生的地方重新执行，异常过滤程序可以修复异常，当然这样做也可能发生危险。一个好的例子是按需换页和按需增长栈空间；不好的例子就很多了，例如死循环。

另一个返回值EXCEPTION_CONTINUE_SEARCH不会执行__except中的代码，而会返回上层，寻找进一步捕获。

我们可以用RaiseException函数返回自己定义的异常，这样可以便利一些代码的编写。

最后还有一些其他的相关内容，比如未处理异常，也就是所有__except的异常过滤函数都返回EXCEPTION_CONTINUE_SEARCH或者没有异常处理块，那么就由UnhandledExceptiongFilter接管，我们也可以注册自己的未处理异常函数，用SetUnhandledExceptionFilter。我们还可以添加向量化异常处理函数，AddVectoredExceptionHandler添加的异常处理函数在SEH处理之前执行，AddVectoredContinueHandler添加在未处理异常发生时执行的向量化异常处理函数。另外还有windows错误报告WER的一些知识，都比较好理解。

向量化异常处理并不判重，所以可以注册多次。这有什么用呢？我稍稍想到一点点，可以做hook。要是有心情写出利用代码了再写出来。
