+++
title = "Erlang和其他语言的交互"
date = "2017-01-09T19:24:10+08:00"
draft = true
description = "Interoperability Erlang"
categories = ["Erlang/OTP"]
tags = ["Erlang/OTP"]
topics = ["Erlang/OTP"]
+++

Erlang和其他语言（如C和Java）的交互手段一直是我很感兴趣的主题，周末看了下OTP文档，终于大致理清楚了思路。这里先简单总结四种交互手段，也是更进一步学习Erlang的开始。

#### 端口

最简单的方式是调用Erlang模块的 `open_port/2` ，创建一个端口。Erlang端口可以被认为是一个外部Erlang进程，交互手段是通过标准IO输入输出，对应C语言里的read和write函数。

```
-spec open_port(PortName, PortSettings) -> port() when
      PortName :: {spawn, Command :: string() | binary()} |
                  {spawn_driver, Command :: string() | binary()} |
                  {spawn_executable, FileName :: file:name() } |
                  {fd, In :: non_neg_integer(), Out :: non_neg_integer()},
      PortSettings :: [Opt],
      Opt :: {packet, N :: 1 | 2 | 4}
           | stream
           | {line, L :: non_neg_integer()}
           | {cd, Dir :: string() | binary()}
           | {env, Env :: [{Name :: string(), Val :: string() | false}]}
           | {args, [string() | binary()]}
           | {arg0, string() | binary()}
           | exit_status
           | use_stdio
           | nouse_stdio
           | stderr_to_stdout
           | in
           | out
           | binary
           | eof
	   | {parallelism, Boolean :: boolean()}
	   | hide.
```

<!--more-->

还没有读源码，但是原理很好理解，只需要重定向标准输入输出，如linux的dup2函数，就可以实现数据交互。具体的数据格式也是简单的二进制串，由{packet, N}指定开头长度标识符的位数。

例子如下：

```
%% erlang
%% simple test for string operation with c
-module(complex1).
-export([start/1, stop/0, init/1]).
-export([strlen/1, strcmp/2]).

start(Prog) ->
    spawn(?MODULE, init, [Prog]).

stop() ->
    ?MODULE ! stop.

strlen(S) ->
    call_port({strlen, S}).
strcmp(S, T) ->
    call_port({strcmp, S, T}).

call_port(Msg) ->
    ?MODULE ! {call, self(), Msg},
    receive
        {?MODULE, Result} ->
            Result
    end.

init(Prog) ->
    register(?MODULE, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, Prog}, [{packet, 2}]),
    loop(Port).

loop(Port) ->
    receive
        {call, From, Msg} ->
            Port ! {self(), {command, encode(Msg)}},
            receive
                {Port, {data, Data}} ->
                    From ! {?MODULE, decode(Data)}
            end,
            loop(Port);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', Port, _Reason} ->
            exit(port_exit_error)
    end.

encode({strlen, X}) -> [1, list_to_binary(X)];
encode({strcmp, X, Y}) -> [2, list_to_binary(X), 0, list_to_binary(Y), 0].

decode([Int]) -> Int.
```

后面的Erlang端程序大同小异，只写关键点。C语言那边也很简单：

```
#include <unistd.h>
#include "comm.h"

int read_exact(byte*, int);
int write_exact(byte*, int);

int read_cmd(byte* buf)
{
    int len;
    if (read_exact(buf, 2) != 2)
        return -1;
    len = (buf[0] << 8) | buf[1];
    return read_exact(buf, len);
}

int write_cmd(byte* buf, int len)
{
    byte li;

    li = (len >> 8) & 0xff;
    write_exact(&li, 1);

    li = len & 0xff;
    write_exact(&li, 1);

    return write_exact(buf, len);
}

int read_exact(byte* buf, int len)
{
    int i, got = 0;
    do {
        if ((i = read(0, buf+got, len-got)) <= 0)
            return i;
        got += i;
    } while (got < len);

    return len;
}

int write_exact(byte* buf, int len)
{
    int i, wrote = 0;
    do {
        if ((i = write(1, buf+wrote, len-wrote)) <= 0)
            return i;
        wrote += i;
    } while (wrote < len);

    return len;
}
```

```
// C
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include "comm.h"

#define LOG_FILE "einterface.log"

void split_string(char* s, char** s1, char** s2)
{
	*s1 = s;
	*s2 = s + strlen(s) + 1;	
}

int main()
{
    int fn, res, len, i;
    char *s1, *s2;
    
	byte buf[100] = {0};
    
    FILE* f = fopen(LOG_FILE, "w");
    if (f == NULL)
    	return -1;

    while ( (len=read_cmd(buf)) > 0) {
        fn = buf[0];
        
        fprintf(f, "Data: ");
        for (i = 0; i < len; ++i)
        	fprintf(f, "0x%02x ", buf[i]);
        fprintf(f, "\n");
        
        if (fn == 1) {
            res = strlen((char*)buf + 1);
        } else if (fn == 2) {
        	split_string((char*)buf + 1, &s1, &s2);
        	fprintf(f, "s1: %s, s2: %s\n", s1, s2);
            res = strcmp(s1,s2);
        }

        buf[0] = res;
        write_cmd(buf, 1);
        memset(buf, 0, sizeof(buf));
    }

	fclose(f);
	
    return 0;
}
```

端口方式最简单，但是缺点也很明显，数据量大的时候效率很低。

#### Erl_Interface

准确的说，这和上一种方式一样，都是利用输入输出。Erl_Interface是Erlang官方提供的数据编码手段，用来替换我们的decode和encode。能把所有Erlang项式编码成二进制。

```
    {call, From, Msg} ->
        Port ! {self(), {command, term_to_binary(Msg)}},
        receive
            {Port, {data, Data}} ->
                From ! {?MODULE, binary_to_term(Data)}
        end,
```

C结构体的定义在erl_interface.h，缺少文档说明的接口也能在这里找到。用到的关键结构ETERM是所有Erlang基本数据结构的union。

```
typedef struct _eterm {
  union {
    Erl_Integer    ival;
    Erl_Uinteger   uival; 
    Erl_LLInteger  llval;
    Erl_ULLInteger ullval;
    Erl_Float      fval;
    Erl_Atom       aval;
    Erl_Pid        pidval;     
    Erl_Port       portval;    
    Erl_Ref        refval;   
    Erl_List       lval;
    Erl_EmptyList  nval;
    Erl_Tuple      tval;
    Erl_Binary     bval;
    Erl_Variable   vval;
    Erl_Function   funcval;
    Erl_Big        bigval;
  } uval;
} ETERM;
```

常用的转换函数也都封装成了宏：

```
#define ERL_INT_VALUE(x)  ((x)->uval.ival.i)
#define ERL_INT_UVALUE(x) ((x)->uval.uival.u)
#define ERL_LL_VALUE(x)   ((x)->uval.llval.i)
#define ERL_LL_UVALUE(x)  ((x)->uval.ullval.u)

#define ERL_FLOAT_VALUE(x) ((x)->uval.fval.f)

#define ERL_ATOM_PTR(x) erl_atom_ptr_latin1((Erl_Atom_data*) &(x)->uval.aval.d)
#define ERL_ATOM_PTR_UTF8(x) erl_atom_ptr_utf8((Erl_Atom_data*) &(x)->uval.aval.d)
#define ERL_ATOM_SIZE(x) erl_atom_size_latin1((Erl_Atom_data*) &(x)->uval.aval.d)
#define ERL_ATOM_SIZE_UTF8(x) erl_atom_size_utf8((Erl_Atom_data*) &(x)->uval.aval.d)

#define ERL_PID_NODE(x) erl_atom_ptr_latin1((Erl_Atom_data*) &(x)->uval.pidval.node)
#define ERL_PID_NODE_UTF8(x) erl_atom_ptr_utf8((Erl_Atom_data*) &(x)->uval.pidval.node)
#define ERL_PID_NUMBER(x) ((x)->uval.pidval.number)
#define ERL_PID_SERIAL(x) ((x)->uval.pidval.serial)
#define ERL_PID_CREATION(x) ((x)->uval.pidval.creation)
```

使用erl_interface的C程序如下：

```
#include <unistd.h>
#include <string.h>
#include "comm.h"
#include "erl_interface.h"
#include "ei.h"


int main()
{
	ETERM *tuplep, *intp;
	ETERM *fnp, *argp, *sp1, *sp2;

	byte buf[100] = {0};
	int res, allocated, freed;

	erl_init(NULL, 0);

    while (read_cmd(buf) > 0) {
		tuplep = erl_decode(buf);
		fnp = erl_element(1, tuplep);
		sp1 = erl_element(2, tuplep);

		if (strncmp((const char*)ERL_ATOM_PTR(fnp), "strlen", 6) == 0) {
			res = strlen(erl_iolist_to_string(sp1));
		} else if (strncmp((const char*)ERL_ATOM_PTR(fnp), "strcmp", 6) == 0) {
			sp2 = erl_element(3, tuplep);
			res = strcmp(erl_iolist_to_string(sp1), erl_iolist_to_string(sp2));
		}

    	intp = erl_mk_int(res);
    	erl_encode(intp, buf);
   		write_cmd(buf, erl_term_len(intp));

    	erl_free_compound(tuplep);
    	erl_free_term(fnp);
    	erl_free_term(argp);
    	erl_free_term(intp);
	}
    return 0;
}
```

`erl_init` 用于初始化内存管理等。

### 嵌入式端口（端口驱动）

和前面两种方式不同，嵌入式端口作为动态模块直接加载到Erlang虚拟机。Erlang端需要先加载模块：

```
start(ProgLib) ->
	case erl_ddll:load_driver("./", ProgLib) of
		ok ->
			ok;
		{error, already_loaded} ->
			ok;
		Reason ->
			io:format("error: ~p~n", [Reason]),
			exit({error, could_not_load_driver})
	end,
    spawn(?MODULE, init, [ProgLib]).
```

C程序做的变动比较大，主要是完善ErlDrvEntry接口：

```
/*
 * This structure defines a driver.
 */

typedef struct erl_drv_entry {
    int (*init)(void);		/* called at system start up for statically
				   linked drivers, and after loading for
				   dynamically loaded drivers */ 

#ifndef ERL_SYS_DRV
    ErlDrvData (*start)(ErlDrvPort port, char *command);
				/* called when open_port/2 is invoked.
				   return value -1 means failure. */
#else
    ErlDrvData (*start)(ErlDrvPort port, char *command, SysDriverOpts* opts);
				/* special options, only for system driver */
#endif
    void (*stop)(ErlDrvData drv_data);
                                /* called when port is closed, and when the
				   emulator is halted. */
    void (*output)(ErlDrvData drv_data, char *buf, ErlDrvSizeT len);
				/* called when we have output from erlang to
				   the port */
    void (*ready_input)(ErlDrvData drv_data, ErlDrvEvent event); 
				/* called when we have input from one of 
				   the driver's handles */
    void (*ready_output)(ErlDrvData drv_data, ErlDrvEvent event);  
				/* called when output is possible to one of 
				   the driver's handles */
    char *driver_name;		/* name supplied as command 
				   in open_port XXX ? */
    void (*finish)(void);        /* called before unloading the driver -
				   DYNAMIC DRIVERS ONLY */
    void *handle;		/* Reserved -- Used by emulator internally */
    ErlDrvSSizeT (*control)(ErlDrvData drv_data, unsigned int command,
			    char *buf, ErlDrvSizeT len, char **rbuf,
			    ErlDrvSizeT rlen); /* "ioctl" for drivers - invoked by
						  port_control/3 */
    void (*timeout)(ErlDrvData drv_data);	/* Handling of timeout in driver */
    void (*outputv)(ErlDrvData drv_data, ErlIOVec *ev);
				/* called when we have output from erlang
				   to the port */
    void (*ready_async)(ErlDrvData drv_data, ErlDrvThreadData thread_data);
    void (*flush)(ErlDrvData drv_data);
                                /* called when the port is about to be 
				   closed, and there is data in the 
				   driver queue that needs to be flushed
				   before 'stop' can be called */
    ErlDrvSSizeT (*call)(ErlDrvData drv_data,
			 unsigned int command, char *buf, ErlDrvSizeT len,
			 char **rbuf, ErlDrvSizeT rlen,
			 unsigned int *flags); /* Works mostly like 'control',
						  a synchronous
						  call into the driver. */
    void (*event)(ErlDrvData drv_data, ErlDrvEvent event,
		  ErlDrvEventData event_data);
                                /* Called when an event selected by 
				   driver_event() has occurred */
    int extended_marker;	/* ERL_DRV_EXTENDED_MARKER */
    int major_version;		/* ERL_DRV_EXTENDED_MAJOR_VERSION */
    int minor_version;		/* ERL_DRV_EXTENDED_MINOR_VERSION */
    int driver_flags;		/* ERL_DRV_FLAGs */
    void *handle2;              /* Reserved -- Used by emulator internally */
    void (*process_exit)(ErlDrvData drv_data, ErlDrvMonitor *monitor);
                                /* Called when a process monitor fires */
    void (*stop_select)(ErlDrvEvent event, void* reserved);
    	                        /* Called on behalf of driver_select when
				   it is safe to release 'event'. A typical
				   unix driver would call close(event) */
    void (*emergency_close)(ErlDrvData drv_data);
                                /* called when the port is closed abruptly.
				   specifically when erl_crash_dump is called. */
    /* When adding entries here, dont forget to pad in obsolete/driver.h */
} ErlDrvEntry;
```

作为样例，简单的初始化stop、start和output就行：

```
ErlDrvEntry driver_entry = {
	NULL,
	drv_start,
	drv_stop,
	drv_output,
	NULL,
	NULL,
	"port_driver",
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	ERL_DRV_EXTENDED_MARKER,
	ERL_DRV_EXTENDED_MAJOR_VERSION,
	ERL_DRV_EXTENDED_MINOR_VERSION,
	0,
	NULL,
	NULL,
	NULL
};
```

利用宏 `DRIVER_INIT` 完成初始化：

```
#ifdef STATIC_ERLANG_DRIVER
#  define ERLANG_DRIVER_NAME(NAME) NAME ## _driver_init
#  define ERL_DRIVER_EXPORT
#else
#  define ERLANG_DRIVER_NAME(NAME) driver_init
#  if defined(__GNUC__) && __GNUC__ >= 4
#    define ERL_DRIVER_EXPORT __attribute__ ((visibility("default")))
#  elif defined (__SUNPRO_C) && (__SUNPRO_C >= 0x550)
#    define ERL_DRIVER_EXPORT __global
#  else
#    define ERL_DRIVER_EXPORT
#  endif
#endif

#ifndef ERL_DRIVER_TYPES_ONLY

#define DRIVER_INIT(DRIVER_NAME) \
    ERL_DRIVER_EXPORT ErlDrvEntry* ERLANG_DRIVER_NAME(DRIVER_NAME)(void); \
    ERL_DRIVER_EXPORT ErlDrvEntry* ERLANG_DRIVER_NAME(DRIVER_NAME)(void)
```

三个回调如下：

```
typedef struct {
	ErlDrvPort port;
} data;

static ErlDrvData drv_start(ErlDrvPort port, char* buf)
{
	data* d = (data*)driver_alloc(sizeof(data));
	d->port = port;
	return (ErlDrvData)d;
}

static void drv_stop(ErlDrvData handle)
{
	driver_free(handle);
}

void split_string(char* s, char** s1, char** s2)
{
	*s1 = s;
	*s2 = s + strlen(s) + 1;	
}

static void drv_output(ErlDrvData handle, char* buf,
		ErlDrvSizeT len)
{
	data* d = (data*)handle;
	char *s1, *s2;

	int res = 0, fn = buf[0];
	
	if (fn == 1) {
		res = strlen(buf + 1);
    } else if (fn == 2) {
        split_string(buf + 1, &s1, &s2);
        res = strcmp(s1,s2);
    }

	driver_output(d->port, (char*)&res, 1);
}
```

依然是利用端口交互。

### 分布式Erlang节点

分布式方式用C创建一个Erlang节点，以分布式的方式和Erlang交互。很独特的一种方法，详细的工作原理和数据格式以后会分析。

```
%% Erlang
-module(complex4).
-export([strlen/1, strcmp/2]).

-define(CNODE, 'cnode@192.168.0.4').

strlen(S) ->
    call_port({strlen, S}).
strcmp(S, T) ->
    call_port({strcmp, S, T}).

call_port(Msg) ->
	{any, ?CNODE} ! {call, self(), Msg},
    receive
        {cnode, Result} ->
            Result
    end.
```

另一端是什么语言编写的完全无所谓。

C程序用ErlMessage封装了消息：

```
typedef struct {
  int type;   /* one of the message type constants in eiext.h */
  ETERM *msg; /* the actual message */
  ETERM *from;
  ETERM *to;
  char to_name[MAXREGLEN+1];
} ErlMessage;
```

`erl_receive_msg` 接收消息，`erl_send` 发送消息，消息格式用erl_interface封装。可以实现为服务端和客户端，服务端等待Erlang节点连接，客户端节点要在Erlang启动后发起连接：

```
	erl_init(NULL, 0);

	addr.s_addr = inet_addr("192.168.0.4");
	if (erl_connect_xinit("192.168.0.4", "cnode", "cnode@192.168.0.4",
				&addr, "123456", 0) == -1)
		erl_err_quit("erl_connect_init error");

	if ((listen = listen_port(port)) <= 0)
		erl_err_quit("listen_port error");

	if (erl_publish(port) == -1)
		erl_err_quit("erl_publish error");

	if ((fd = erl_accept(listen, &conn)) == ERL_ERROR)
		erl_err_quit("erl_accept error");
```

```
	erl_init(NULL, 0);

	addr.s_addr = inet_addr("192.168.0.4");
	if (erl_connect_xinit("192.168.0.4", "cnode", "cnode@192.168.0.4",
				&addr, "123456", 0) == -1)
		erl_err_quit("erl_connect_init error");

	if ((fd = erl_connect("e1@192.168.0.4")) < 0)
		erl_err_quit("erl_connect error");

	fprintf(stderr, "Connected to e1@192.168.0.4\n"); 
```

连接成功后就是数据交互：

```
	while (loop) {
		got = erl_receive_msg(fd, buf, BUFSIZE, &emsg);
		if (got == ERL_ERROR) {
			loop = 0;
		} else if (got == ERL_TICK) {
			// pass
		} else {
			if (emsg.type == ERL_REG_SEND) {
				fromp = erl_element(2, emsg.msg);
				tuplep = erl_element(3, emsg.msg);
				fnp = erl_element(1, tuplep);
				s1 = erl_element(2, tuplep);
				
				if (strncmp((const char*)ERL_ATOM_PTR(fnp), "strlen", 6) == 0) {
					res = strlen(erl_iolist_to_string(s1)); 
				} else if (strncmp((const char*)ERL_ATOM_PTR(fnp), "strcmp", 6) == 0) {
					s2 = erl_element(3, tuplep);
					res = strcmp(erl_iolist_to_string(s1), erl_iolist_to_string(s2));
				}

				resp = erl_format("{cnode, ~i}", res);
				erl_send(fd, fromp, resp);

				erl_free_term(emsg.from);
				erl_free_term(emsg.msg);
				erl_free_term(fromp);
				erl_free_term(tuplep);
				erl_free_term(fnp);
				erl_free_term(s1);
				erl_free_term(s2);
				erl_free_term(resp);
			}
		}
	}
```

#### NIF

最后一张方式，也是最新的，实现NIF内部函数。具体来说，就是编写一个动态链接库，加载到Erlang虚拟机，导出接口让Erlang调用。

```
-module(complex5).
-export([cstrlen/1, cstrcmp/2]).

-on_load(init/0).

init() ->
	ok = erlang:load_nif("./cnif", 0).

cstrlen(_S) ->
	exit(nif_library_not_loaded).

cstrcmp(_S, _T) ->
	exit(nif_library_not_loaded).
```

`load_nif` 加载模块，erlang形式的函数用于模块导出函数不存在时的stub。

C程序填充ERL_NIF_INIT宏：

```
#define ERL_NIF_INIT(NAME, FUNCS, LOAD, RELOAD, UPGRADE, UNLOAD) \
ERL_NIF_INIT_PROLOGUE                   \
ERL_NIF_INIT_GLOB                       \
ERL_NIF_INIT_DECL(NAME);		\
ERL_NIF_INIT_DECL(NAME)			\
{					\
    static ErlNifEntry entry = 		\
    {					\
	ERL_NIF_MAJOR_VERSION,		\
	ERL_NIF_MINOR_VERSION,		\
	#NAME,				\
	sizeof(FUNCS) / sizeof(*FUNCS),	\
	FUNCS,				\
	LOAD, RELOAD, UPGRADE, UNLOAD,	\
	ERL_NIF_VM_VARIANT,		\
	ERL_NIF_ENTRY_OPTIONS		\
    };                                  \
    ERL_NIF_INIT_BODY;                  \
    return &entry;			\
}     
```

实际上是初始化了ErlNifEntry结构体。

```
typedef struct enif_entry_t
{
    int major;
    int minor;
    const char* name;
    int num_of_funcs;
    ErlNifFunc* funcs;
    int  (*load)   (ErlNifEnv*, void** priv_data, ERL_NIF_TERM load_info);
    int  (*reload) (ErlNifEnv*, void** priv_data, ERL_NIF_TERM load_info);
    int  (*upgrade)(ErlNifEnv*, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
    void (*unload) (ErlNifEnv*, void* priv_data);
    const char* vm_variant;
    unsigned options;
}ErlNifEntry;
```

作为简单例子，这里只关注导出函数：

```
static ErlNifFunc nif_func[] = {
	{"cstrlen", 1, strlen_nif},
	{"cstrcmp", 2, strcmp_nif}
};
```

分别是函数名、元数和实现：

```
static ERL_NIF_TERM strlen_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	int ret;
	char s[100];
	
	if (!enif_get_string(env, argv[0], s, 100, 1)) {
		return enif_make_badarg(env);
	}

	ret = strlen(s);

	return enif_make_int(env, ret);
}

static ERL_NIF_TERM strcmp_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	int ret;
	char s1[100], s2[100];

	if (!enif_get_string(env, argv[0], s1, 100, 1) || 
			!enif_get_string(env, argv[1], s2, 100, 1)) {
		return enif_make_badarg(env);
	}

	ret = strcmp(s1, s2);

	return enif_make_int(env, ret);
}
```

#### 最后

完整的代码在[这里](https://github.com/0xfei/erlang_lib/tree/master/erlang_comm_c)找到。

对我而言，平时C语言写的最多，而且往往是内核模块。结合C和Erlang是很有必要的，一定要搞清楚交互方式。这几种方式都透露着Erlang的设计哲学和内部实现，接下来需要深入代码来学习了。