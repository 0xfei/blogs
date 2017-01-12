+++
title = "Erlang编译相关模块"
date = "2017-01-12T16:48:14+08:00"
draft = true
description = "Erlang compile"
tags = ["Erlang/OTP", "Code"]
topics = ["Erlang/OTP", "Code"]
+++


#### compile 模块

[compile](http://erlang.org/doc/man/compile.html) 模块提供编译接口，主要是 file/2 和 forms/2，分别接受文件和Erlang[抽象格式(Abstract Format)](http://erlang.org/doc/apps/erts/absform.html)——Erlang项式解析树的标准表现形式。第二个参数是选项，大致可以分为几类：
测试编译结果、是否产生二进制数据、调试信息、Makefile条目生成、'P'/'E'/'S'生成中间格式文件、错误和警告信息、宏定义等。具体内容看文档。

'P'、'E'、'S' 选项分别生成预处理和解析变换（基本检查、函数是否存在等）、代码转换（导入导出文件处理、宏替换、生成module_info函数）、汇编代码（.S文件即模块对应的Erlang汇编，可以用file和forms函数生成bin）文件。这也暗示了compile模块的几个过程，下面介绍相关的其他模块。

#### epp 模块

epp 模块解析Erlang源文件，并生成抽象格式。可以用open打开一个EPP句柄，并迭代处理文件，也可以用 `parse_file` 一次性生成。也提供文件编码相关的函数。

<!--more-->

```
27> {ok, Epp} = epp:open("dynamic_compile.erl", []).
{ok,<0.109.0>}
28> epp:parse_erl_form(Epp).
{ok,{attribute,1,file,{"dynamic_compile.erl",1}}}
29> epp:parse_erl_form(Epp).
{ok,{attribute,41,module,dynamic_compile}}
30> epp:parse_erl_form(Epp).
{ok,{attribute,44,export,
               [{load_from_string,1},{load_from_string,2}]}}
31> epp:parse_erl_form(Epp).
{ok,{attribute,45,export,[{from_string,1},{from_string,2}]}}
32> epp:parse_erl_form(Epp).
{ok,{attribute,47,import,
               {lists,[{reverse,1},{keyreplace,4}]}}}
33> epp:parse_erl_form(Epp).
{ok,{function,57,load_from_string,1,
              [{clause,57,
                       [{var,57,'CodeStr'}],
                       [],
                       [{call,58,
                              {atom,58,load_from_string},
                              [{var,58,'CodeStr'},{nil,58}]}]}]}}
34> epp:parse_erl_form(Epp).
{ok,{function,60,load_from_string,2,
              [{clause,60,
                       [{var,60,'CodeStr'},{var,60,'CompileFormsOptions'}],
                       [],
                       [{match,61,
                               {tuple,61,[{var,61,'Mod'},{var,61,'Bin'}]},
                               {call,61,
                                     {atom,61,from_string},
                                     [{var,61,'CodeStr'},
                                      {var,61,'CompileFormsOptions'}]}},
                        {call,62,
                              {remote,62,{atom,62,code},{atom,62,load_binary}},
                              [{var,62,'Mod'},{nil,62},{var,62,'Bin'}]}]}]}}
```

#### erl_scan 模块

`erl_scan` 将字符串转变成Erlang的token——表示语法结构的元组。token是编译的基本单位，也叫标记。`erl_scan` 中最重要的两个函数是 string和tokens，需要注意tokens参数字符串要空格结尾。

```
56> S = "1+1. [1,2,3]. fun(X) -> X+1 end. ".
"1+1. [1,2,3]. fun(X) -> X+1 end. "
57> erl_scan:string(S).
{ok,[{integer,1,1},
     {'+',1},
     {integer,1,1},
     {dot,1},
     {'[',1},
     {integer,1,1},
     {',',1},
     {integer,1,2},
     {',',1},
     {integer,1,3},
     {']',1},
     {dot,1},
     {'fun',1},
     {'(',1},
     {var,1,'X'},
     {')',1},
     {'->',1},
     {var,1,'X'},
     {'+',1},
     {integer,1,1},
     {'end',1},
     {dot,1}],
    1}
58> erl_scan:tokens([], S, 1).
{done,{ok,[{integer,1,1},{'+',1},{integer,1,1},{dot,1}],1},
      "[1,2,3]. fun(X) -> X+1 end. "}
```

#### erl_parse 模块

`erl_parse` 将 token 变成抽象结构体，是最重要的解析模块。

```
5> {ok, Tokens, _} = erl_scan:string("[1,2,3]. ").
{ok,[{'[',1},
     {integer,1,1},
     {',',1},
     {integer,1,2},
     {',',1},
     {integer,1,3},
     {']',1},
     {dot,1}],
    1}
6> erl_parse:parse_term(Tokens).
{ok,[1,2,3]}
7> f(Tokens).
ok
8> {ok, Tokens, _} = erl_scan:string("fun(X) -> X + 1 end. ").
{ok,[{'fun',1},
     {'(',1},
     {var,1,'X'},
     {')',1},
     {'->',1},
     {var,1,'X'},
     {'+',1},
     {integer,1,1},
     {'end',1},
     {dot,1}],
    1}
9> erl_parse:parse_exprs(Tokens).
{ok,[{'fun',1,
            {clauses,[{clause,1,
                              [{var,1,'X'}],
                              [],
                              [{op,1,'+',{var,1,'X'},{integer,1,1}}]}]}}]}
```

parse_exprs/1 解析表达式，还有parse_term和parse_form。parse_term可以把字符串变成Erlang项，自定义配置文件时很有用；parse_form能解析完整的函数体。

#### beam_lib

[`beam_lib`](http://erlang.org/doc/man/beam_lib.html) 提供控制Erlang可执行文件.beam的接口。beam文件也是分成多个chunk，类似PE文件或jvm的段：

*abstract_code ("Abst")
*atoms ("Atom")
*attributes ("Attr")
*compile_info ("CInf")
*exports ("ExpT")
*imports ("ImpT")
*indexed_imports ("ImpT")
*labeled_exports ("ExpT")
*labeled_locals ("LocT")
*locals ("LocT")

*abstract_code* 编译时附加调试信息才有，官方文档有个反汇编的例子：

```
{ok,{_,[{abstract_code,{_,AC}}]}} = beam_lib:chunks(Beam,[abstract_code]).
io:fwrite("~s~n", [erl_prettypr:format(erl_syntax:form_list(AC))]).
```

除了chunks函数返回段数据，还提供了md5、info、cmp和去除和加密调试信息的相关函数。

#### [dynamic_compile](https://github.com/jkvor/dynamic_compile)

`dynamic_compile` 实际上重新实现了编译功能，`load_from_string/1` 加载字符串形式的模块。也是利用 `epp`、`erl_scan` 和 `erl_parse` 模块，处理了include文件、宏定义、record的类型，大部分代码都在处理宏。

scanner函数以文件内容、行数、宏字典为参数，返回tokens、文件包含或者宏定义，返回tokens时调用 `erl_parse:parse_form/1` ，返回include迭代处理include文件，macro会扩充宏字典。

```
scanner(Text, Line, MacroDict) ->
    case erl_scan:tokens([],Text,Line) of
        {done, {ok,Toks,NLine}, LeftOverChars} ->
            case pre_proc(Toks, MacroDict) of
                {tokens,  NToks}      -> {tokens,  NLine, LeftOverChars, NToks};
                {macro,   NMacroDict} -> {macro,   NLine, LeftOverChars, NMacroDict};
                {include, Filename}   -> {include, NLine, LeftOverChars, Filename}
            end;
        {more, _Continuation} ->
            %% This is supposed to mean "term is not yet complete" (i.e. a '.' has
            %% not been reached yet).
            %% However, for some bizarre reason we also get this if there is a comment after the final '.' in a file.
            %% So we check to see if Text only consists of comments.
            case is_only_comments(Text) of
                true  ->
                    done;
                false ->
                    throw({incomplete_term, Text, Line})
            end
    end.
```

`pre_proc` 处理tokens。

```
pre_proc([{'-',_},{atom,_,define},{'(',_},{_,_,Name}|DefToks],MacroDict) ->
    false = dict:is_key(Name, MacroDict),
    case DefToks of
    	[{',',_} | Macro] ->
    	    {macro, dict:store(Name, {[], macro_body_def(Macro, [])},  MacroDict)};
    	[{'(',_} | Macro] ->
    	    {macro, dict:store(Name, macro_params_body_def(Macro, []), MacroDict)}
    end;

pre_proc([{'-',_}, {atom,_,include}, {'(',_}, {string,_,Filename}, {')',_}, {dot,_}], _MacroDict) ->
    {include, Filename};

pre_proc(Toks,MacroDict) ->
    {tokens, subst_macros(Toks, MacroDict)}.
```

宏定义有两种形式：`-define(TEST, 123).` 或者 `-define(TEST(X,Y), X+Y).` ，抽象格式分别是：

```
[{'-',2},{atom,2,define},{'(',2},{var,2,'TEST'},{',',2},{integer,2,123},{')',2},{dot,2}]
[{'-',1},{atom,1,define},{'(',1},{var,1,'TEST'},{'(',1},{var,1,'X'},{',',1},{var,1,'Y'},{')',1},{',',1},{var,1,'X'},{'+',1},{var,1,'Y'},{')',1},{dot,1}]
```

所以才有 `macro_body_def` 和 `macro_params_body_def` ，`subst_macros_rev` 替换宏也类似。

直观来看，Erlang应该提供了编译字符串的工具，毕竟这和 `compile:file/2` 编译文件几乎一样，但是compile确实没有提供 `compile:string/2` 。而 `dynamic_compile` 实现了这一点。

#### [smerl](https://github.com/deadtrickster/smerl)

smerl 提供了比 `dynamic_compile` 更强大的元编程能力，可以动态添加函数，获取模块信息等。很多函数没有异常检查，生产环境使用还需要大规模修改。

```
Erlang/OTP 18 [erts-7.3] [source] [64-bit] [smp:4:4] [async-threads:10] [kernel-poll:false]

Eshell V7.3  (abort with ^G)
1> c(smerl).
{ok,smerl}
2> M1 = smerl:new(test).
{meta_mod,test,undefined,[],[],false}
3> {ok, M2} = smerl:add_func(M1, "test(K) -> K+1. ").
{ok,{meta_mod,test,undefined,
              [{test,1}],
              [{function,1,test,1,
                         [{clause,1,
                                  [{var,1,'K'}],
                                  [],
                                  [{op,1,'+',{var,1,'K'},{integer,1,1}}]}]}],
              false}}
4> {ok, M3} = smerl:add_func(M2, "test(K,J) -> K+J. ").
{ok,{meta_mod,test,undefined,
              [{test,2},{test,1}],
              [{function,1,test,2,
                         [{clause,1,
                                  [{var,1,'K'},{var,1,'J'}],
                                  [],
                                  [{op,1,'+',{var,1,'K'},{var,1,'J'}}]}]},
               {function,1,test,1,
                         [{clause,1,
                                  [{var,1,'K'}],
                                  [],
                                  [{op,1,'+',{var,1,'K'},{integer,1,1}}]}]}],
              false}}
5> smerl:compile(M3).
ok
6> test:test(16).
17
7> test:test(16,14).
30
```

原理很好理解，利用 `erl_scan` `epp` 和 `erl_parse` 模块解析字符串，动态修改beam文件。详细看了下代码，写得很挫，每次修改后都重新编译，没什么实用价值。
