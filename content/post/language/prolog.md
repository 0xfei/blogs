---
title: "Prolog和逻辑编程"
date: 2014-04-04T02:43:49+08:00
draft: true
categories: ["编程语言"]
tags: ["编程语言"]
topics: ["编程语言"]
---

Prolog是我最喜欢的语言之一。ruby给人的感觉是轻松灵巧，还会有点儿惊艳，写ruby程序就像是拥抱自由；和ruby不同，Prolog规规矩矩，编程模式和常见编程语言大相径庭，通过已知自动推断未知。提供一些事实，制定一些规则，Prolog就会自动给出某些判断是yes还是no。

<!--more-->

### Prolog基础

通过一个例子来介绍：

```
animal(cat).
animal(dog).

isAnimal(X) :- animal(X).
```

在Prolog中，变量若以小写字母开头，那就是一个原子，类似ruby中的符号；大写字母或下划线开头的才是真正的变量。

这个小程序给出两个事实：cat是animal，dog也是animal。

然后有一个规则：如果animal(X)，那么isAnimal(X)。

虽然这个例子几乎毫无实用价值，不过它确实是一个完整的Prolog程序，而且我相信通过这么一个没用的程序，可以对Prolog有一个初步印象。

我们看一下这个程序的运行情况：

```
GNU Prolog 1.4.4 (64 bits)
Compiled Apr 23 2013, 16:05:07 with cl
By Daniel Diaz
Copyright (C) 1999-2013 Daniel Diaz
compiling D:/Learning/Language/Prolog/1.pl for byte code...
D:/Learning/Language/Prolog/1.pl compiled, 4 lines read - 423 bytes written, 16 ms
| ?- animal(cat).

yes
| ?- animal(big).

no
| ?- animal(dog).

yes
| ?- isAnimal(cat).

yes
| ?- isAnimal(dog).

yes
| ?- isAnimal(pig).

no
```

可以看到，Prolog就像一个诚实的人，虽然只会说yes和no。如果你有兴趣，可以试一些其他范型的编程语言中的常见语句，事实Prolog会怎么处理。

Prolog既然号称是面向逻辑语言，就一定有它自己的**逻辑能力**，推导就是它最基本的能力之一。再看一个稍微有用一些的例子：

```
diff(a,b).
diff(b,a).
diff(a,c).
diff(c,a).
diff(b,c).
diff(c,b).

color(A, B, C, D) :-
	diff(A,B),
	diff(A,D),
	diff(B,C),
	diff(C,D).
```
这里给出六个事实，abc互不相同。然后color是一个有四个变量的推论，满足相邻变量diff则是一个合法color。不太理解的话看一下输出：

```
| ?- color(a,b,c,b).

true ? 

yes
| ?- color(c,a,c,a).

true ? 

yes
| ?- color(b,a,c,c).

no
| ?- color(a,b,c,What).

What = b ? 

yes
| ?- color(a,b,c,What).

What = b ? ;

no
| ?- color(a,b,a,What).

What = b ? ;

What = c ? ;

(16 ms) no
| ?- 
```

看到了吧，Prolog可以自动推测变量来满足事实。这是很强大的功能，事实上，刚才的diff-color小程序就解决了经典了四色问题，相邻国家地图不能同色，做过OI或者ACM的人都知道，这是搜索的最入门题目，C语言二三十行左右搞定，但是Prolog只需要最最基础的事实-推论，然后自动得出结论。

### 递归

还是用例子来说明吧，Prolog语法少，理解简单，说来说去都不如一个好例子有意思。

```
fib(1,1).
fib(2,1).
fib(N,Ans) :-
	(N &gt; 2),
	A is N - 1,
	B is N - 2,
	fib(A,Ans1),
	fib(B,Ans2),
	Ans is Ans1 + Ans2.
```
斐波那契数列的Prolog版本。给出两个事实，fib(1,1)和fib(2,1)，其实它们表示的是斐波那契数列的第一项和第二项分别是1和1。不过对于Prolog，只有事实。可以看到下面的推论，fib(N,Ans)，用了递归的思想。

用Prolog的面向逻辑思维来解释就是，fib(N,Ans)是一个推论，后面是这个推论满足**是**的条件。有6个条件，这六个条件都满足，就完成了推论。

### 合一

合一是理解Prolog的关键，理解了合一，Prolog就理解了一大半了。

我觉得合一很好理解，虽然听起来很玄学。Prolog的合一和调用函数的形参实参概念类似，都是把变量或者原子对应到另一个变量或者原子。不同的是，函数调用会有形参实参，但是合一在=左右有效，在推论中也有效。

为了理解合一，需要知道Prolog中常用的一个数据结构：列表。几乎所有面向对象语言和函数式编程语言，列表都是很重要的类型，Prolog中也是。[]中括号形成空列表，中括号里头可以有原子、数字和字符串，形成列表。

列表可以分解。 [1,2,3,4,5] = [Head|Tail] ，列表被分解，Head = 1， Tail = [2,3,4,5]。这就是合一的过程，Head被赋为1，Tail被赋为[2,3,4,5]。

还有一个重要规则需要学习，append。

append([1,2,3], [4,5], Ans)，通过append规则，Prolog得出结论：Ans = [1,2,3,4,5]。

append还能做减法，append([1,2,3], Ans, [1,2,3,4,5])，Prolog得出结论： Ans = [4,5]。

看一个简单的求平均数的例子：

```
count(0, []).
count(Count, [Head|Tail]) :- count(TailCount, Tail), Count is TailCount + 1.

sum(0, []).
sum(Total, [Head|Tail]) :- sum(TailSum, Tail), Total is TailSum + Head.

average(Average, List) :- sum(Sum, List), count(Count, List), Average is Sum/Count.
```

请自行理解，讲解甚是无聊，而且也啰啰嗦嗦说不清楚。按照Prolog的思维来看这个程序：事实，逻辑，推论。

### 通过几个程序自己学习

我发现学习Prolog的最好方法就是自己写小程序来理解它面向逻辑的精髓。多说无益，写代码是最好的学习方式。

阶乘:

```
jiecheng(1,1).
jiecheng(N,Ans) :-
	N >= 1,
	M is N-1,
	jiecheng(M, Temp),
	Ans is N * Temp.
```


最小值：

```
zuixiaozhi([], nil).
zuixiaozhi([A], A).
zuixiaozhi(List, Ans) :-
	List = [Head|Tail],
	zuixiaozhi(Tail, Temp),
	Ans is min(Head, Temp).

```


翻转列表：

```
reverseList([],[]).
reverseList(List,Rlist) :-
	List = [Head|Tail],
	reverseList(Tail, Temp),
	append(Temp,[Head],Rlist).
```


快速排序:

```
divide([], _, [], []).
divide([Head|Tail], Middle, Small, Big) :-
	(	Head =&lt; Middle -&gt;
		Small = [Head|Rest],
		divide(Tail, Middle, Rest, Big)
	;
		Big = [Head|Rest],
		divide(Tail, Middle, Small, Rest)
	).

qsort([], []).
qsort([Head|Tail], Sorted) :-
	divide(Tail, Head, Small, Big),
	qsort(Small, A),
	qsort(Big, B),
	append(A, [Head], Temp),
	append(Temp, B, Sorted).

```
注意括号和if语句很相似。

八皇后问题:

```
valid_queen(Row, Col) :- member(Col, [1,2,3,4,5,6,7,8]).
valid_board([]).
valid_board(Head|Tail) :- valid_queen(Head), valid_board(Tail).

cols([],[]).
cols([(_,Col)|QueensTail], [Col|ColsTail]) :-
	cols(QueensTail, ColsTail).
	
diags1([],[]).
diags1([(Row,Col)|QueensTail], [Diag|DiagsTail]) :-
	Diag is Col - Row,
	diags1(QueensTail, DiagsTail).

diags2([],[]).
diags2([(Row,Col)|QueensTail], [Diag|DiagsTail]) :-
	Diag is Col + Row,
	diags2(QueensTail, DiagsTail).

eight_queen(Board) :-
	Board = [(1,_),(2,_),(3,_),(4,_),(5,_),(6,_),(7,_),(8,_)],
	valid_board(Board),
	cols(Board,Cols),
	diags1(Board,Diags1),
	diags2(Board,Diags2),
	fd_all_different(Cols),
	fd_all_different(Diags1),
	fd_all_different(Diags2).
```

数独问题:

```
valid([]).
valid(List) :-
	List = [Head|Tail],
	fd_all_different(Head),
	valid(Tail).
	
sudoku(Puzzle, Solution) :-
	Solution = Puzzle,
	Puzzle = [S11,S12,S13,S14,
			  S21,S22,S23,S24,
			  S31,S32,S33,S34,
			  S41,S42,S43,S44],
	fd_domain(Solution,1,4),
	Row1 = [S11,S12,S13,S14],
	Row2 = [S21,S22,S23,S24],
	Row3 = [S31,S32,S33,S34],
	Row4 = [S41,S42,S43,S44],
	
	Col1 = [S11,S21,S31,S41],
	Col2 = [S12,S22,S32,S42],
	Col3 = [S13,S23,S33,S43],
	Col4 = [S14,S24,S34,S44],
	
	Squ1 = [S11,S12,S21,S22],
	Squ2 = [S13,S14,S23,S24],
	Squ3 = [S31,S32,S41,S42],
	Squ4 = [S33,S34,S43,S44],
	
	valid([Row1,Row2,Row3,Row4,
		   Col1,Col2,Col3,Col4,
		   Squ1,Squ2,Squ3,Squ4]).
```

可以看到，Prolog是解决资源受限问题的利器。事务处理、逻辑游戏等等问题，用Prolog解决，会比其他语言方便太多。

### 关于编程语言

最开始写程序，我用Pascal，那时候常常羡慕会C语言的，莫名觉得比我们要先进。后来觉得其实什么语言都是一样的，再后来学会了C，更加坚定了这一看法。

不过随着不同类型语言的学习，觉得语言其实是有优劣之分的，最起码个性是很明显的。每一种语言都代表了一种思维，如果只会C，那么凡事都要一句话一句话来；反之如果只会Python，那么凡事都不用想，编程能力和对计算机的领悟到不了一定水平。虽然不必要每种语言都精通，而且那也是不可能的，但是多学习不同思路，还是有很大好处的。
