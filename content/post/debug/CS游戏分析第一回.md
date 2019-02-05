---
title: "CS游戏分析第一回——做到无敌"
date: 2014-12-21T09:13:30+08:00
draft: true
categories: ["调试逆向"]
tags: ["调试逆向"]
topics: ["调试逆向"]
---

### 前言

游戏安全一直是敏感的灰色话题，很多技术不会公开，从事游戏攻防的也很低调。这造成一个问题，对游戏安全感兴趣的人会无从下手，比如我，就不知道面对复杂的游戏如何分析。经过一些日子的思考学习，总算有了点思路，就以CS为例，介绍如何分析游戏、辅助工具以及相应的安全防护。

选择CS有几个原因：首先它是我第一次碰电脑就接触的东西，感情深厚；其次CS逻辑清晰易懂，没有复杂的反调试、加密等防护；最后就是基本所有人都玩过，写例子大家也容易理解。打算写一个小系列，从简单的不死、子弹不减到联网状态的数据包传输、找到各个功能实现call、人物画面渲染等，最后了解整个游戏结构，并写出防护软件来保护CS。

<!--more-->

今天写第一回，血量、子弹分析。涉及到的工具有CE、OD、IDA、PCHunter32、notepad++、CS1.6中文版、VS2005。最后实现自己不掉血、不减少子弹。假设读者有一定逆向基础，了解windows编程。并没有分析过CS，也不太了解游戏分析流程，毕竟求大神飘过无视。工具使用只会略作介绍。


#### 子弹不减

打开CS，选fy_iceworld雪地，方便捡枪的图。先用手枪，刚开始20发子弹。打开CE，附加到CS，扫描4字节的精确十进制数20，这里我默认子弹数用四字节。首次扫描，找出了内存中所有值为20的4字节数据的地址，非常多，需要排除。打一枪，再扫描19，剩余197项；再来几枪，扫描子弹剩余数，这次都跟着变化了，如下图所示。所以挨着查看分析。

![pic1](/images/cs11.png "")

先想想，实际上我们需要找到保存子弹数目的原始地址，这个地址的值是运算得来的。其它地方，比如用于显示图上“16|40”数字的地址，它的值应该是直接复制而来的。所以我们改原始地址的值，其它地方应该会相应的改变。这是一种假设，是我们设计程序的时候采用的方法。不妨试试挨着修改。

![pic2](/images/cs12.png "")

非常幸运，改地址列表中的第一个地址的值，就验证了我们的猜测。至于为什么能想到这个，我想这就是学习的目的吧。

找到了需要的原始地址，接下来该确定什么代码改写了它。内存写断点可以实现。右键CE中我们找到的地址，选“找出什么改写了这个地址”，将CS载入CE内置的调试器并设好了内存写断点。然后我们打一枪，就能找到减少子弹的代码了。如下图：

![pic3](/images/cs13.png "")

改变子弹数目的代码如下：

```
mp.weapon_glock18+5EA - 48                    - dec eax
mp.weapon_glock18+5EB - 8B CE                 - mov ecx,esi
mp.weapon_glock18+5ED - 89 86 CC000000        - mov [esi+000000CC],eax
```

很明显dec eax，子弹减了一个。我最开始做到这一步的时候，非常高兴，马上用OD附加到CS，然后分析前后代码，纠结减少子弹和换弹夹的问题。而且上面代码的ESI明显是个对象或者结构体，试图确定每一个成员的作用。但是后来我发现那样有点事倍功半，CE的调试器很好的利用了CS的符号表，告诉我们一个非常明确的信息：现在是在mp模块的weapon_glock18部分，手枪的名字就是glock18咯。

为了验证是不是每个武器都有一部分代码，我们换一把枪，用同样的方法（子弹数扫描并查找代码），找到了AK47的子弹处理部分：

![pic4](/images/cs14.png "")

跟上面手枪的处理非常相似。这个时候其实我们可以不必用OD动态调试了，完全可以用分析mp.dll，找到其中关于武器的代码：

自动分析后看到如下函数：

```
weapon_ak47         .text 100011E0 0000004F R . . . . . .
weapon_aug          .text 10001B40 0000004F R . . . . . .
weapon_awp          .text 10002270 0000004F R . . . . . .
weapon_c4           .text 10002950 0000004F R . . . . . .
weapon_deagle       .text 10003410 0000004F R . . . . . .
weapon_elite        .text 10003AD0 0000004F R . . . . . .
weapon_famas        .text 100042E0 0000004F R . . . . . .
weapon_fiveseven    .text 10004A80 0000004F R . . . . . .
weapon_flashbang    .text 100051A0 0000004F R . . . . . .
weapon_g3sg1        .text 100059E0 0000004F R . . . . . .
weapon_galil        .text 10005FE0 0000004F R . . . . . .
weapon_glock18      .text 10006650 0000004F R . . . . . .
weapon_hegrenade    .text 10006FC0 0000004F R . . . . . .
weapon_knife        .text 100076E0 0000004F R . . . . . .
weapon_m249         .text 10008A90 0000004F R . . . . . .
weapon_m3           .text 10009100 0000004F R . . . . . .
weapon_m4a1         .text 10009870 0000004F R . . . . . .
weapon_mac10        .text 1000A190 0000004F R . . . . . .
weapon_mp5navy      .text 1000A780 0000004F R . . . . . .
weapon_p228         .text 1000AD60 0000004F R . . . . . .
weapon_p90          .text 1000B3D0 0000004F R . . . . . .
weapon_scout        .text 1000BA30 0000004F R . . . . . .
weapon_sg550        .text 1000C0B0 0000004F R . . . . . .
weapon_sg552        .text 1000C830 0000004F R . . . . . .
weapon_shield       .text 10093170 0000004F R . . . . . .
weapon_smokegrenade .text 1000CEE0 0000004F R . . . . . .
weapon_tmp          .text 1000D6D0 0000004F R . . . . . .
weapon_ump45        .text 1000DCF0 0000004F R . . . . . .
weapon_usp          .text 1000E2C0 0000004F R . . . . . .
weapon_xm1014       .text 1000EC10 0000004F R . . . . . .
weaponbox           .text 100B8860 0000004F R . . . . . .
```

同时还有更意外的发现，mp.dll就是CS的核心，不仅武器部分，甚至BOT的AI也是在这里实现的。我们可以看到很多有价值的函数，下面选取一部分：

```
CAirtank::TankThink(void)   
CAirtank::TankTouch(CBaseEntity)
CAmbientGeneric::RampThink(void) 
CAmbientGeneric::ToggleUse(CBaseEntity*,CBaseEntity,USE_TYPE,float)
CArmoury::ArmouryTouch(CBaseEntity*)
CBaseButton::ButtonBackHome(void)     
CBaseButton::ButtonTouch(CBaseEntity*)
CBaseDelay::DelayThink(void)
CBaseDoor::DoorTouch(CBaseEntity*)
CBaseMonster::CorpseFallThink(void)
CBasePlayer::PlayerDeathThink(void)       
CBasePlayerAmmo::DefaultTouch(CBaseEntity*)
CBasePlayerItem::DestroyItem(void)                                                                                          
CBaseToggle::AngularMoveDone(void)
CBaseToggle::LinearMoveDone(void)  
CBombTarget::BombTargetTouch(CBaseEntity*)   
CBreakable::Die(void) 
CBuyZone::BuyTouch(CBaseEntity*)  
CCSBot::BotTouch(CBaseEntity*) 
CEnvExplosion::Smoke(void)      
CEnvSpark::SparkStart(CBaseEntity*,CBaseEntity*,USE_TYPE,float)
CWeaponBox::Kill(void)
```

可以看到，只要把mp.dll分析清楚，CS基本就搞清楚了。了解这么多之后，可以回到最初的目标：子弹不减。其实只要简单的把dec eax改成 inc eax就行了。子弹就不会减少了。

这时候我们打枪子弹会增加，直到127，然后减少，变成0之后又增加。这也说明子弹数目是一个字节来保存的。具体的变化可以自己试一下，非常有意思。

我们的第一个目标已经实现了：子弹不减。但是还有很多问题，难道我们每次换把枪都要用CE来这么一次？然后修改？这么搞都超时断开了。我们需要把武器和人物结合起来，这样就可以定点修改了。总不能敌人的子弹也不减少吧？

### 血量分析

经过找子弹的简单练习，应该更坚信了能实现不死的效果。可能会觉得按照上述步骤，找血、扫描、修改然后找到原始地址就行了，但是实际上血量的查找和修改要复杂不少。

还是用CE扫描，和找子弹类似，先扫描值为100的地址，然后自己跳楼减少血量，再次扫描。为什么跳楼呢？这应该是最方便的可控减血方式。这次用cs_assault图。和上述找子弹步骤类似，扫描几次之后确定了几个可能是血量保存地址的地方：

![pic5](/images/cs15.png "")

这次依然利用上一节方法，修改并观察其他的是否跟着改变。发现这几个只有一个可以修改，但是修改之后只是显示满血，一跳楼就又变成理论血量了，该死还是死。只是看起来好看一些。

![pic6](/images/cs16.png "")
![pic7](/images/cs17.png "")

虽然显示变成了100，但是实际血量正如其它地方显示的：24。跳楼之后果然成功挂掉。之前的猜测不管用了。只好换一个思路。

肯定有一个地址是与众不同的，它第一时间保存变化后的血量。其它的地址类似上面找子弹的路人甲乙丙丁地址，都是直接赋值而来。那么哪个是第一时间接收到变化后的血量的呢？我们不妨看一下改变这些地址的代码，如果是路人甲，那么它应该被改写非常多的，即使血量不变化，也会有代码执行，毕竟得看初始地址眼色行事，不积极一点就跟不上节奏了。所以我们就看血量不变化的情况下是这些地址有没有被写入。

0011984C疯狂的被改写，明显是路人甲：

![pic8](/images/cs18.png "")

0012BE74虽然不那么疯狂，但也是明显路人乙的节奏：

![pic9](/images/cs19.png "")

接下来我们看到了主角：0096C5BC，明显的稳如泰山，血量不变化的情况下不被改写。

![pica](/images/cs1a.png "")

我们试着跳楼：

![picb](/images/cs1b.png "")

看到只改写了一次，可以确定这就是我们要找的地址和代码。
仔细看截图会发现跳楼前后地址变了，确实是。写文章的过程中得自己调试着来，中间不小心卡死了，重来了一次，局部变量地址发生了变化，但是思路是连续的。

目前为止，我们可以认为找到了改变血量的地址：

```
061DAFF8 - fld dword ptr [edi+00000160]
061DAFFE - call mp.worldspawn+26280
061DB003 - mov [esi+000005B4],eax
061DB009 - fld dword ptr [edi+000001BC]
061DB00F - call mp.worldspawn+26280
```

为了方便使用，之后的调试我们使用OD。附加到CS并在61DB003处下断。跳楼减血顺利断下，可以看到EAX的值正是将要变成的血量：24。

![picc](/images/cs1c.png "")

现在我们的任务就是查找EAX从何而来。这里有个小技巧，我们在61DB003之前下断点，如果只有跳楼减血的时候断下，那说明这个地址的代码和61DB003是承接起来的一个流程，如果一直在断下或者跳楼也没断下，那这个地址和我们要确定的EAX来源关系应该不大。

往上翻看代码，大致了解一下流程，建议倒着看，因为我就是倒着分析的，所以思考方式也是从结果往前推导。每段注释都是针对注释下面的语句：

```
061DAF77                    83C4 14         ADD ESP,14
061DAF7A                    FF15 6CA52A06   CALL DWORD PTR DS:[62AA56C]              ; cstrike.01D79590
061DAF80                    892D 74A42A06   MOV DWORD PTR DS:[62AA474],EBP
061DAF86                    8B7E 04         MOV EDI,DWORD PTR DS:[ESI+4]
//	这里又是一个将[EDI+160]的值赋值到st(0)的语句，同时EDI = [ESI + 4]，和我们下面看到的一样。如果当时我们纠结ESI的来源，不往前看，就有可能忽略这里。下断看看这个时候[EDI+160]是不是已经变成了将要变成的血值。下断之后发现一直断下，而且[EDI+160]的血值是之前的，而不是之后的。因为游戏刚开始的时候这里一直断下，根本走不开，而[EDI+160]的值是100.000。那么我们有必要看看下面那个call，6220AE0。直觉告诉我们这里有可能是计算血量的地方。倒着看到这里，该看下一段代码了，也就是6220AE0 call的内部。
061DAF89                    D987 60010000   FLD DWORD PTR DS:[EDI+160]
061DAF8F                    E8 4C5B0400     CALL mp.06220AE0
061DAF94                    3B86 B4050000   CMP EAX,DWORD PTR DS:[ESI+5B4]
061DAF9A                    74 6D           JE SHORT mp.061DB009
//	这个jmp直接跳到了本段代码最后，所以正常的减血赋值操作应该不会跳转，所以说下面这条指令是会执行的。下个断点跳楼就足以证明。你可能已经猜到了，在这个地方 [EDI+160]已经是将要变成的血量了，所以还得往前寻找。
061DAF9C                    D987 60010000   FLD DWORD PTR DS:[EDI+160]
061DAFA2                    D81D F0622306   FCOMP DWORD PTR DS:[62362F0]
061DAFA8                    DFE0            FSTSW AX
061DAFAA                    25 00410000     AND EAX,4100
//	下面两个判断分支加上几个全局call的调用，暂时先不要陷入call的追踪，还是先看看之前是不是已经有结果了。这是经验之谈，盲目的追踪一个call只会浪费时间和让人越来越烦。在这里就可以试着下断，看看赋值减血的流程是跟着哪个步骤来的。经过在AFB1和AFBD下断，我发现AFB1会断下，顺着走当然会跳过AFBD的赋值0操作。同时我发现在061DAFB1这个地址处，[EDI+160]的值已经是将要变成的血值了，也就是说下面叨咕半天的又是没用的，还得往前推。
061DAFAF                    75 0C           JNZ SHORT mp.061DAFBD
061DAFB1                    D987 60010000   FLD DWORD PTR DS:[EDI+160]
061DAFB7                    D95C24 10       FSTP DWORD PTR SS:[ESP+10]
061DAFBB                    EB 08           JMP SHORT mp.061DAFC5
061DAFBD                    C74424 10 00000>MOV DWORD PTR SS:[ESP+10],0
061DAFC5                    8B8F 08020000   MOV ECX,DWORD PTR DS:[EDI+208]
061DAFCB                    8B15 94B22A06   MOV EDX,DWORD PTR DS:[62AB294]
061DAFD1                    51              PUSH ECX
061DAFD2                    55              PUSH EBP
061DAFD3                    52              PUSH EDX
061DAFD4                    6A 01           PUSH 1
061DAFD6                    FF15 68A52A06   CALL DWORD PTR DS:[62AA568]              ; cstrike.01D794B0
061DAFDC                    D94424 20       FLD DWORD PTR SS:[ESP+20]
061DAFE0                    E8 FB5A0400     CALL mp.06220AE0
061DAFE5                    50              PUSH EAX
061DAFE6                    FF15 70A52A06   CALL DWORD PTR DS:[62AA570]              ; cstrike.01D79820
061DAFEC                    83C4 14         ADD ESP,14
//	一个未知全局call，有可能改变 [EDI+160]
061DAFEF                    FF15 6CA52A06   CALL DWORD PTR DS:[62AA56C]              ; cstrike.01D79590
//	FLD 指令将 [EDI + 160]装入 st(0)，如果多一个心眼儿看看[EDI+160]的浮点数，会发现是24.000，这其实告诉我们下面那个CALL和最终的血量没啥关系，如果提前考虑这个问题，就不必费心查看那个CALL内部了。EDI = [ESI + 4]，这个时候问题已经变成追踪ESI了。
061DAFF5                    8B7E 04         MOV EDI,DWORD PTR DS:[ESI+4]
061DAFF8                    D987 60010000   FLD DWORD PTR DS:[EDI+160]
//	调用了一个函数，可能返回将要变成的血量
061DAFFE                    E8 DD5A0400     CALL mp.06220AE0
//	EAX 是将要变成的血量
061DB003                    8986 B4050000   MOV DWORD PTR DS:[ESI+5B4],EAX
061DB009                    D987 BC010000   FLD DWORD PTR DS:[EDI+1BC]
```

我们看看全局call是什么功能。内部实现如下：

```
06220AE0                             55              PUSH EBP
06220AE1                             8BEC            MOV EBP,ESP
06220AE3                             83C4 F4         ADD ESP,-0C
06220AE6                             9B              WAIT
```

```
FSTCW dest
将FPU的控制字保存到dest
```

```
//	执行 FSTCW 语句之后 [EBP-2]的值变成027F
06220AE7                             D97D FE         FSTCW WORD PTR SS:[EBP-2]
06220AEA                             9B              WAIT
06220AEB                             66:8B45 FE      MOV AX,WORD PTR SS:[EBP-2]
06220AEF                             80CC 0C         OR AH,0C
//	这里做一个运算，把FPU的控制字和C做或运算，然后放到[EBP-4]这个WORD中。
06220AF2                             66:8945 FC      MOV WORD PTR SS:[EBP-4],AX
FLDCW src
从src装入FPU的控制字
FPU CW <-src (mem16)
```

```
//	将异或之后的值放入控制寄存器，整个过程相当于自己或。FPU = FPU | 0xC，也就是将第12为置位。这里貌似是改变进位设置，我也没有深究。
06220AF6                             D96D FC         FLDCW WORD PTR SS:[EBP-4]
//	这里是将浮点栈的值放入 [EBP-C] 并出栈。可以看到浮点是是 QWORD 级别，8个字节
06220AF9                             DF7D F4         FISTP QWORD PTR SS:[EBP-C]
//	控制字符串恢复
06220AFC                             D96D FE         FLDCW WORD PTR SS:[EBP-2]
//	这个赋值可以看到这个函数相当于把浮点栈顶的值放到EAX中。仔细看上面那段代码，这个call执行之前的指令是FLD DWORD PTR DS:[EDI+160]，也就是说这个函数的作用只是把 [EDI+160]的值放入EAX。
06220AFF                             8B45 F4         MOV EAX,DWORD PTR SS:[EBP-C]
06220B02                             8B55 F8         MOV EDX,DWORD PTR SS:[EBP-8]
06220B05                             C9              LEAVE
06220B06                             C3              RETN
```

看到这里应该明白那个跳转的意思了，`CMP EAX,DWORD PTR DS:[ESI+5B4]`，比较 `[EDI+160]`和 `[ESI+5B4]`的值，不相等就进入下面的处理流程，进行同步。在`061DAF9C    D987 60010000   FLD DWORD PTR DS:[EDI+160]` 这里下断，查看`[ESI+5B4]`和`[EDI+5B4]`的值。如下：
`[EDI+160]`

![picd](/images/cs1d.png "")

`[ESI+5B4]`

![pice](/images/cs1e.png "")

高下立判。[EDI+160]才是真正的血量存储池。而我们找了半天的地方其实只是一个转换。现在才有资本去找真正改变血量的地方。既然[EDI+160]是改变血量的地方，那么就在该地址下内存写入断点。
因为之前调试的时间问题，游戏可能会断开，这个时候重新开图。初始化的时候断了下来，完全的意外收获，我们找到了player初始化的地方：

```
061D73BB    83C4 0C                    ADD ESP,0C
061D73BE    8B15 1CA72A06              MOV EDX,DWORD PTR DS:[62AA71C]           ; cstrike.02517D60
061D73C4    55                         PUSH EBP
061D73C5    B8 A8522506                MOV EAX,mp.062552A8                      ; ASCII "player"
061D73CA    53                         PUSH EBX
061D73CB    2B82 98000000              SUB EAX,DWORD PTR DS:[EDX+98]
061D73D1    8907                       MOV DWORD PTR DS:[EDI],EAX
061D73D3    8B0D 1CA72A06              MOV ECX,DWORD PTR DS:[62AA71C]           ; cstrike.02517D60
061D73D9    8B91 98000000              MOV EDX,DWORD PTR DS:[ECX+98]
061D73DF    03D0                       ADD EDX,EAX
061D73E1    52                         PUSH EDX
061D73E2    57                         PUSH EDI
061D73E3    E8 981BFCFF                CALL mp.06198F80
061D73E8    8B46 04                    MOV EAX,DWORD PTR DS:[ESI+4]
061D73EB    83C4 0C                    ADD ESP,0C
061D73EE    C780 60010000 0000C842     MOV DWORD PTR DS:[EAX+160],42C80000
061D73F8    8A86 C4010000              MOV AL,BYTE PTR DS:[ESI+1C4]
061D73FE    3AC3                       CMP AL,BL
061D7400    75 0F                      JNZ SHORT mp.061D7411
061D7402    8B4E 04                    MOV ECX,DWORD PTR DS:[ESI+4]
061D7405    8999 BC010000              MOV DWORD PTR DS:[ECX+1BC],EBX
061D740B    899E C0010000              MOV DWORD PTR DS:[ESI+1C0],EBX
061D7411    8B56 04                    MOV EDX,DWORD PTR DS:[ESI+4]
061D7414    B9 00000040                MOV ECX,40000000
061D7419    C782 10020000 00007A44     MOV DWORD PTR DS:[EDX+210],447A0000
061D7423    8B46 04                    MOV EAX,DWORD PTR DS:[ESI+4]
061D7426    8988 6C010000              MOV DWORD PTR DS:[EAX+16C],ECX
061D742C    8B56 04                    MOV EDX,DWORD PTR DS:[ESI+4]
061D742F    B8 03000000                MOV EAX,3
061D7434    8982 0C010000              MOV DWORD PTR DS:[EDX+10C],EAX
061D743A    8B56 04                    MOV EDX,DWORD PTR DS:[ESI+4]
061D743D    8982 08010000              MOV DWORD PTR DS:[EDX+108],EAX
061D7443    8B46 04                    MOV EAX,DWORD PTR DS:[ESI+4]
061D7446    8B90 60010000              MOV EDX,DWORD PTR DS:[EAX+160]
061D744C    8990 B0010000              MOV DWORD PTR DS:[EAX+1B0],EDX
061D7452    8B46 04                    MOV EAX,DWORD PTR DS:[ESI+4]
061D7455    8BB8 A4010000              MOV EDI,DWORD PTR DS:[EAX+1A4]
061D745B    81E7 00001000              AND EDI,100000
```

此时EAX为03C31BFC。不妨先剧透一下，这个值是让我头疼了一天一夜的值。它是“你”的对象地址。“你”指的是玩家。New一个对象的时候最想分配到的就是03C31BFC，被“你”占用了。这段初始化代码可以告诉我们很多信息，比如初始血量，42C80000显然是100.000的浮点形式。而160这个偏移也让我们浮想联翩。没错，之前那个EDI也是player对象的首地址。因为本节的目的是不掉血，所以暂时跳过这段精彩的发现。

接着运行，让游戏跑起来。然后我们跳楼，这时就会找到修改 对象+160处偏移的代码，也就是掉血的代码。因为下内存写断点会非常卡，要跳楼也是需要时间的。趁这点时间写几点无关的体会和经验：关注重点，不要随便进入一个call就分析；多注意关键代码的前后逻辑，多下断尝试，注意观察关键内存地址和寄存器的值；调CS的时候鼠标可能会跳出来，这个时候摁两下win键就OK了；mp.dll每次加载基址可能会变，所以需要记下偏移，最开始我就吃了这个大亏。

好了，跳楼成功，断下在这里：

```
061A6ED6                             8B46 04                    MOV EAX,DWORD PTR DS:[ESI+4]
061A6ED9                             8B8B 08020000              MOV ECX,DWORD PTR DS:[EBX+208]
061A6EDF                             8988 8C010000              MOV DWORD PTR DS:[EAX+18C],ECX
061A6EE5                             8B46 04                    MOV EAX,DWORD PTR DS:[ESI+4]
061A6EE8                             D94424 54                  FLD DWORD PTR SS:[ESP+54]
061A6EEC                             D880 D8010000              FADD DWORD PTR DS:[EAX+1D8]
061A6EF2                             D998 D8010000              FSTP DWORD PTR DS:[EAX+1D8]
061A6EF8                             8B46 04                    MOV EAX,DWORD PTR DS:[ESI+4]
061A6EFB                             D980 60010000              FLD DWORD PTR DS:[EAX+160]
061A6F01                             D86424 54                  FSUB DWORD PTR SS:[ESP+54]
061A6F05                             D998 60010000              FSTP DWORD PTR DS:[EAX+160]
061A6F0B                             83BE 3C010000 06           CMP DWORD PTR DS:[ESI+13C],6
可以看到经过一个简单的运算，算出一个减数，[ESP+54]，被减数为原血值。所以最最简单的方法就是把减数[ESP+54]变成0。这样理论上血值就不会减少了，一直是100。改一下试试：
061A6ED6                             8B46 04                    MOV EAX,DWORD PTR DS:[ESI+4]
061A6ED9                             8B8B 08020000              MOV ECX,DWORD PTR DS:[EBX+208]
061A6EDF                             8988 8C010000              MOV DWORD PTR DS:[EAX+18C],ECX
061A6EE5                             8B46 04                    MOV EAX,DWORD PTR DS:[ESI+4]
061A6EE8 				            	C74424 54 00000000       MOV DWORD PTR SS:[ESP+54],0
061A6EF0               		       	90                       NOP
061A6EF1                         	90                       NOP
061A6EF2                             D998 D8010000            FSTP DWORD PTR DS:[EAX+1D8]
061A6EF8                             8B46 04                  MOV EAX,DWORD PTR DS:[ESI+4]
061A6EFB                             D980 60010000            FLD DWORD PTR DS:[EAX+160]
061A6F01                             D86424 54                 FSUB DWORD PTR SS:[ESP+54]
061A6F05                             D998 60010000             FSTP DWORD PTR DS:[EAX+160]
061A6F0B                             83BE 3C010000 06         CMP DWORD PTR DS:[ESI+13C],6
```

从这个简单运算中我们也可以对player对象更进一步的了解。比如偏移+1D8的值用于计算减少的血值。通过这个值我们还能找到血量变化的运算，进而找到跳楼、射击、炸弹等动作，这都是后话。

现在改了之后所有人都是不死之身了，玩儿起来就是血腥，狂喷血，谁也死不了。发一张血腥的游戏截图，少儿不宜啊：

![picf](/images/cs1f.png "")

分析几个参数和dll注入实现最初的功能

在我们修改代码的那里下断，每回掉血都会断下来，我们分析一下这几个参数。

![picg](/images/cs1g.png "")

EAX和EBX造型差不多，而EAX我们非常熟悉，根据之前的剧透，这正是“我”的地址。既然这是一个掉血的过程，那么一个掉血，必然有一个砍刀的，不妨猜测EBX存储的是攻击者的对象首地址，EAX存储被攻击者的首地址。为什么这么猜测呢？因为我故意不动让BOT来砍我。换我射击BOT，断下来又是另一番情景：

![pich](/images/cs1h.png "")

EAX 和 EBX 值换了，再仔细分析其它几个寄存器的值，包括它们作为地址存储的值，分析之后得出以下结论：

```
EAX = 被攻击者首地址
EBX =  攻击者首地址
ECX =  EBX - 80
EDX =  [ESI]
EBP =  ESI
EDI =  Kill or SelfKill
```

EDI是怎么知道的呢？我发现跳楼和其他的情况到这里的时候EDI值不同，自杀是0x0002，他杀是0x1002。
EBX在数据窗口跟随就可以看到“我”这个对象，还能找到坐标等有价值的值。

现在我们改变一下HOOK方式，不修改代码，而是跳到一个地方，根据EAX的值进行相应的变化。之前提到过，初始化时把03C31BFC地址分配给自己，所以根据EAX的值选择血值不变还是照常减少，这样又可杀敌，又可保命。

我选择远程写入方式，在CS进程分配地址并写入hook代码。根据mp.dll的加载地址和减血偏移，采用inline hook。代码如下：

```
#include <windows.h>
#include <TlHelp32.h>
#include <stdio.h>

#define BLOOD_OFFSET		0x66EE8
#define BLOOD_ADDR(base)	((base)+BLOOD_OFFSET)	

DWORD g_csProcessId = 0;
DWORD g_mpBase = 0;
DWORD g_bloodAddr = 0;

BYTE g_originalBlood[10] = {0xD9,0x44,0x24,0x54,0xD8,0x80,0xD8,0x01,0x00,0x00};
BYTE g_patchedBlood[10] = {0xE9,0xAA,0xAA,0xAA,0xAA,0x90,0x90,0x90,0x90,0x90};

BYTE g_bloodShellcode[] = {
	0x3D,0xFC,0x1B,0xC3,0x03,				// CMP EAX, 3C31BFC
	0x74,0x0F,								// JE target:
	0xD9,0x44,0x24,0x54,					// FLD DWORD PTR SS:[ESP+54]
	0xD8,0x80,0xD8,0x01,0x00,0x00,			// FADD DWORD PTR DS:[EAX+1D8]
	0xE9,0xAA,0xAA,0xAA,0xAA,				// jmp orig
	0xC7,0x44,0x24,0x54,0x00,0x00,0x00,0x00,// target: MOV [ESP+54],0
	0xE9,0xAA,0xAA,0xAA,0xAA				// jmp orig
};
DWORD origOffset1 = 18;
DWORD origOffset2 = 31;

DWORD GetProcessIdByName(char *name)
{
	HANDLE snapshot;
	PROCESSENTRY32 processinfo;

	processinfo.dwSize = sizeof(processinfo);
	snapshot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, TH32CS_SNAPPROCESS);
	if(snapshot == INVALID_HANDLE_VALUE)
		return -1;
	
	if (!Process32First(snapshot, &processinfo))
	{
		CloseHandle(snapshot);
		return -1;
	}
	
	do
	{
		if (stricmp(name, processinfo.szExeFile)==0)
			return processinfo.th32ProcessID;
	}while (Process32Next(snapshot,&processinfo));
	
	return -1;
}

BYTE* GetMPBase(DWORD pid, char* moduleName)
{
	HANDLE hModuleSnap = INVALID_HANDLE_VALUE;
	MODULEENTRY32 me32;
	
	hModuleSnap = CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, pid);
	if (hModuleSnap == INVALID_HANDLE_VALUE)
		return NULL;
	
	me32.dwSize = sizeof( MODULEENTRY32 );
	if (!Module32First(hModuleSnap, &me32))
	{
		CloseHandle(hModuleSnap);
		return NULL;
	}
	do
	{
		if (stricmp(me32.szModule, moduleName) == 0)
			return me32.modBaseAddr;
	} while(Module32Next(hModuleSnap, &me32));
	
	CloseHandle(hModuleSnap);
	return NULL;
}

BOOL ChangeToUndead()
{
	HANDLE hProcess;
	PVOID addrToJmp = NULL;
	DWORD ret, offset;

	hProcess = OpenProcess(PROCESS_ALL_ACCESS, FALSE, g_csProcessId);
	if (hProcess == NULL)
	{
		printf("Get cstrike.exe handle error!\n");
		return FALSE;
	}

	addrToJmp = VirtualAllocEx(hProcess, 0, 50, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
	if (addrToJmp == NULL)
	{
		printf("VirtualAllocEx in cs.exe error!\n");
		CloseHandle(hProcess);
		return FALSE;
	}

	offset = (g_bloodAddr + 10) - 5 - ((DWORD)addrToJmp + origOffset1 -1);
	memcpy((g_bloodShellcode + origOffset1), &offset, 4);

	offset = (g_bloodAddr + 10) - 5 - ((DWORD)addrToJmp + origOffset2 -1);
	memcpy((g_bloodShellcode + origOffset2), &offset, 4);

	WriteProcessMemory(hProcess, addrToJmp, g_bloodShellcode, sizeof(g_bloodShellcode), &ret);
	if (ret != sizeof(g_bloodShellcode))
	{
		printf("WriteProcessMemory error!\n");
		VirtualFreeEx(hProcess, addrToJmp, 50, MEM_FREE);
		CloseHandle(hProcess);
		return FALSE;
	}

	offset = (DWORD)addrToJmp - 5 - g_bloodAddr;
	memcpy(g_patchedBlood + 1, &offset, 4);
	WriteProcessMemory(hProcess, (PVOID)g_bloodAddr, g_patchedBlood, 10, &ret);
	if (ret != 10)
	{
		printf("WriteProcessMemory error!\n");
		VirtualFreeEx(hProcess, addrToJmp, 50, MEM_FREE);
		CloseHandle(hProcess);
		return FALSE;
	}

	return TRUE;
}
int main()
{
	g_csProcessId = GetProcessIdByName("cstrike.exe");
	g_mpBase = (DWORD)GetMPBase(g_csProcessId, "mp.dll");
	g_bloodAddr = BLOOD_ADDR(g_mpBase);

	ChangeToUndead();

	printf("%d 0x%p\n", g_csProcessId, g_mpBase);

	// BackToOriginalDead(g_bloodAddr, g_originalBlood, g_patchedBlood);
	return 0;
}
```

编译运行之后csrike.exe进程就被patch了，这个时候只有自己是死不了的。我们的目标实现了。

问题及后续

有了这个代码，建立游戏就可以在局域网内狂虐小伙伴们了。但是如果你加入一个游戏，那就行不通了。也就是说我们之前找的都是服务器的代码段。客户端根本不会执行那些代码。现在以客户端的身份查找血值变化。只找到这么一个疑似代码段：

```
01948B80 - 8B 44 24 08           - mov eax,[esp+08]
01948B84 - 8B 4C 24 0C           - mov ecx,[esp+0C]
01948B88 - 50                    - push eax
01948B89 - 51                    - push ecx
01948B8A - E8 A1F30000           - call 01957F30
01948B8F - 83 C4 08              - add esp,08
01948B92 - E8 F9F30000           - call 01957F90
01948B97 - 8B 15 747CA101        - mov edx,[01A17C74] : [00000001]
01948B9D - 8B 0D 787CA101        - mov ecx,[01A17C78] : [00000052]
01948BA3 - 83 CA 01              - or edx,01
01948BA6 - 3B C1                 - cmp eax,ecx
01948BA8 - 89 15 747CA101        - mov [01A17C74],edx
01948BAE - 74 0F                 - je 01948BBF
01948BB0 - C7 05 847CA101 0000C842 - mov [01A17C84],42C80000
01948BBA - A3 787CA101           - mov [01A17C78],eax
01948BBF - B8 01000000           - mov eax,00000001
01948BC4 - C3                    - ret 
```

直接赋值操作。Eax存储的是变化后的血量。

当前需要解决的问题有：人物和武器如何对应、服务器和客户端的功能，限于篇幅，这些内容下次再写。另外，不要忘记前言提到的几大目标。我们的目的是通过逆向CS，学习游戏攻防思路技巧，并不是真的要去做CS或者其他外挂。勿忘初心。
