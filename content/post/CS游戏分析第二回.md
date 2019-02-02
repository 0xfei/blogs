---
title: "CS游戏分析第二回——解决上次遗留的数据问题"
date: 2015-01-09T09:13:30+08:00
draft: true
categories: ["调试逆向"]
tags: ["调试逆向"]
topics: ["调试逆向"]
---

### 武器

上回的一个遗留问题是人物和武器的结合。今天先来分析一下。首先根据上节课的找子弹部分，找到武器的对象地址。

这是子弹减少的代码，esi指向武器，偏移CC指向子弹。

```
ESI=0102CFC8

07956C3A - 48 - dec eax
07956C3B - 8B CE  - mov ecx,esi
07956C3D - 89 86 CC000000  - mov [esi+000000CC],eax <<
07956C43 - 8B 86 A4000000  - mov eax,[esi+000000A4]
07956C49 - 8B 40 04  - mov eax,[eax+04]
```

<!--more-->

我标记成蓝色的那两句比较有意思，根据ESI的值，我们知道现在`EAX = [0102D06C] = 009475F8`。然后 `eax = [eax + 4]`，这时`eax = [ 009475FC ] = 03C31BFC`。非常熟悉的值，就是当前枪的持有者。

换把枪试试。

```
ESI = 0120BE28
mp.weapon_aug+3FF - 89 86 CC000000        - mov [esi+000000CC],eax
mp.weapon_aug+405 - 8B 86 A4000000        - mov eax,[esi+000000A4]
// eax = 009475F8 跟上面的一样
mp.weapon_aug+40B - 8B 40 04              - mov eax,[eax+04]
// eax = 3C31BFC 同样是持枪者的首地址
mp.weapon_aug+40E - 8B 90 18010000        - mov edx,[eax+00000118]
mp.weapon_aug+414 - 83 CA 02              - or edx,02
mp.weapon_aug+417 - 89 90 18010000        - mov [eax+00000118],edx
mp.weapon_aug+41D - 8B 8E A4000000        - mov ecx,[esi+000000A4]
mp.weapon_aug+423 - E8 68FE0800           - call mp.player+3E50
```

009475F8显然是非常重要的一个值。现在我们先换一种思路，找如何从对象定位到所持武器。
如果说能从对象3C31BFC定位到枪esi，那么中间应该是一种指针的关系。比如person->something->something->value = weapon.。如果我们在内存中搜索esi，及值为枪的对象首地址的地址。

![pic1](/images/cs21.png "")

找到四个地方。现在我们换枪，看看它们分别是怎么变化的。按一下Q，如图变化：

![pic2](/images/cs22.png "")

再按一下G扔掉手枪：

![pic3](/images/cs23.png "")

后两个地址显然更值得关注。看一下它们怎么被修改，分别使用Q、G、捡枪、滚轮选择四种方式测试：

```
4	079E881C - 89 8E EC050000  - mov [esi+000005EC],ecx
3	079E866D - 89 BD EC050000  - mov [ebp+000005EC],edi
2	079ED048 - 89 BE EC050000  - mov [esi+000005EC],edi
1	079EA474 - 89 BD EC050000  - mov [ebp+000005EC],edi
```

我摁了四次Q，三次滚轮，两次G，一次捡枪。0827D0EC如上面代码所示被修改，意义不言而喻。同样的方法测试一下0827D0F0。

```
079EB343 - 89 86 F0050000  - mov [esi+000005F0],eax
```

只是一直在执行这一句，可见0827D0EC才是关键。我们仔细分析一下上面四种情况，代码贴出来。

换武器Q：

```
EAX=0102CFC8
ECX=010BBEF0
ESI=00960F78
mp.CBasePlayer::PlayerDeathThink+42C0 - 8B 86 EC050000        - mov eax,[esi+000005EC]
mp.CBasePlayer::PlayerDeathThink+42C6 - 8B 8E F4050000        - mov ecx,[esi+000005F4]
mp.CBasePlayer::PlayerDeathThink+42CC - 89 8E EC050000        - mov [esi+000005EC],ecx
mp.CBasePlayer::PlayerDeathThink+42D2 - 89 86 F4050000        - mov [esi+000005F4],eax
mp.CBasePlayer::PlayerDeathThink+42D8 - 8B 11                 - mov edx,[ecx]
mp.CBasePlayer::PlayerDeathThink+42DA - FF 92 00010000        - call dword ptr [edx+00000100]
```

明显的交换操作，我们也知道了5EC和5F4偏移是首选的两个武器包。5EC是当前武器。

选武器：

```
ESI = 
mp.CBasePlayer::PlayerDeathThink+4111 - FF 90 0C010000        - call dword ptr [eax+0000010C]
// 这个call非常有意思，call之后ecx = 持有的武器，edx = 人物对象首地址
mp.CBasePlayer::PlayerDeathThink+4117 - 8B 8D EC050000        - mov ecx,[ebp+000005EC]
// ecx = 原来的武器
mp.CBasePlayer::PlayerDeathThink+411D - 89 BD EC050000        - mov [ebp+000005EC],edi
// edi = 新选择的武器
mp.CBasePlayer::PlayerDeathThink+4123 - 85 FF                 - test edi,edi
mp.CBasePlayer::PlayerDeathThink+4125 - 89 8D F4050000        - mov [ebp+000005F4],ecx
// 原来的武器放回第二个选择包里
```

扔武器：

```
ESI = 0098FFA8
mp.CBasePlayer::PlayerDeathThink+8AEC - FF 90 0C010000        - call dword ptr [eax+0000010C]
// call 之后 edx = 人物对象首地址
mp.CBasePlayer::PlayerDeathThink+8AF2 - 8B 8E EC050000        - mov ecx,[esi+000005EC]
// ecx = 现在的武器
mp.CBasePlayer::PlayerDeathThink+8AF8 - 89 BE EC050000        - mov [esi+000005EC],edi
// 现在的武器换成第二个武器
mp.CBasePlayer::PlayerDeathThink+8AFE - 89 8E F4050000        - mov [esi+000005F4],ecx
// 把要扔掉的武器放到第二个背包，这里注意，和我们理解的扔有一定偏差。
mp.CBasePlayer::PlayerDeathThink+8B04 - 8B 17                 - mov edx,[edi]
mp.CBasePlayer::PlayerDeathThink+8B06 - 8B CF                 - mov ecx,edi
mp.CBasePlayer::PlayerDeathThink+8B08 - FF 92 00010000        - call dword ptr [edx+00000100]
```

捡武器：

```
EBP = 0098FFA8
mp.CBasePlayer::PlayerDeathThink+5F18 - FF 90 0C010000        - call dword ptr [eax+0000010C]
mp.CBasePlayer::PlayerDeathThink+5F1E - 8B 8D EC050000        - mov ecx,[ebp+000005EC]
mp.CBasePlayer::PlayerDeathThink+5F24 - 89 BD EC050000        - mov [ebp+000005EC],edi
mp.CBasePlayer::PlayerDeathThink+5F2A - 89 8D F4050000        - mov [ebp+000005F4],ecx
mp.CBasePlayer::PlayerDeathThink+5F30 - 8B 17                 - mov edx,[edi]
mp.CBasePlayer::PlayerDeathThink+5F32 - 8B CF                 - mov ecx,edi
mp.CBasePlayer::PlayerDeathThink+5F34 - FF 92 00010000        - call dword ptr [edx+00000100]
mp.CBasePlayer::PlayerDeathThink+5F3A - 8B 8F A4000000        - mov ecx,[edi+000000A4]
```

如果不是因为重启，上面存放武器的基址应该是一样的。我们就以0098FFA8为准。[0098FFA8+5EC]为当前武器。现在搜索一下0098FFA8。搜索结果出人意料，找到了 03C31BF8。而人物对象首地址我们一直知道是3C31BFC。所以现在可以定位武器了。

![pic4](/images/cs24.png "")

![pic5](/images/cs25.png "")

现在改善上次的小程序，做到不仅无敌，而且子弹无数。不过手榴弹烟雾弹又是另一回事，下面烟雾弹举例：

```
0899584D - 8B 10  - mov edx,[eax]
0899584F - 4A - dec edx
08995850 - 89 10  - mov [eax],edx <<
08995852 - 8B 96 C4000000  - mov edx,[esi+000000C4]
08995858 - 8B 86 A4000000  - mov eax,[esi+000000A4]

EAX=0932141C
EBX=00000020
ECX=09320DF8
EDX=00000001
ESI=00A05008
EDI=08A86EE0
ESP=0012F39C
EBP=00000000
EIP=08995852
```

不妨猜测手雷和枪放的地址相邻，先看一下`[人物首地址-4]`的值，发现跟EAX比较接近，不妨猜测一个偏移，然后重启游戏再看。事实证明这个猜测是对的：

![pic6](/images/cs26.png "")

偏移0x624处是闪光弹数目。

我们的目的是学习游戏分析，并不是写一款商业挂。所以类似的手雷部分数据就不再分析了，分许过程是一样的。

### 人物关系

上一篇的代码假设“自己”的首地址是3C31BFC，这在我的XP上是没有问题的，因为没有地址随机化。不过硬编码有两个问题：首先是平台改变之后程序铁定失效；还有就是只能造福自己不能惠及队友。所以我们得解决人物关系问题，找出游戏是如何区分警匪的。

基本做法还是CE数据查找。首先看一下值为3C31BFC的地址，找到两个：

![pic7](/images/cs27.png "")

我的做法是接着类似的方法找其它player的首地址所在内存，然后尽量把他们关联起来。找了几组之后发现player是以链表形式链接起来的，类似LIST_ENTRY：

![pic8](/images/cs28.png "")

```
[8E52EF8] = 3C31BFC
[[8E52EF8+4]] = 3C31F20
[[[8E52EF8+4]+4]] = 3C32568
```

结构类似这样：

```
Struct PlayerList
{
PlayerClass player;
PVOID next;
}
```

但是只知道这个价值并不大，因为存放链表的地址每次都会变化，我们需要找到某个全局地址。我还是找到几个疑似地址的，但是每次重启之后都会变化。看的眼睛都要烧了的时候我突然发现我忽略了最重要的一点：

![pic9](/images/cs29.png "")

妈的这个地址是mp.dll的一个偏移。我说咋多次查找数据都非常类似，偏移为1B2EF8；不过有时候偏移是1C2EF8。可以写个小程序枚举人物，同时列出他们的坐标。为日后自己写程序和BOT干架打下良好基础。

![pica](/images/cs2a.png "")

核心代码如下：

```
#define PLAYER_OFFSET		0x1B2EF8
#define WEAPON_OFFSET		0x5EC
#define BULLET_OFFSET		0xCC

void DisplayPlayers()
{
	typedef struct __Player
	{
		DWORD player;
		DWORD next;
	} Player, *PPlayer;

	typedef struct __Point
	{
		float x;
		float y;
	} Point, *PPoint;


	int number=0;
	DWORD ret, bullet, value=0;
	Point xy;
	float blood;
	Player players[30];
	HANDLE process = OpenProcess(PROCESS_ALL_ACCESS, FALSE, g_csProcessId);

	if (!ReadProcessMemory(process, (LPCVOID)(g_mpBase+PLAYER_OFFSET), (LPVOID)&players[number], sizeof(Player), &ret))
	{
		printf("ReadProcessMemoryError!\n");
		CloseHandle(process);
		return;
	}
	if (players[0].player == 0)
	{
		ReadProcessMemory(process, (LPCVOID)(g_mpBase+PLAYER_OFFSET+0x10000), (LPVOID)&players[number], sizeof(Player), &ret);
	}
	while (players[number].next != 0)
	{
		if (!ReadProcessMemory(process, (LPCVOID)players[number].next, (LPVOID)&players[number+1], sizeof(Player), &ret))
		{
			number++;
			printf("ReadProcessMemoryError!\n");
			break;
		}
		number++;
	}
	while (1)
	{
		for (int i=0; i<=number; i++)
		{
			__try 
			{
				ReadProcessMemory(process, (LPCVOID)(players[i].player+8), (LPVOID)&xy, sizeof(Point), &ret);
				printf("Player%d at (%f,%f), ", i, xy.x, xy.y);
				ReadProcessMemory(process, (LPCVOID)(players[i].player+0x160), (LPVOID)&blood, sizeof(blood), &ret);
				printf("blood %f, ", blood);
				ReadProcessMemory(process, (LPCVOID)(players[i].player-4), (LPVOID)&value, sizeof(value), &ret);
				ReadProcessMemory(process, (LPCVOID)(value+WEAPON_OFFSET), (LPVOID)&value, sizeof(value), &ret);
				ReadProcessMemory(process, (LPCVOID)(value+BULLET_OFFSET), (LPVOID)&bullet, sizeof(bullet), &ret);
				printf("left %d bullets\n", bullet);
				printf("---------------------------------------------\n");
			}
			__except(EXCEPTION_EXECUTE_HANDLER)
			{
			}
		}
		printf("++++++++++++++++++++++++++++++++++++++++++++++++++\n");
		Sleep(2000);
	}
	CloseHandle(process);
}
```

所有player是在一个链表上，而且可以通过mp.dll基址找到“我”，解决了平台问题。

那么有没有可能警和匪分别还位于另两个链表呢？不然该如何区分？在对象的某个地方设置一个bool？这些都是可以得，不过从方便管理的角度来说，我觉得会是再弄两个链表。

类似找上一个链表的过程，找了好久并没有发现类似的结构。不同的角色——警和匪，想找到组织起来的链表非常麻烦。所以还是回归找标志的思路。我们假设对象地址3C31BFC某处有这个标志。

但是该怎么搜索数据呢？直接查找？谁知道警和匪是什么标志，是0还是1，1还是2，很难说。而通过数据变化搜索更是不可能，就算每次没有地址随机化，所有地址都跟上一次一样（这个其实也算一个新课题，给分析带来不少便利），光是数据量就足以让虚拟机挂掉，搜索数据是不可能了。回想上一篇文章我们意外找到的疑似初始化代码：

```
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
```

这个偏移是973ee，用OD定位到这里，往上翻看到函数的开头：

```
06367350                         83EC 1C                      SUB ESP,1C
06367353                         53                           PUSH EBX
06367354                         56                           PUSH ESI
06367355                         8BF1                         MOV ESI,ECX
06367357                         33DB                         XOR EBX,EBX
06367359                         8BCB                         MOV ECX,EBX
0636735B                         8BD3                         MOV EDX,EBX
0636735D                         8D86 F0070000                LEA EAX,DWORD PTR DS:[ESI+7F0]
06367363                         57                           PUSH EDI
06367364                         8B7E 04                      MOV EDI,DWORD PTR DS:[ESI+4]
06367367                         899E E4070000                MOV DWORD PTR DS:[ESI+7E4],EBX
0636736D                         8908                         MOV DWORD PTR DS:[EAX],ECX
0636736F                         899E E8070000                MOV DWORD PTR DS:[ESI+7E8],EBX
06367375                         899E EC070000                MOV DWORD PTR DS:[ESI+7EC],EBX
0636737B                         899E 04080000                MOV DWORD PTR DS:[ESI+804],EBX
06367381                         8950 04                      MOV DWORD PTR DS:[EAX+4],EDX
```

寄存器：

```
EAX 063C9450 mp.063C9450
ECX 00955FF0
EDX 03C31BFC
EBX 00000000
ESP 0012FAC4
EBP 06498078
ESI 00955FF0
EDI 00955FF0
```

稍微分析代码就知道这个函数是对ECX这个类进行一系列赋值，该类偏移4的地方就是3c31bfc；最开始我投入过多精力分析这个函数的每一句话，而实际上如果连数据的基本含义都没搞懂的话，盲目分析语句是没有意义的。
搜索一下00955FF0，结果让我大吃一惊：

![picb](/images/cs2b.png "")

还记得之前枪和人物关系对应时的那个 3C31BFC-4 嘛？这里又出现了，它的值就是这个初始化函数里的ECX。我们把目前接触到的东西都串联起来了。这个时候该怎么分析角色呢？我的方法是每次拦截ECX，然后比较它们在内存中的不同：

![picc](/images/cs2c.png "")

连续加入不同角色的player，包括bot和真人，反复比较那片地址区域的值的不同，注意数值比较小的地址。这个过程实际上是非常枯燥的，甚至会感到绝望。

反复确认之后我发现 偏移 0x1C8只可能是 2 和 1，当1时是匪，2是警。再加入不同的角色，再进行一些验证，基本可以确定这就是区别警和匪的地方。另外这个类的虚表可以用来区分是BOT还是真人。偏移0x34处也非常可疑，有待分析，不过我们的目的已经达到了。

### 小总结

这两篇对CS的分析其实主要集中在数据查找上。对函数功能、程序的逻辑并没有进行过多分析，也没有涉及网络部分。分析过程中我们可以看到，没有对数据的充分了解，即数据的作用、大致结构、继承关系等，就不可能去分析某个函数，更不用说想了解游戏框架。

数据查找是分析游戏的第一步，可以说是最简单的一步，但是工作量几乎是最大的。首先CE查找，猜测程序对数据的组织形式、查看内存并验证猜测，注意多级指针的查找。直到最后把所有有价值的数据连接起来，并能准确定位到。

比如我们这里，通过mp.dll的偏移，就可以找到所有players，通过players对象可以定位到武器、角色属性、血量、位置坐标等基本信息。这就是数据查找的目的。
