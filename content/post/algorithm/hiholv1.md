+++
date = "2016-09-07T23:46:45+08:00"
draft = true
description = "hiho level1 codes."
categories = ["算法题目"]
tags = ["C/C++", "算法题目"]
topics = ["C/C++", "算法题目"]
title = "hiho难度1题目题解"
+++

hiho是个很不错的oj网站。定位应该是找工作的应届生。看起来很新颖，但实际上还是传统oj，只不过出题的由学校变成企业，讨论也很少，感觉几乎没什么人。它最吸引我的就是题目比较好，不像leetcode那样烂大街（leetcode的题目我并不怎么喜欢），很有acm的感觉；题目数量不多，但分类明确，有挑战有知识含量，省得我这种不是专门刷题的工作党到处找题做。缺点就是优点换个说法：太acm，不像leetcode那样专门为了企业招人；题目偏难，分类太过明确。

最近为了提高自己的修养，不烂在一个地方，重新开始刷oj。poj刷了一些，题目参差不齐，有点浪费时间；hiho就好多了。先把level1的都做完。这些题目难度不大，对我来说基本读完题就能有思路；但同时陷阱也不少，自己又好多年没写过这种模式的代码，不免好多时间都在找错误。虽然没专门做acm，但自己这几年写了不少代码，工程上考虑更多，也有些基础，所以做起来还算得心应手。这里先总结一下最简单的lv1。

<!--more-->

所有代码都可以在[这里](https://github.com/0xfei/hihocodes)找到。下面是每道题目的简单思路。

#### 1039 字符消除

非常纯粹的一道模拟题。做的时候没有仔细看，以为插入字符之后可以自由选择消除顺序。那样的话会是一个很复杂的问题。
写了一会儿我开始怀疑这是个NP问题——只能搜索。第二天才发现原来过程是贪心的，有满足条件的就消除，然后再迭代。这样的话它就变成了一道很简单的题目。但是我还犯了一个想当然的错误，觉得添加一个相同的字符一定比其它的要优秀，当然自己也很快
想出了反例，比如AAABA -> ABAABA是最优解。

#### 1051 补提交卡

很直接的思路——一定要按顺序填补。


#### 1082 然而沼跃鱼早就看穿了一切

简单的字符串处理。


#### 1094 Lost in the City

有的时候，最麻烦的处理反而最简单。所以我直接手写了八种匹配情况，看起来也不乱，跟着转就行。

```
bool check(int x, int y)
{
        if (a[x][y]==b[0][0] && a[x][y+1]==b[0][1] && a[x][y+2]==b[0][2] && 
        	 a[x+1][y]==b[1][0] && a[x+1][y+1]==b[1][1] && a[x+1][y+2]==b[1][2] &&
                a[x+2][y]==b[2][0] && a[x+2][y+1]==b[2][1] && a[x+2][y+2]==b[2][2])
                return true;
        if (a[x][y]==b[2][0] && a[x][y+1]==b[1][0] && a[x][y+2]==b[0][0] &&
                a[x+1][y]==b[2][1] && a[x+1][y+1]==b[1][1] && a[x+1][y+2]==b[0][1] &&
                a[x+2][y]==b[2][2] && a[x+2][y+1]==b[1][2] && a[x+2][y+2]==b[0][2])
                return true;
        if (a[x][y]==b[2][2] && a[x][y+1]==b[2][1] && a[x][y+2]==b[2][0] &&
                a[x+1][y]==b[1][2] && a[x+1][y+1]==b[1][1] && a[x+1][y+2]==b[1][0] &&
                a[x+2][y]==b[0][2] && a[x+2][y+1]==b[0][1] && a[x+2][y+2]==b[0][0])
                return true;
        if (a[x][y]==b[0][2] && a[x][y+1]==b[1][2] && a[x][y+2]==b[2][2] &&
                a[x+1][y]==b[0][1] && a[x+1][y+1]==b[1][1] && a[x+1][y+2]==b[2][1] &&
                a[x+2][y]==b[0][0] && a[x+2][y+1]==b[1][0] && a[x+2][y+2]==b[2][0])
                return true;
        return false;
}
```

#### 1120 小Hi小Ho的惊天大作战：扫雷·三

这是第一道让我感到有些头晕的题目。估计是那天在家头脑不清醒。我总是找不到最好的方法来判断什么样的情况满足局面，怎么样把一个位置标记为有雷。第二天想明白了，先填充好，然后枚举一遍判断就行。数据量很小，直接搞一个10位bits。

#### 1121 二分图一?二分图判定

二分图就很常用了，判断方法也很简单。BFS扫描一遍染色就可以。

#### 1135 Magic Box

似乎是微软校招题。非常简单，我的做法是先把三个数排序，这样就只有三种差值情况了。

#### 1137 Recruitment

这道题目让我感到很自豪，终于觉得自己就算在算法思路上，也是比高中强不少。很明显我们能想到背包问题，而M和F的设定就是扰乱视线，和最终的实现没什么关系，只要分开处理，然后合并答案就可以。也是个二维背包，写的时候还想了好久怎么处理顺序。最后也算自己想了清楚。

```
void packet(int n, int m, int x, Value &f, Person &a)
{
    f[0][0].value = 0;
	for (int now=1; now<=n; ++now) {
		for (int i=now; i>=1; --i){
			int temp;
			for (int j=m; j>=a[now].price; --j) {
				temp = f[j-a[now].price][i-1].value + a[now].value;
				if (temp >= 0 && temp > f[j][i].value) {
					f[j][i].value = temp;
					f[j][i].used = f[j-a[now].price][i-1].used;
					f[j][i].used.set(a[now].index);
				}
			}
		}
	}
}

// find answer
bitset<101> used;
for (int i=0; i<=B; ++i) {
	for (int j=0; j<=B-i; ++j) {
		if (f[i][X].value + m[j][Y].value > Ans) {
			Ans = f[i][X].value + m[j][Y].value;
			Cost = i + j;
			used = f[i][X].used | m[j][Y].used;
		}
	}
}
```

#### 1138 Islands Travel

很明显的dijkstra+heap优化，当时偷了个懒，顺便回忆了迭代的spfa，没想到spfa这种奇葩算法也能通过数据。话说回来，hiho的数据并不算苛刻。

#### 1143 骨牌覆盖问题·一

我能说什么呢，以前最爱给别人讲的就是幂运算+简单矩阵乘来算斐波那契数列。

```
#include <iostream>

using namespace std;

int main()
{
        int n;
        const int m = 19999997;
        long long a=0, b=1, c=1, d=1;
        long long x=1, y=0, z=0, q=1;
        long long t1, t2, t3, t4;
        cin >> n;
        while (n) {
                if (n%2) {
                        t1 = (a*x%m + c*y%m)%m;
                        t2 = (b*x%m + d*y%m)%m; 
                        t3 = (a*z%m + c*q%m)%m;
                        t4 = (b*z%m + d*q%m)%m;
                        x = t1, y = t2, z = t3, q = t4;
                }
                t1 = (a*a%m + b*c%m)%m;
                t2 = (a*b%m + b*d%m)%m;
                t3 = (a*c%m + c*d%m)%m;
                t4 = (b*c%m + d*d%m)%m;
                a = t1, b = t2, c = t3, d = t4;
                n /= 2;
        }
        cout << q << endl;
        return 0;
}
```

#### 1148 2月29日

这道题让我感到挫败。因为我居然不太清楚闰年为什么是闰年，或者说我没有好好想一想。闰年显然是为了弥补历法和实际公转的某个差值，所以得过些年加一天补充一次，不然四季就会错乱，所以规定整100年时必须被400整除才算闰年。而我印象中却是过4年就是闰年。人类的认知，人类的寿命，跟宇宙比起来，
实在太渺小啊。

此外这里有个小技巧，(年份/4-年份/100+年份/400)是耶稣诞生以来的闰年数。我在代码里注释了N个自己犯的小错误，惭愧的很。

#### 1152 Lucky Substrings

这个题目简单到离谱，然而不审题的后果就是错了也不知道为什么。我到现在还是想当然……

#### 1175 拓扑排序·二

拓扑排序很有意思，我以前就对它的效率表示怀疑。当然，每个点最多进一次队列，所以确实是O(M)的。我以为value为0的边可以不用加入队列，实际上这是个大错误，入度存在，但是边没加进去，拓扑就不会成功。

#### 1186 Coordinates

枚举就可以。注意for循环的上限，是x而不是x-1。

#### 1197 Give My Text Back

这个题很有意思。这种题目的通过率往往很低，大家都容易少考虑很多情况。我还算比较有心得，只要一步一步考虑，思考当前位置应该发生什么，还是比较简单的。

#### 1223 不等式

很容易想到枚举可能的数字，然后枚举等式，计数。但是数据可能是小数，所以很坑。我的做法是乘10，然后当做再枚举。每次做这种题我都觉得好多所谓“算法”和“思路”比赛，其实只是些小trick。做了当然有进步，但是进步真有多大呢？

#### 1268 九宫

这个题和之前的1094有点像。我也是采用类似的方法，先把所有可能算出来。不过这次比较惨，上下颠倒的时候写错了一个数。
```
string start = "04923578160";

string turn_right(const string &s)
{
        string t = s;
        t[1] = s[7], t[2] = s[4], t[3] = s[1];
        t[4] = s[8], t[5] = s[5], t[6] = s[2];
        //                        t[7] = s[3] fuck!!!!
        t[7] = s[9], t[8] = s[6], t[9] = s[3];
        return t;
}

string left_to_right(const string &s)
{
        string t = s;
        for (int i=1; i<=3; ++i) {
                t[i] = s[i+6];
        }
        for (int i=7; i<=9; ++i) {
                t[i] = s[i-6];
        }
        return t;
}

string up_to_down(const string &s)
{
        string t = s;
        t[1] = s[3], t[3] = s[1];
        t[4] = s[6], t[6] = s[4];
        t[7] = s[9], t[9] = s[7];
        return t;
}
```
这么简单的手写都容易出错，所以把事情分成小块交给程序，保证程序的没一小部分都正确是很有必要的。

#### 1272 买零食

因为最多可以买三样零食，所以枚举三层就可以。注意在第一层和第二层也需要计算，因为不一定非卖三样。同样，我也把数据乘10，这样方便计算是否被5整除。

#### 1283 hiho密码

这就是最经典的二分了。但是要让所有人一下就想到二分并且写对，估计也不是那么容易。以前NOIP就因为二分写错，到现在不知道为什么。
这次换了种写法。

```
    int l = 0, r = n, mid = l+(n-l)/2;
	while (l < r) {
		if (check(mid)) {
			r = mid;
		} else {
			l = mid + 1;
		}
		mid = l+(r-l)/2;
	}
```

其实保证二分正确有一些基本原则：所有数据尤其是边界能被覆盖、不会死循环和走出二分之后能拿到正确的值。比如我这种写法，r一定是正确的值，而每一步r或l会有变化，同时l<r的设定保证一定能走出来。

#### 1288 Font Size

这道题也可以用二分的思想解决——我们假定某个答案是X，然后判断它对不对。这得保证答案的有序性，比如x满足条件，那么x+1或者x-1一定满足。这样才可以二分。这道题就符合条件，最小的肯定满足，我们要找的是满足条件的最大的。所以二分可行。

```
    int r = min(w, h)+1, l = 1;
	while (l < r) {
		int mid = l+(r-l)/2;
		if (!check(mid)) {
			r = mid;
		} else {
			l = mid + 1;
		}
	}
```

可以注意到我这里做了少许变化，因为答案的有序性和上一题正好相反。可以想到，l是不满足条件的最小值，所以l-1是满足条件的最大值。

#### 1296 数论三·约瑟夫问题

这个问题非常巧妙。以至于我不得不单独写一篇文章来说一下。

#### 1297 数论四·扩展欧几里德

同样，扩展欧几里得的推导也很有意思。我也想单独写一篇文章来说这个问题，包括中国剩余定理。

#### 1304 搜索一·24点

题目解释里给了不少提示：增加两种运算 反- 和 反/，就可以化成(((a b ) c) d ) 和 ((a b) (c d))这两种情况。只要枚举数字，再枚举运算符就可以。
这里我犯了个大错，枚举出三个运算符之后，计算情况一，当我发现被除数为0或者情况不满足时会continue，这样跳过了第二种情况的计算。
这个错误隐藏很深，发现之后不得不写个goto来简单处理。

#### 1308 搜索二·骑士问题

这个题我用了状态压缩的思路来存储状态，把它们按8进制压缩。之后再进行BFS遍历。状态转移和判断条件都写得比较清晰，也注意了很多问题。也在写的过程中简化了不少代码。

```
inline bool valid(int x, int y)
{
    return (x >= 0 && x < 8 && y >=0 && y < 8);
}

inline bool ok(int status)
{
	return ((status & 077) == ((status >> 6) & 077)) && ((status & 077) == (status >> 12));
}

int find_ans(int start)
{
	queue<int> q;
	map<int, int> path;

	q.push(start);
	path.insert(make_pair(start, 0));

	while (!q.empty()) {
		int now = q.front();
		q.pop();
		int a[] = {now >> 12, (now >> 6) & 077, now & 077};
		int d = path[now];

		if (ok(now)) {
			return d;
		}

		for (int i=0; i<3; ++i) {
			for (int j=0; j<8; ++j) {
				int orig = a[i];
				int x = (orig >> 3) + mov[j][0], y = (orig & 07) + mov[j][1];
				a[i] = x*8 + y;
				int next = (a[0] << 12) + (a[1] << 6) + a[2];
				if (valid(x, y) && path.find(next) == path.end()) {
					if (ok(next)) {
						return d + 1;
					}
					q.push(next);
					path.insert(make_pair(next, d+1));
				}
				a[i] = orig;
			}
		}
	}
	return 1;
}
```

#### 1321 搜索五·数独

数独和八皇后算是搜索和回溯的经典问题了。关键是判重，我这里定义了一个三维的bool数组，v[3][N][N]，用来记录第i行的数字k、第i列的数字k和第i个小方块的数字k已经被占用。这样在搜索到i\j这个坐标时，可以尽可能少的知道该枚举哪个值。

```
bool find_ans(int x, int y, int (&a)[n][n], bool (&v)[3][N][N])
{
    if (x == n) {
		return true;
	}
	if (a[x][y] > 0) {
		if (y == n-1) {
			return find_ans(x + 1, 0, a, v);
		} else {
			return find_ans(x, y + 1, a, v);
		}
	}

	for (int i=1; i<=9; ++i) {
		if (!v[0][x][i] && !v[1][y][i] && !v[2][get_block(x,y)][i]) {
			a[x][y] = i;
			v[0][x][i] = true;
			v[1][y][i] = true;
			v[2][get_block(x,y)][i] = true;
			bool result = false;
			if (y == n-1) {
				result = find_ans(x + 1, 0, a, v);
			} else {
				result = find_ans(x, y + 1, a, v);
			}
			if (result) {
				return true;
			}
			a[x][y] = 0;
			v[0][x][i] = false;
			v[1][y][i] = false;
			v[2][get_block(x,y)][i] = false;
		}
	}
	return false;
}
```

此外还要注意一些边界问题。

#### 1325 平衡树·Treap 1329 平衡树·Splay 1333 平衡树·Splay2 1337 平衡树·SBT

这四个题目可以算是平衡树的练习题。我也很久没写过了，所以把一些操作杂糅在了一起。基本操作还是splay，它也能应对这四道题目的所有数据。代码真的很丑。

#### 1350 Binary Watch

这个“二进制表”很简单，枚举+统计二进制位的1个数。自己写或者用bit都可以。

#### 1361 Playfair密码表

也是很单纯的模拟题。注意点也很少。

#### 1365 图片排版

这道题是最后做的。虽然lv1评级，但是我觉得真需要一些思考。首先观察数据，虽然矩阵很多，但是宽度有限，最大才100。而只能去掉一个矩阵，因此顺序几乎是固定的。

我们这样想，如果去掉的是第i个矩阵，前i-1的排列方式已经知道，只要把后i+1个矩阵挨着前i-1放置就可以。大致定义解决问题的方程。
F[I][J]表示把第I个矩阵放到坐标J，J最大为M，M最大100，因此空间可以接受。但是从1~n计算有些困难，我当时就在这里卡了一会儿，这样的话情况会有太多，尤其是涉及到最有一个需要压缩。突然想到，如果倒着来，一切就又是已知的了。剩下的就是编码问题。

编码难度其实也不低。这里给出我做预处理的代码，完整代码github上也有。

```
    inline int cut(int left, int k, int &high)
    {
        if (left < k) {
		    high = static_cast<int>(ceil(static_cast<double>(left*high)/k));
		    return 0;
	    } else  {
		    return left - k;
	    }
    }

    for (int i=1; i<=n; ++i) {
		int high;
		cin >> a[i] >> b[i];
		f[i] = f[i-1];
		high = b[i];
		f[i].left = cut(f[i].left, a[i], high);
		if (high > f[i].highest) {
			f[i].highest = high;
		}
		if (f[i].left == 0) {
			f[i].left = m;
			f[i].high += f[i].highest;
			f[i].highest = 0;
		}
	}

	for (int i=1; i<=m; ++i) {
		int high = b[n];
		if (a[n]+i-1 >= m) {
			high = static_cast<int>(ceil(static_cast<double>((m-i+1)*high)/a[n]));
		}
		g[n][i].highest = high;
		if (i == 1) {
			g[n][i].high = high;
			g[n][i].highest = 0;
		}
	}

	for (int i=n-1; i>0; --i) {
		for (int j=m; j>=1; --j) {
			int high = b[i];
			if (a[i] + j - 1>= m) {
				// fuck! j is not i !!!!!!
				high = static_cast<int>(ceil(static_cast<double>((m-j+1)*high)/a[i]));
				g[i][j].highest = high;
				g[i][j].high = g[i+1][1].high;
			} else {
				g[i][j] = g[i+1][a[i]+j];
				if (high > g[i][j].highest) {
					g[i][j].highest = high;
				}
			}
			if (j == 1) {
				g[i][j].high += g[i][j].highest;
				g[i][j].highest = 0;
			}
		}
	}
```
