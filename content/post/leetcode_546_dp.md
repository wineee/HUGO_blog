---
title: 记录一个有趣的dp题目-leetcode 546 移除盒子
date: 2020-08-15
math: true
---
## 题面
> 给出一些不同颜色的盒子，盒子的颜色由数字表示，即不同的数字表示不同的颜色。

> 你将经过若干轮操作去去掉盒子，直到所有的盒子都去掉为止。每一轮你可以移除具有相同颜色的连续 k 个盒子（k >= 1），这样一轮之后你将得到 k * k 个积分。
当你将所有盒子都去掉之后，求你能获得的最大积分和。

来源：力扣（LeetCode）

链接：https://leetcode-cn.com/problems/remove-boxes
著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。
<!--more-->
## 解题思路

**用f[l][r]来表示移除区间 [l,r] 内所有的盒子能得到的最大积分，然后去探索某一种移除盒子的策略来进行状态转移**,这个方法应该是大部分人的第一个想法,官方题解称之错误的思路,但实际上,它是可以做出来的,只是状态转移方程比较特殊.

首先枚举的是[i,j]区间,对于确定的区间,如何确定f[i][j]呢,我是枚举最后一次消除的是哪个数(以后用numx代替),比如样例[1,3,2,2,2,3,4,3,1],

i=0,j=2,[1,3]最后一个可以是1或3

i=0,j=8,[1,3,2,2,2,3,4,3,1],最后一个可以是1,3,2或者4

我们知道最后的消除的是谁了,那么,一个朴素的想法,$(a+b)^2>a^2+b^2$,把所有的合一起消掉肯定比分两波甚至更多波好,那么状态转移方程就是

 $ f[i][j] = \sum_{(x,y)}f[x][y]+cntx^2$

其中(x,y)是被numx分割的子序列,cntx是numx的个数

聪明的你可能发现了有点不对劲,没错,当我信心满满的交上时,结果WA了

我又仔细想了想,发现加入我们枚举7的时候,有一个子段XXOO,2,2,2,7,2,2,2,XXOO,我们为了7最优,却忽略了被7分开的2,我们放弃这个7去成就2全局可能更好.情况可能更复杂XXOO,2,2,2,7,3,7,4,2,2,2,XXOO,这次要放弃7,3,7,4成就2,也就是说只看7两边一样的数也不行,要看全局!

看全局,怎么看?枚举7的时候有m个7分布在数列不同位置,如果一个个枚举7选不选(在最后一波一起消除),$2^m$太爆炸了,但是,仔细想想,这不是动态规划问题吗(没错,用动态规划维护上面的动态规划的状态转移,套娃警告)


- $g[cnt][num]$表示前cnt个numx,一共num个不参与最后一轮(且第cnt个必定不参与)消除的情况下的最优解
- 初值$g[0][0]=0$
- 状态转移方程: $g[cnt][num] = \max_{0<=per<cnt}(g[per][num-1]+f[posx[per]+1][posx[cnt]-1]))$,其中posx[i]表示第i个numx下标

最后,终于可以更新 $f$ 了

 $f[i][j] = max(f[i][j], g[cntx][num]+num*num)$ 
 (1<=num<=cntx)

 $f[i][j] = max(f[i][j], g[cnt][num]+num*num+f[posx[cnt]+1][j])$
 (1<cnt<cntx,1<=num<=cnt别忘了考虑最后一个numx不参与的情况)

最后:

- 递推和记忆化搜索本质是相同的,递推写法注意,第一层必须枚举区间长度,先算小区间再算大区间,不要枚举起点终点

- $g[cnt][num]$为什么规定第cnt个必定不参与,感觉是经典问题了,为了方便状态转移,必须知道最后一个不参与的是谁,用$g[cnt][num][last]$表示前cnt个numx,一共num个不参与,位置最靠后的不参与的是last的情况下的最优解,优化一下的产物

- 类似11111211枚举1时显然要把1一次性合并,没必要dp了,我稍微判断了一下$other * other-other <= cntx * cntx-(cntx-1)*(cntx-1))$时,没必要继续dp,算是常数优化吧,当然,可以更精细

- 我看了官方题解,状态定义非常新颖,转移决策更自然,f(l,r,k)真的很难想到,非常厉害,我的方法虽然慢,但是说明f[i][j]的做法是可行的,还是有点价值的

``` c
inline int max(int a,int b){return a>b?a:b;}
int removeBoxes(int* boxes, int boxesSize){
    int f[boxesSize+2][boxesSize+2];//f[i][j]表示i到j区间最优解
    int g[boxesSize+2][boxesSize+2],posx[boxesSize+2]; 
    int flags[boxesSize+2];
    for (int i = 0; i <= boxesSize; i++)
      for (int j = 0; j <= boxesSize; j++)
        f[i][j] = 0;
    for (int i = 0; i < boxesSize; i++) {
        flags[i] = 0;
        f[i][i] = 1;
    }
    for (int len = 2; len <= boxesSize; len++) //枚举区间长度
      for (int i = 0; i+len-1 < boxesSize; i++) { //枚举起点
          int j = i+len-1;                        //区间i到j
          int flag = i*10000+j;                   //一个特殊标记flag判重,防止枚举重复数字
          for (int x = i; x <= j; x++)            //枚举这个区间最后消除的数字numx
           if (flags[x] != flag) { 
              flags[x] = flag;
              int tmp = x?f[i][x-1]:0;            //tmp记录numx分割开的子区间最优解的和  
              int cntx = 1,lastx = x;             //cntx为numx的数量,lastx是邻近的前一个numx的下标
              posx[0] = i-1;                      //posx[i]记录第i个numx下标
              posx[1] = x;
              for (int k = x+1; k <= j; k++) {
                  if (boxes[k] == boxes[x]) {
                      cntx++;
                      posx[cntx] = k;
                      tmp += f[lastx+1][k-1];
                      lastx = k;
                      flags[k] = flag;
                  } else {
                      if (k == j)
                          tmp += f[lastx+1][j];
                  }
              }
              tmp += cntx*cntx; //加上最后消除numx的得分(此处只考虑所有numx都在最后消除)
              if (tmp > f[i][j])
                  f[i][j] = tmp;
              int other = len-cntx;
              if (cntx==1 || other*other-other <= cntx*cntx-(cntx-1)*(cntx-1))
                  continue; //优化,极端情况(把所有other集合起来利益不如numx缺少一个的损失)不需要下面计算
           
              for (int cnt = 0; cnt <= cntx; cnt++)
                   for (int num = 0; num <= cntx; num++)
                       g[cnt][num] = -233333;
              g[0][0] = 0; //初始化
              //g[i][j]表示前i个numx,一共j个不参与最后一轮(且第i个不参与)的情况下的最优解
              for (int cnt = 1; cnt <= cntx; cnt++) //考虑前cnt个numx
                for (int num = 1; num <= cnt; num++) //枚举不参与最后一轮的numx的个数 
                  for (int per = 0; per < cnt; per++) { //枚举前驱 -- 上一个不参与的numx是谁
                     if (posx[cnt] > 0)
                      g[cnt][num] = max(g[cnt][num], g[per][num-1]+f[posx[per]+1][posx[cnt]-1]);
                     else
                      g[cnt][num] = max(g[cnt][num], g[per][num-1]);
                  }

              for (int num = 1; num <= cntx; num++)
                  f[i][j] = max(f[i][j], g[cntx][num]+num*num); //更新答案
              for (int cnt = 1; cnt < cntx; cnt++)
                  for (int num = 1; num <= cnt; num++)
                      f[i][j] = max(f[i][j], g[cnt][num]+num*num+f[posx[cnt]+1][j]);
                      //考虑最后一个numx不参与的情况
           }
      }
    return f[0][boxesSize-1];
}

作者：wineee
链接：https://leetcode-cn.com/problems/remove-boxes/solution/zhen-zheng-de-fang-fa-er-yong-flr-lai-biao-shi-yi-/
来源：力扣（LeetCode）
著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。
```

- 时间复杂度：$O(n^5)$,比官方题解高,因为常数比较小,实际表现不算太差,击败6.25% 的用户已经很好了。注意不要数for循环,比如x*cnt的两个循环只能算$O(n)$

- 空间复杂度：$O(n^2)$,击败100.00% 的用户无压力
