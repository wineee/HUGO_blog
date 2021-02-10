+++
title = "[管理运筹学]指派问题的匈牙利算法及其c++实现 (例:「网络流 24 题」分配问题 )"
date = 2019-06-28T00:00:00+08:00
description = "Jiugongge"
tags = [
    "c++",
    "匈牙利算法"
]
noToc = true

+++



**题目描述**

有 n 件工作要分配给 n 个人做。第 i 个人做第 j 件工作产生的效益为 c[i][j] 。试设计一个将 n 件工作分配给 n 个人做的分配方案，使产生的总效益最大。
输入格式：
文件的第 1 行有 1 个正整数 n，表示有 n 件工作要分配给 n个人做。
接下来的n 行中，每行有 n 个整数 c[i][j]，表示第 i个人做第 j 件工作产生的效益为 c[i][j]。
输出格式：
两行分别输出最小总效益和最大总效益。

<!--more-->

求最小总效益就是经典的指派问题，最大总收益只要把c乘-1再求最小就可以。
这里举个例子
n = 5，c为下表



|  12  |  7   |  9   |  7   | 9    |
| :--: | :--: | :--: | :--: | ---- |
|  8   |  9   |  6   |  6   | 6    |
|  7   |  17  |  12  |  14  | 9    |
|  15  |  14  |  6   |  6   | 10   |
|  4   |  10  |  7   |  10  | 9    |



第一步：每行减去该行最小的数，保证每行都有0。



|  5   |  0   |  2   |  0   | 2    |
| :--: | :--: | :--: | :--: | ---- |
|  2   |  3   |  0   |  0   | 0    |
|  0   |  10  |  5   |  7   | 2    |
|  9   |  8   |  0   |  0   | 4    |
|  0   |  6   |  3   |  6   | 5    |



第二步：每列减去该列最小的数，保证每行每列都有0。



|  5   |  0   |  2   |  0   | 2    |
| :--: | :--: | :--: | :--: | ---- |
|  2   |  3   |  0   |  0   | 0    |
|  0   |  10  |  5   |  7   | 2    |
|  9   |  8   |  0   |  0   | 4    |
|  0   |  6   |  3   |  6   | 5    |


第三步：
从单个0元素的行开始，给0加圈，记作O,然后划去所在行的其它0元素，记为X。
第四步：
从单个0元素的列开始，给0加圈，记作O,然后划去所在列的其它0元素，记为X。
重复三四，直到无法标记；
第五步：
若还存在没有画圈的0元素，则从剩余的0元素最少的行(列)开始，选0元素画圈，然后划掉同行同列的其它0元素，反复进行，直到所有0元素均被圈出或划掉为止； 
检验：
若O的数目cnt=n，则该指派问题的最优解已经得到。
否则，进行调整。



|  5   |  O   |  2   |  X   | 2    |
| :--: | :--: | :--: | :--: | ---- |
|  2   |  3   |  X   |  X   | O    |
|  O   |  10  |  5   |  7   | 2    |
|  9   |  8   |  O   |  X   | 4    |
|  X   |  6   |  3   |  6   | 5    |


例子cnt = 4，少一个O
调整：找最少覆盖所有0的直线
1) 对没有O的行打√
2) 对已打√行中含X所在列打√
3) 对已打√列中含O所在行打√
4) 重复2~3, 直至没有要打√的行和列为止
 5) 对没有打√的行划横线, 对打√的列划竖线，得到最少覆盖所有0的直线。
6) 取未划线的最小数，未划线的减去这个数，线交点处加上这个数。
返回第一步。

打√后

|  5   |  O   |  2   |  X   |  2   |      |
| :--: | :--: | :--: | :--: | :--: | ---- |
|  2   |  3   |  X   |  X   |  O   |      |
|  O   |  10  |  5   |  7   |  2   | √3   |
|  9   |  8   |  O   |  X   |  4   |      |
|  X   |  6   |  3   |  6   |  5   | √1   |
|  √2  |      |      |      |      |      |


调整6后



|  7   |  0   |  2   |  0   | 2    |
| :--: | :--: | :--: | :--: | ---- |
|  4   |  3   |  0   |  0   | 0    |
|  0   |  8   |  3   |  5   | 0    |
|  11  |  8   |  0   |  0   | 4    |
|  0   |  4   |  1   |  4   | 3    |

正确性初步说明：同一行或同一列减去同一个数不影响最优分配方案

程序的一点说明：实际计算中， 第五步其实不需要dfs所有情况，模拟即可。相应调整5后可以这样: 设直线数为l，若l小于n，则转调整6；若l=n，则转第五步重新试探。程序直接枚举到最好的情况了，所以省了这个判断。

```cpp
#include <iostream>
#include <cstdio>
#include <algorithm>

using namespace std;

#define N 110
void Out(int a[N][N],int v[N][N],int n) {
     for (int i = 1; i <= n; i++){
        for (int j = 1; j <= n; j++) {
           if(!v[i][j]) printf("%4d",a[i][j]);
           else printf("   %c",v[i][j]==1?'O':'X');
        }
       puts("");
    }puts("End!");
}

int HZ[N],LZ[N];

struct Pair {
    int x,y;
    Pair(int x = 0,int y = 0):x(x),y(y){}
    bool operator < (const Pair & b) const {
        return HZ[this->x]==HZ[b.x] ? LZ[this->y]<LZ[b.y] : HZ[this->x]<HZ[b.x];
    }
}Pt[N*N];

int FH[N],FL[N];
int Maxx,Mx;
int st[N*N],tot,used[N*N];
void dfs(int s,int t,int sum) {
    if (sum > Mx) {
        Mx = sum;
        used[0] = tot;//记录最好方案
        for (int i = 1; i <= tot; i++) used[i] = st[i];
    }
    if (Mx == Maxx) return;//已经找到满意解
    if (t-s+1+sum <= Mx) return;//乐观估计不如目前最优解
    if (s > t) return;
    if (!FH[Pt[s].x] && !FL[Pt[s].y]) { //选s点
        FH[Pt[s].x] = 1;
        FL[Pt[s].y] = 1;
        st[++tot] = s;
        dfs(s+1,t,sum+1);
        --tot;
        FH[Pt[s].x] = 0;
        FL[Pt[s].y] = 0;
    }
    dfs(s+1,t,sum);//不选s点
}

int calc(int b[N][N],int n) {
    int a[N][N],v[N][N];
    for (int i = 1; i <= n; i++)
        for (int j = 1; j <= n; j++)
            a[i][j] = b[i][j];
    for (int i = 1; i <= n; i++) {
        int t = a[i][1];
        for (int j = 2; j <= n; j++)
          if (a[i][j] < t) t = a[i][j];
        for (int j = 1; j <= n; j++)
          a[i][j] -= t;
    }
    for (int i = 1; i <= n; i++) {
        int t = a[1][i];
        for (int j = 2; j <= n; j++)
          if (a[j][i] < t) t = a[j][i];
        for (int j = 1; j <= n; j++)
          a[j][i] -= t;
    }
    //先让每行每列都有0
//Out(a,v,n);
    int H[N],L[N];
    while(1) {
      for (int i = 1; i <= n; i++) {
        H[i] = L[i] = 0; //H[i]，第i行有多少个0，L为列
        FH[i] = FL[i] = 0; //FH[i],第i行有没有画O
        for (int j = 1; j <= n; j++) v[i][j] = 0;//v[i][j] = 1代表‘O’,-1代表‘X’
      }
      for (int i = 1; i <= n; i++)
       for (int j = 1; j <= n; j++)
         if (a[i][j] == 0) {
            H[i]++; L[j]++;
         }
      int cnt = 0;
      while (1) {
        int tpcnt = cnt;
        for (int i = 1; i <= n; i++) //找每行单独的0画‘O’，同列画‘X’
         if (H[i] == 1) {
           int t = 1;
           while(a[i][t] || v[i][t]) t++;
           v[i][t] = 1;
           cnt++;  //cnt记有几个‘O’
           H[i]--; L[t]--;
           FH[i] = 1; FL[t] = 1;
           for (int j = 1; j <= n; j++)
            if (a[j][t]==0 && j!=i && v[j][t]==0) {
              v[j][t] = -1;
              H[j]--; L[t]--;
           }
        }
        for (int i = 1; i <= n; i++) //对称的
         if (L[i] == 1) {
           int t = 1;
           while(a[t][i] || v[t][i]) t++;
           v[t][i] = 1;
           cnt++;
           H[t]--; L[i]--;
           FH[t] = 1; FL[i] = 1;
           for (int j = 1; j <= n; j++)
            if (a[t][j]==0 && j!=i && v[t][j]==0) {
              v[t][j] = -1;
              H[t]--; L[j]--;
            }
         }
         if (tpcnt == cnt) break;
        }
//Out(a,v,n);
      int top = 0;
      for (int i = 1; i <= n; i++)
       for (int j = 1; j <= n; j++)
        if (a[i][j]==0 && v[i][j]==0) {
            Pt[++top] = Pair(i,j);
            HZ[i]++;
            LZ[j]++;
        }
      sort(Pt+1,Pt+top+1);//同行同列少的排前面
      Maxx = n-cnt;
      Mx = 0; used[0] = 0;
      dfs(1,top,0);//对剩下的0进行试探画‘O’
      cnt += Mx;
      for (int i = 1; i <= used[0]; i++) {
            v[Pt[used[i]].x][Pt[used[i]].y] = 1;
            FH[Pt[used[i]].x] = 1;
            FL[Pt[used[i]].y] = 1;
      }
//Out(a,v,n);
      if (cnt == n) { //已经找到
        int ans = 0;
        for (int i = 1; i <= n; i++)
          for (int j = 1; j <= n; j++)
            if (v[i][j] == 1) ans += b[i][j];
        return ans;
      }
      int flagx[N],flagy[N]; //对号标记
      for (int i = 1; i <= n; i++) flagx[i] = flagy[i] = 0;
      int cas = 1;//时间戳，每次只检查新增对号行/列
      for (int i = 1; i <= n; i++)
         if (!FH[i]) flagx[i] = cas;
      bool chang = 1;
      while (chang) {
        chang = 0;
        cas++;
        for (int i = 1; i <= n; i++)
          if (flagx[i] == cas-1)
            for (int j = 1; j <= n; j++)
              if (v[i][j] == -1) {
                flagy[j] = cas;
                chang = 1;
              }
        for (int i = 1; i <= n; i++)
          if (flagy[i] == cas-1)
            for (int j = 1; j <= n; j++)
              if (v[j][i] == 1) {
                flagx[j] = cas;
                chang = 1;
              }
      }
      int Mi = ~0u>>2;
      for (int i = 1; i <= n; i++)
        for (int j = 1; j <= n; j++)
          if (flagx[i] && !flagy[j] && Mi > a[i][j])
            Mi = a[i][j];  //未划线找最小的
       for (int i = 1; i <= n; i++)
        for (int j = 1; j <= n; j++)
          if (flagx[i] && !flagy[j]) //未划线
             a[i][j] -= Mi;
          else if (!flagx[i] && flagy[j]) //线交点
             a[i][j] += Mi;
    }
}

int main() {
    int n,a[N][N],b[N][N];
    scanf("%d",&n);
    for (int i = 1; i <= n; i++)
     for (int j = 1; j <= n; j++) {
       scanf("%d",&a[i][j]);
       b[i][j] = -a[i][j];
     }
    printf("%d\n%d",calc(a,n),-calc(b,n));
    return 0;
}
```