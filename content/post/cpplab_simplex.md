+++
title = "[管理运筹学]线性规划&单纯形法的各种姿势(题目：[NOI2008]志愿者招募)"
date = 2019-06-29T00:00:00+08:00
description = "simplex"
tags = [
    "c++",
    "单纯型"
]
noToc = true

+++

## 题目描述
申奥成功后，布布经过不懈努力，终于成为奥组委下属公司人力资源部门的主管。布布刚上任就遇到了一个难题：为即将启动的奥运新项目招募一批短期志愿者。经过估算，这个项目需要$ N $天才能完成，其中第 $ i $ 天至少需要$A_i$个人。 布布通过了解得知，一共有$M$类志愿者可以招募。其中第$i$类可以从第$S_i$ 天工作到第 $ T_i $ 天，招募费用是每人$C_i$ 元。新官上任三把火，为了出色地完成自己的工作，布布希望用尽量少的费用招募足够的志愿者，但这并不是他的特长。于是布布找到了你，希望你帮他设计一种最优的招募方案。

<!--more-->

输入格式：
第一行包含两个整数 $N$  , $M$，表示完成项目的天数和可以招募的志愿者的种类。 接下来的一行中包含N 个非负整数，表示每天至少需要的志愿者人数。 接下来的$M$ 行中每行包含三个整数$S_i$, $T_i$, $C_i$，含义如上文所述。为了方便起见，我们可以认为每类志愿者的数量都是无限多的。

输出格式：
仅包含一个整数，表示你所设计的最优方案的总费用。

输入样例#1： 
3 3
2 3 4
1 2 2
2 3 5
3 3 2
输出样例#1： 
14

说明
$1 ≤ N ≤ 1000$，$1 ≤ M ≤ 10000$，题目中其他所涉及的数据均不超过$2^{31}-1$。

## 分析与建模
设第 $i$ 天需要 $x_i$ 个志愿者，记 $a_{ij}$ 为第 $i$ 天第 $j$ 个志愿者是否能工作。
题目要求总费用最低，即求：$Min \, z = \sum_{i=1}^{n}{C_i * x_i}$。
它们需要满足要求：



\begin{cases}
a_{11}x_1+a_{12}x_2+a_{13}x_3...+a_{1m}x_m \geq A_1 \\\\
a_{21}x_1+a_{22}x_2+a_{23}x_3...+a_{2m}x_m \geq A_2 \\\\
...\\\\
a_{n1}x_1+a_{n2}x_2+a_{n3}x_3...+a_{nm}x_m \geq A_n \\\ 
x_1,x_2,x_3  ... x_m \geq 0\\\
\end{cases}$$

这明显是一个的线性规划问题,但它不是标准形式,需要一些改造。
### 方法一 转化为对偶问题
观察它的对偶问题：$Max \, w = \sum_{i=1}^{m}A_i*y_i$。
设$b_{ij} = a_{ji}$, 为第 $i$ 个志愿者第 $j$ 天是否能工作,新的约束条件为：
$$\begin{cases}
b_{11}y_1+b_{12}y_2+b_{13}y_3...+b_{1n}y_n \leq C_1 \\\\
b_{21}y_1+b_{22}y_2+b_{23}y_3...+b_{2n}y_n \leq C_2 \\\\
...\\\\
b_{m1}y_1+b_{m2}y_2+b_{m3}y_3...+b_{mn}y_n \leq C_n \\\\
y_1,y_2,y_3  ... y_n \geq 0\\\\
\end{cases}$$

添加松弛变量后为：
$$\begin{cases}
b_{11}y_1+b_{12}y_2+b_{13}y_3...+b_{1n}y_n+y_{n+1} = C_1 \\\\ 
b_{21}y_1+b_{22}y_2+b_{23}y_3...+b_{2n}y_n+y_{n+2} = C_2 \\\\ 
...\\\\
b_{m1}y_1+b_{m2}y_2+b_{m3}y_3...+b_{mn}y_n+y_{n+m} = C_n \\\\ 
y_1,y_2,y_3  ... y_{n+m} \geq 0\\\\
\end{cases}$$
这是线性规划的标准形式，对偶问题可以使用原始单纯型法求解。
根据对偶定理，若原问题有最优解，那么对偶问题也有最优解，且目标函数值相等。题目要的只有目标函数值，所以可以直接求解对偶问题，但这种方法得到的解不是原问题的解，所以这种方法实际上不太合适，不过它只要最基本的单纯形法，故放第一位。
代码如下：

```cpp
#include <iostream>
#include <cstdio>
#include <cmath>

using namespace std;
const int M=10005,N=1005,INF=1e9;
const double eps=1e-7;

int n,m;
double a[M][N],b[M],c[N],v,cc[N];
int B[N],P[M];

void Out() {
    for (int i = 1; i <= m; i++)  printf("%4d",P[i]);
    puts("");
    for (int i = 1; i <= m; i++) {
        for (int j = 1; j <= n; j++)
            printf("%4d",(int)a[i][j]);
        printf("    B:%4d b:%4d\n",B[i],(int)b[i]);
    }
    for (int i = 1; i <= n; i++)
        printf("%4d", (int)c[i]);
    puts("\nEnd");
}

void pivot(int l,int e) { //旋转运算,l换出变量 e换入变量
    swap(B[l],P[e]);
    b[l]/=a[l][e];
    for (int j=1;j<=n;j++) if(j!=e) a[l][j]/=a[l][e];
// 行变换，处理l对应行,将a[l][e]变为1
    a[l][e]=1/a[l][e];

    for (int i=1;i<=m;i++) if(i!=l&&fabs(a[i][e])>0) {  //其他行，e对应列，除a[l][e]外变为0
        b[i] -= a[i][e]*b[l];
        for(int j=1;j<=n;j++) if(j!=e) a[i][j]-=a[i][e]*a[l][j];
        a[i][e]=-a[i][e]*a[l][e];
    }
//需要注意的是，e列将对应新的非基变量，即原基变量l对应的列
    v += c[e]*b[l]; //x的检验数即x值增加1对答案的影响，新的基变量值为b[l]
    for (int j=1;j<=n;j++) if(j!=e) c[j] -= c[e]*a[l][j];  //更新检验数
    c[e] = -c[e]*a[l][e];
}

double simplex() {
    while(true) {
        int e=1,l=0;//l换出变量 e换入变量
        for(int t=2; t<=n; t++) if(c[t]>c[e]) e = t; //找到检验数最大的变量
        if (c[e] < eps) return v;//所以检验数<=0，最优解的情况
        double mn=INF;
        for(int i=1;i<=m;i++) //找换出变量，a[i][e]>0且b[i]/a[i][e]最小
            if(a[i][e]>eps&&mn>b[i]/a[i][e]) mn=b[i]/a[i][e],l=i;
        if(mn==INF) return INF;//无可行解
        pivot(l,e);
       // Out();
    }
}

int main() {
    cin >> n >> m;
    for (int i=1; i<=n; i++) {
        cin >> cc[i];
        c[i] = cc[i];
    }
    for (int i=1; i<=m; i++) {
        int s,t; cin >> s >> t;
        for (int j=s; j<=t; j++) a[i][j]=1;
        cin >> b[i];
        P[i] = i;
    }
    for (int i = 1; i <= n; i++) B[i] = m+i;
    //Out();
    printf("%d",(int)simplex());
}

```

### 方法二 人工变量法
在加入剩余变量后，分别给每个约束方程加入人工变量$x_{m+n+1},...,x_{m+n+n}$,得到初始基可行解。
$$\begin{cases}
a_{11}x_1+a_{12}x_2+a_{13}x_3...+a_{1m}x_m - x_{m+1} + x_{m+n+1} = A_1 \\\\ 
a_{21}x_1+a_{22}x_2+a_{23}x_3...+a_{2m}x_m - x_{m+2} + x_{m+n+2} = A_2 \\\\ 
...\\\\
a_{n1}x_1+a_{n2}x_2+a_{n3}x_3...+a_{nm}x_m - x_{m+n} + x_{m+n+n} = A_n \\\\ 
x_1,x_2,x_3  ... x_m+n+n \geq 0\\\\
\end{cases}$$

#### 1. 大M法
人工变量不能影响目标函数取值，必须从基变量中换出来。为此，可以在目标函数中把人工变量的系数设成M(一个充分大的数/惩罚系数)，使得在求Min的过程中，人工变量变为非基变量可以大大减小目标函数值
$Min \, z = \sum_{i=1}^{n}{C_i * x_i}+\sum_{j=m+n+1}^{m+n+n}M*x_j$


```cpp
#include <iostream>
#include <cstdio>
#include <cmath>

using namespace std;
typedef long long ll;

const int M=12005,N=1005,INF=1e5;
const double eps=1e-6;

int n,m;
double a[N][M],b[N],c[M],v,cc[M];
int B[N];

void pivot(int l,int e) {
    b[l]/=a[l][e];
    for(int j=1;j<=n;j++) if(j!=e) a[l][j]/=a[l][e];
    a[l][e]=1;

    for(int i=1;i<=m;i++) if(i!=l&&fabs(a[i][e])>0) {
        b[i] -= a[i][e]*b[l];
        for(int j=1;j<=n;j++) if(j!=e) a[i][j]-=a[i][e]*a[l][j];
        a[i][e]=0;
    }
   for (int j=1;j<=n;j++) if(j!=e) c[j] -= c[e]*a[l][j];
   c[e] = 0;
   B[l] = e;
}

void Out() {
    puts("");
    for (int i = 1; i <= m; i++) {
        for (int j = 1; j <= n; j++)
            printf("%4d",(int)a[i][j]);
        printf("    B:%4d b:%4d\n",B[i],(int)b[i]);
    }
    for (int i = 1; i <= n; i++)
        printf("%4d", (int)c[i]);
    puts("\nEnd");
}

double simplex() {
    for (int i = 1; i <= n; i++) {
      cc[i] = c[i];
      for (int j = 1; j <= m; j++) {
       c[i] = c[i]-INF*a[j][i];
       }
    }
    while(true) {
        int e=0,l=0;
        for (int t=1; t<=n; t++)
          if (c[t]<-eps) {
           e = t;
           break;
         }
        if (e == 0) {
            v = 0;
            for (int i = 1; i <= m; i++) v += b[i]*cc[B[i]];
            return v;
        }
        double mn=INF;
        for(int i=1;i<=m;i++)
            if(a[i][e]>eps && mn>b[i]/a[i][e]) mn=b[i]/a[i][e],l=i;
        if(mn==INF) return INF;
        pivot(l,e);
        //Out();
    }
}

int main() {
    cin >> n >> m;
    for (int i=1; i<=n; i++) cin >> b[i];
    for (int i=1; i<=m; i++) {
        int s,t; cin >> s >> t;
        for (int j=s; j<=t; j++) a[j][i]=1;
        cin >> c[i];
    }
    for (int i = 1; i <= n; i++) {
            a[i][m+i] = -1;
            a[i][m+n+i] = 1;
            B[i] = m+n+i;
    }
    for (int i = m+n+1; i <= m+n+n; i++) c[i] = INF;
    m += n+n;
    swap(n,m);
    //Out();
    printf("%d",(int)simplex());
}
```
#### 2. 两阶段法
两阶段法则更为直接，第一步，目标函数为$Min \, w = \sum_{i=m+1}^{m+n}{x_i}$，即尽量使人工变量取值为0，如果$w>0$，说明人工变量换不出去，即无解。若$w = 0$，说明有可行解，进行第二阶段，把矩阵中人工变量部分删掉，目标函数换回原问题的，即$Min \, z = \sum_{i=1}^{n}{C_i * x_i}$

```c
#include <iostream>
#include <cstdio>
#include <cmath>

using namespace std;
typedef long long ll;

const int M=12005,N=1005,INF=1e5;
const double eps=1e-6;

int n,m;
int a[N][M],b[N],c[M],v,cc[M],A[M];
int B[N];

void pivot(int l,int e) {
    b[l]/=a[l][e];
    for(int j=1;j<=n;j++) if(j!=e) a[l][j]/=a[l][e];
    a[l][e]=1;

    for(int i=1;i<=m;i++) if(i!=l&&fabs(a[i][e])>0) {
        b[i] -= a[i][e]*b[l];
        for(int j=1;j<=n;j++) if(j!=e) a[i][j]-=a[i][e]*a[l][j];
        a[i][e]=0;
    }
   for (int j=1;j<=n;j++) if(j!=e) c[j] -= c[e]*a[l][j];
   c[e] = 0;
   B[l] = e;
}

void Out() {
    puts("");
    for (int i = 1; i <= m; i++) {
        for (int j = 1; j <= n; j++)
            printf("%4d",(int)a[i][j]);
        printf("    B:%4d b:%4d\n",B[i],(int)b[i]);
    }
    for (int i = 1; i <= n; i++)
        printf("%4d", (int)c[i]);
    puts("\nEnd");
}

int simplex() {
    while(true) {
        int e=0,l=0;
        for (int t=1; t<=n; t++)
          if (c[t]<-eps) {
           e = t;
           break;
         }
        if (e == 0) {
            v = 0;
            for (int i = 1; i <= m; i++) {
            v += b[i]*cc[B[i]];
          }
            return v;
        }
        int mn=INF;
        for(int i=1;i<=m;i++)
            if(a[i][e]>eps && mn>b[i]/a[i][e]) mn=b[i]/a[i][e],l=i;
        if(mn==INF) return INF;
        pivot(l,e);
        //Out();
    }
}

int main() {
    cin >> n >> m;
    for (int i=1; i<=n; i++) cin >> b[i];
    for (int i=1; i<=m; i++) {
        int s,t; cin >> s >> t;
        for (int j=s; j<=t; j++) a[j][i]=1;
        cin >> A[i];
    }
    for (int i = 1; i <= n; i++) {
            a[i][m+i] = -1;
            a[i][m+n+i] = 1;
            B[i] = m+n+i;
    }

    for (int i = m+n+1; i <= m+n+n; i++) cc[i] = c[i] = 1;
    m += n+n;
    swap(n,m);

    //Out();
    for (int i = 1; i <= n; i++)
      for (int j = 1; j <= m; j++) {
        c[i] = c[i]- 1*a[j][i];
      }
    if (simplex() < eps) {
    	n -= m;
    	for (int i = 1; i <= n; i++) c[i] = cc[i] = A[i];
    	for (int i = 1; i <= n; i++)
          for (int j = 1; j <= m; j++) {
            c[i] = c[i]- cc[B[j]]*a[j][i];
          }
    	printf("%d",(int)(simplex()));
    } else {
        puts("Unsolvable!");
    }
    return 0;
}
```
###方法三 对偶单纯形法

将所有式子同乘$-1$后得到类似标准形式，但是b列出现负数，不能用原始单纯形法。
整理一下式子：$Max \,z_{'} = \sum_{i=1}^{n}{-C_i * x_i}$

$$\begin{cases}
-a_{11}x_1-a_{12}x_2-a_{13}x_3...-a_{1m}x_m + x_{m+1} = -A_1 \\\\ 
-a_{21}x_1-a_{22}x_2-a_{23}x_3...-a_{2m}x_m + x_{m+2} = -A_2 \\\\ 
...\\\\
-a_{n1}x_1-a_{n2}x_2-a_{n3}x_3...-a_{nm}x_m + x_{m+n} = -A_n \\\\
x_1,x_2,x_3  ... x_m \geq 0\\\\
\end{cases}$$
这里只介绍一下步骤。
(1)对线性规划问题是所有检验数<=0,即对偶问题为基可行解。(本题初始既满足)。
(2)检验：若b列都非负，检验数都非正，已达到最优解，停止计算。
(3)按$min[(B^{-1}b)_i,|(B^{-1}b<0)_i]=(B^{-1}b)_l$对应的基变量$x_l$为换出变量
(4)检查$x_l$行各系数$a_{lj}$，若所有$a_{lj}>=0$，说明无解。否则，计算$\theta = min_j(\frac{c_j-z_j}{a_{lj}} |a_{lj}<0) = \frac{c_k-z_k}{a_{lk}}$，以对应的$x_k$ 为换入变量($\theta$规则保证对偶问题的解仍为可行解)。
(5)以$a_{lk}$为主元素，进行迭代，得到新表。
重复(2)～(5)。

```cpp
#include <iostream>
#include <cstdio>
#include <cmath>

using namespace std;

const int M=12005,N=1205,INF=1e7;
const double eps=1e-6;

int n,m;
double a[N][M],b[N],c[M],v,cc[M],A[N];
int B[N];

void pivot(int l,int e) {
    b[l]/=a[l][e];
    for(int j=1; j<=n; j++)
        if(j!=e)
            a[l][j]/=a[l][e];
    a[l][e]=1;

    for(int i=1; i<=m; i++)
        if(i!=l&&fabs(a[i][e])>0) {
            b[i] -= a[i][e]*b[l];
            for(int j=1; j<=n; j++)
                if(j!=e)
                    a[i][j]-=a[i][e]*a[l][j];
            a[i][e]=0;
        }

    for (int j=1; j<=n; j++)
        if(j!=e)
            c[j] -= c[e]*a[l][j];
    c[e] = 0;
    B[l] = e;
}

void Out() {
    puts("");
    for (int i = 1; i <= m; i++) {
        for (int j = 1; j <= n; j++)
            printf("%4d",(int)a[i][j]);
        printf("    B:%4d b:%4d\n",B[i],(int)b[i]);
    }
    for (int i = 1; i <= n; i++)
        printf("%4d", (int)c[i]);
    puts("\nEnd");
}

double dsimplex() {
    while (true) {
        int e=1,l=0;
        for (int t=2; t<=m; t++)
          if (b[t] < b[e]) e = t;
        if (b[e] > -eps) {
            v=0;
            for (int i = 1; i <= m; i++) v += b[i]*cc[B[i]];
            //for (int i = 1; i <= m; i++) v += (-c[n-m+i])*A[i];
            return v;
        }

        double mn=INF;
        for (int i = 1; i <= n; i++)
            if (a[e][i] < -eps && mn > c[i]/a[e][i])
                mn=c[i]/a[e][i],l=i;

        if (mn==INF) return INF;
        pivot(e,l);
      //  Out();
    }
}

int main() {
    cin >> n >> m;
    for (int i=1; i<=m; i++) {
        cin >> A[i];
        b[i] = -A[i];
    }
    for (int i=1; i<=m; i++) {
        int s,t;
        cin >> s >> t;
        for (int j=s; j<=t; j++)
            a[j][i] = -1;
        cin >> cc[i];
        c[i] = -cc[i];
    }
    for (int i = 1; i <= n; i++) {
        a[i][n+i] = 1;
        B[i] = n+i;
    }
    m += n;
    swap(n,m);
   //Out();
    printf("%d",(int)(dsimplex()));
    return 0;
}

```
## 一些补充说明
1.这四个程序在运算速度上没有本质上的差别，实际测试中人工变量法稍慢。

2.原问题的线性规划模型是一步到位的，但不是标准形式，具体的解决方法有很多，可见线性规划与单纯形法内容丰富，体系比较完备。

3.在已有的题解中，绝大多数作者是使用网络流来做。本题费用流的建图方法颇有挑战性，而线性规划模型简单很多，程序实现上单纯形法稍微麻烦一点，而这两类方法时间复杂度都比较高而且玄学，难分高下。总的来说，线性规划的方法更方便。

供参考的资料：
https://10420.blog.luogu.org/solution-p3980 | 
https://blog.csdn.net/little_cats/article/details/81189794
（注：这篇是作为《管理运筹学》自选题写的）