+++
title = "[c语言课程设计] 重排九宫问题"
date = "2019-02-05"
description = "Jiugongge"
tags = ["康托展开","bfs","c"]

+++

## 题目 重排9宫

### 设计内容：

编写软件模拟排九宫。从九宫的某个状态出发，移动格子中的数字，使9宫格中的数字顺序排列，空格在最后。

### 设计功能：

1、九宫初始化，绘制九宫初始状态。9宫格中只有一个位置是空的，其它位置分别是1-8的8个数字，

2、手工输入移动九宫的步骤，如11d(第一行一列格中数字下移)，22r(第二行二列格中数字右移)，23u(第二行三列格中数字上移)等，记录移动后九宫状态，并形象绘制。要求判断移动是否合法。一次只能移动一个数字，并且只能到相邻的空格子中。

3、记录从初始状态以后的移动步骤。

4、判断成功状态，并给出成功提示。

5、你能否让计算机自动求解（思考选作）。

<!--more-->

| 7    | 5    | 6    |
| ---- | ---- | ---- |
| 8    |      | 2    |
| 4    | 3    | 1    |

初始状态



| 1    | 2    | 3    |
| ---- | ---- | ---- |
| 4    | 5    | 6    |
| 7    | 8    |      |

完成状态

### 程序源代码

```cpp
/*
输入命令介绍 
   w  手动输入九宫格（用0表示空白格） 
   m  进入移动模式 
       11d 第一行一列格中数字下移
	   22r 第二行二列格中数字右移
	   23u 第二行三列格中数字上移
	   ...诸如此类 
	   p 输出从初始状态以后的移动步骤 
       e 退出移动模式 
   r  随机生成一个九宫格 
   f  自动求解 
   e  退出程序 
*/ 
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int map[3][3];
void swap(int *a,int *b) {
	int t = *a; *a = *b; *b = t;
}

void print_map(int map[3][3]) {
   int i,j;
   for (i = 0; i < 3; i++) {
     for (j = 0; j < 3; j++) 
        if(map[i][j]) printf("%d ",map[i][j]);
	    else printf("  ");
	 puts("");
   }
}

void write() {
   int i,j,v[10] = {0};	
   for (i = 0; i < 3; i++)
     for (j = 0; j < 3; j++) {
       scanf("%d",&map[i][j]);
	   v[map[i][j]] = 1; 
	 }
   for (i = 0; i < 9; i++) 
    if (v[i] != 1) {  
    	printf("您的输入不合法，请重试!\n");
    	write(); 
    	return;
	}
   printf("输入成功!结果如下\n");
   print_map(map);	  
}

void random_map() {
	int i,j;
	for (i = 0; i < 3; i++)
     for (j = 0; j < 3; j++)
      map[i][j] = i*3+j;
    for (i = 0; i < 9; i++) 
      swap(&map[rand()%3][rand()%3],&map[rand()%3][rand()%3]);
    print_map(map);
}

int in_map(int x,int y) {
	return x>=0 && y>=0 && x<3 && y<3;
}

int map_to_int(int a[3][3]) {
	int ans = 0,i,j;
	for (i = 0; i < 3; i++)
     for (j = 0; j < 3; j++) 
      ans = ans*10+a[i][j];
	return ans;
}

void move() {
	char s[5];
	int st[300][3],top = 0;
	while (~scanf("%s",s)) {
	  if(s[0] == 'e') return;
	  if(s[0] == 'p') {
  		for (int i = 0; i < top; i++) {
		   printf("第%d行%d列格中数字",st[i][0]+1,st[i][1]+1);  
		   if(st[i][2] == 0) puts("上移"); 	
		   else if(st[i][2] == 1) puts("下移"); 
		   else if(st[i][2] == 2) puts("左移"); 
		   else puts("右移"); 
	    }
	    continue; 
   	  }  
	  int dx = 2,dy;
	  switch(s[2]) {
	    case 'u': dx=-1,dy=0,st[top][2]=0; break;
		case 'd': dx=1,dy=0,st[top][2]=1;  break;
		case 'l': dx=0,dy=-1,st[top][2]=2; break;
		case 'r': dx=0,dy=1,st[top][2]=3;  break;
		defalt: printf("输入不合法!\n");
	  }
	  if(dx == 2) continue;
	  int x = s[0]-'1',y = s[1]-'1';   
	  if(!in_map(x+dx,y+dy) || !in_map(x,y)) {
	    printf("输入不合法!\n");
	    continue; 
	  } 
	  swap(&map[x][y],&map[x+dx][y+dy]);
	  st[top][0] = x; st[top++][1] = y;
	  print_map(map);
	  if(map_to_int(map) == 123456780)
	    puts("这是完成状态!!");
	}
}

#define MAXX 362883
int q[MAXX],head,tail;
int dis[MAXX],per[MAXX];
int xx[] = {1,-1,0,0};
int yy[] = {0,0,1,-1};

int next(int x) {
	return ++x == MAXX ? x = 0 : x;
}

int fac[11] = {1,1,2,6,24,120,720,5040,40320,362880,3628800};
int map_to_order(int a[3][3]) {	
    int i,j,ans = 0;
	for (i = 0; i < 9; i++) {
	  int cnt = 0;
	  for (j = i+1; j < 9; j++) 
	   if (a[j/3][j%3] < a[i/3][i%3]) cnt++;
      ans += cnt*fac[8-i];
	} 
	return ans; 
}

void int_to_map(int x,int a[3][3]) {
	int i;
	for (i = 8; ~i; i--,x /= 10)
	  a[i/3][i%3] = x%10;
}

int ans[MAXX],top;
void find() {
	head = 1; tail = 2;
	q[1]  =  map_to_int(map);
    if(q[1] == 123456780) {
	  printf("无需移动！\n");
  	  return;
    }
	int a[3][3],i,x,y,k;
	for (i = 0; i < MAXX; i++)
      dis[i] = 0xffffff,per[i] = 0;
	dis[map_to_order(map)] = 0;
	while(head != tail) {
	  int_to_map(q[head],a);
	  int old_id = map_to_order(a);
	  for (x = 0; x < 3; x++)
        for (y = 0; y < 3; y++)
          for (i = 0; i < 4; i++)
            if (in_map(x+xx[i],y+yy[i])) {
  	          swap(&a[x][y],&a[x+xx[i]][y+yy[i]]);
      	      int new_id = map_to_order(a);
		      if (dis[new_id] > dis[old_id]+1) {
	 	        dis[new_id] = dis[old_id]+1;
	 	        per[new_id] = q[head];
	 	        int id_int = map_to_int(a);
	 	        if (id_int == 123456780){
	              printf("可以通过%d次变换完成\n",dis[new_id]);	
	              top = 0;
	              for (k = id_int; k; ) {
                    ans[++top] = k;
                    int_to_map(k,a);
			        k = per[map_to_order(a)];	
	              }
	              int ttop = top;
	              while(top) {
			 	  	printf("第%d次变换后为：\n",ttop-top);
                    int_to_map(ans[top],a);
                    print_map(a);puts("");
	  	            top--;
	              }
	              return;
                }
	 	        q[tail] = id_int; tail = next(tail);
		     }
		     swap(&a[x][y],&a[x+xx[i]][y+yy[i]]);
	     }
	  	 head = next(head);
	} 
}

int main() {
   srand(time(0));
   char opt;
   while(~scanf("%c",&opt)) {
     if(opt == 'e') break;
	 else if(opt == 'w') write();
	 else if(opt == 'm') move();
	 else if(opt == 'r') random_map();
	 else if(opt == 'f') find();
   }
   return 0;
}
```

