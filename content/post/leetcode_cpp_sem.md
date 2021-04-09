---
title: 使用c++解决leetcode多线程题题目（之一）semaphore
date: 2021-04-09
math: true
tags:  ["c++", "leetcode", "并发"]
---

通过 `man sem_init` 可以查看具体文档
或者使用： https://man7.org/linux/man-pages/man3/sem_init.3.html

sem_init函数是Posix信号量操作中的函数。sem_init() 初始化一个定位在 sem 的匿名信号量。pshared 参数指明信号量是由进程内共享，还是由进程之间共享。如果 pshared 的值为 0，那么信号量将被进程内的线程共享。value 参数指定的初始值。 

sem_post是给信号量的值加上一个“1”，它是一个“原子操作”－－－即同时对同一个信号量做加“1”操作的两个线程是不会冲突的。

sem_wait是一个函数，也是一个原子操作，它的作用是从信号量的值减去一个“1”，但它永远会先等待该信号量为一个非零值才开始做减法。也就是说，如果你对一个值为2的信号量调用sem_wait()，线程将会继续执行，将信号量的值将减到1。如果对一个值为0的信号量调用sem_wait()，这个函数就会原地等待直到有其它线程增加了这个值使它不再是0为止。

<!--more-->

### [ 交替打印FooBar](https://leetcode-cn.com/problems/print-foobar-alternately) 

> 我们提供一个类：
>
> class FooBar {
>   public void foo() {
>     for (int i = 0; i < n; i++) {
>       print("foo");
>     }
>   }
>
>   public void bar() {
>     for (int i = 0; i < n; i++) {
>       print("bar");
>     }
>   }
> }
> 两个不同的线程将会共用一个 FooBar 实例。其中一个线程将会调用 foo() 方法，另一个线程将会调用 bar() 方法。
>
> 请设计修改程序，以确保 "foobar" 被输出 n 次。
>
> 来源：力扣（LeetCode）
> 链接：https://leetcode-cn.com/problems/print-foobar-alternately
> 著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。

```c++
#include <semaphore.h>

class FooBar {
private:
    int n;
    sem_t sem_a, sem_b;

public:
    FooBar(int n) {
        this->n = n;
        sem_init(&sem_a, 0, 0);
        sem_init(&sem_b, 0, 0);
        sem_post(&sem_a);
    }

    void foo(function<void()> printFoo) {
        
        for (int i = 0; i < n; i++) {
            sem_wait(&sem_a);
        	// printFoo() outputs "foo". Do not change or remove this line.
        	printFoo();
            sem_post(&sem_b);
        }
    }

    void bar(function<void()> printBar) {
        
        for (int i = 0; i < n; i++) {
            sem_wait(&sem_b);
        	// printBar() outputs "bar". Do not change or remove this line.
        	printBar();
            sem_post(&sem_a);
        }
    }
};
```

### [ 按序打印](https://leetcode-cn.com/problems/print-in-order) 

> 我们提供了一个类：
>
> public class Foo {
>   public void first() { print("first"); }
>   public void second() { print("second"); }
>   public void third() { print("third"); }
> }
> 三个不同的线程 A、B、C 将会共用一个 Foo 实例。
>
> 一个将会调用 first() 方法
> 一个将会调用 second() 方法
> 还有一个将会调用 third() 方法
> 请设计修改程序，以确保 second() 方法在 first() 方法之后被执行，third() 方法在 second() 方法之后被执行。
>
> 来源：力扣（LeetCode）
> 链接：https://leetcode-cn.com/problems/print-in-order
> 著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。

```c++
#include <semaphore.h>

class Foo {
    sem_t sem_a, sem_b;
public:
    int a,b; 
    Foo() {
        sem_init(&sem_a, 0, 0);
        sem_init(&sem_b, 0, 0);
    }

    void first(function<void()> printFirst) {
        // printFirst() outputs "first". Do not change or remove this line.
        printFirst();
        sem_post(&sem_a);
    }

    void second(function<void()> printSecond) {
        sem_wait(&sem_a);
        // printSecond() outputs "second". Do not change or remove this line.
        printSecond();
        sem_post(&sem_b);
    }

    void third(function<void()> printThird) {
        sem_wait(&sem_b);
        // printThird() outputs "third". Do not change or remove this line.
        printThird();
    }
};
```

### [打印零与奇偶数](https://leetcode-cn.com/problems/print-zero-even-odd) 
>
> 假设有这么一个类：
>
> class ZeroEvenOdd {
>   public ZeroEvenOdd(int n) { ... }      // 构造函数
>   public void zero(printNumber) { ... }  // 仅打印出 0
>   public void even(printNumber) { ... }  // 仅打印出 偶数
>   public void odd(printNumber) { ... }   // 仅打印出 奇数
> }
> 相同的一个 ZeroEvenOdd 类实例将会传递给三个不同的线程：
>
> 线程 A 将调用 zero()，它只输出 0 。
> 线程 B 将调用 even()，它只输出偶数。
> 线程 C 将调用 odd()，它只输出奇数。
> 每个线程都有一个 printNumber 方法来输出一个整数。请修改给出的代码以输出整数序列 010203040506... ，其中序列的长度必须为 2n。
>
> 
>
> 来源：力扣（LeetCode）
> 链接：https://leetcode-cn.com/problems/print-zero-even-odd
> 著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。

```c++
#include <semaphore.h>

class ZeroEvenOdd {
private:
    int n;
    sem_t sem_zero, sem_odd, sem_even;

public:
    ZeroEvenOdd(int n) {
        this->n = n;
        sem_init(&sem_zero, 0, 1); //初始化一个0
        sem_init(&sem_odd, 0, 0);
        sem_init(&sem_even, 0, 0);     
    }

    // printNumber(x) outputs "x", where x is an integer.
    void zero(function<void(int)> printNumber) {
         for(int i = 1; i <= n; i++) {
            sem_wait(&sem_zero);
            printNumber(0);
            if (i & 1) sem_post(&sem_odd);
            else sem_post(&sem_even);
        }
    }

    void even(function<void(int)> printNumber) {
        for (int i = 1; i <= n/2; i++) {
            sem_wait(&sem_even);
            printNumber(i*2);
            sem_post(&sem_zero);
        }
    }

    void odd(function<void(int)> printNumber) {
        for (int i = 1; i <= n-n/2; i++) {
            sem_wait(&sem_odd);
            printNumber((i-1)*2+1);
            sem_post(&sem_zero);
        }
    }
};

```

###  [1195. 交替打印字符串](https://leetcode-cn.com/problems/fizz-buzz-multithreaded/)
>
> 难度中等47收藏分享切换为英文接收动态反馈
>
> 编写一个可以从 1 到 n 输出代表这个数字的字符串的程序，但是：
>
>     如果这个数字可以被 3 整除，输出 "fizz"。
>     如果这个数字可以被 5 整除，输出 "buzz"。
>     如果这个数字可以同时被 3 和 5 整除，输出 "fizzbuzz"。
>
> 例如，当 n = 15，输出： 1, 2, fizz, 4, buzz, fizz, 7, 8, fizz, buzz, 11, fizz, 13, 14, fizzbuzz。
>
> 假设有这么一个类：
>
> class FizzBuzz {
>   public FizzBuzz(int n) { ... }               // constructor
>   public void fizz(printFizz) { ... }          // only output "fizz"
>   public void buzz(printBuzz) { ... }          // only output "buzz"
>   public void fizzbuzz(printFizzBuzz) { ... }  // only output "fizzbuzz"
>   public void number(printNumber) { ... }      // only output the numbers
> }
>
> 请你实现一个有四个线程的多线程版  FizzBuzz， 同一个 FizzBuzz 实例会被如下四个线程使用：
>
>     线程A将调用 fizz() 来判断是否能被 3 整除，如果可以，则输出 fizz。
>     线程B将调用 buzz() 来判断是否能被 5 整除，如果可以，则输出 buzz。
>     线程C将调用 fizzbuzz() 来判断是否同时能被 3 和 5 整除，如果可以，则输出 fizzbuzz。
>     线程D将调用 number() 来实现输出既不能被 3 整除也不能被 5 整除的数字。
>
>  
>
>
> 提示：
>
>     本题已经提供了打印字符串的相关方法，如 printFizz() 等，具体方法名请参考答题模板中的注释部分。
>
> 来源：力扣（LeetCode）
> 链接：https://leetcode-cn.com/problems/fizz-buzz-multithreaded
> 著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。

这是个假并行，所以now不需要加锁

```c++
#include <semaphore.h>

class FizzBuzz {
private:
    int n, now;
    sem_t sem_fizz;
    sem_t sem_buzz;
    sem_t sem_fizz_buzz;
    sem_t sem_num;
public:
    FizzBuzz(int n) {
        this->n = n;
        this->now = 1;
        sem_init(&sem_fizz, 0, 0);
        sem_init(&sem_buzz, 0, 0);
        sem_init(&sem_fizz_buzz, 0, 0);
        sem_init(&sem_num, 0, 1);
    }

    void next() {
        now++;
        if (now == n+1) {
            sem_post(&sem_fizz_buzz);
            sem_post(&sem_buzz);
            sem_post(&sem_fizz);
            sem_post(&sem_num);
            return;
        }
        bool a = now%3==0;
        bool b = now%5==0;
        if (a && b) sem_post(&sem_fizz_buzz);
        else if (!a && b) sem_post(&sem_buzz);
        else if (a && !b) sem_post(&sem_fizz);
        else sem_post(&sem_num);
    }
    // printFizz() outputs "fizz".
    void fizz(function<void()> printFizz) {
        while(now <= n) {
            sem_wait(&sem_fizz);
            if(now > n) break;
            printFizz();
            this->next();
        }
    }

    // printBuzz() outputs "buzz".
    void buzz(function<void()> printBuzz) {
        while(now <= n) {
            sem_wait(&sem_buzz);
            if(now > n) break;
            printBuzz();
            this->next();
        }
    }

    // printFizzBuzz() outputs "fizzbuzz".
	void fizzbuzz(function<void()> printFizzBuzz) {
        while(now <= n) {
            sem_wait(&sem_fizz_buzz);
            if(now > n) break;
            printFizzBuzz();
            this->next();
        }
    }

    // printNumber(x) outputs "x", where x is an integer.
    void number(function<void(int)> printNumber) {
        while(now <= n) {
            sem_wait(&sem_num);
            if(now > n) break;
            printNumber(now);
            this->next();
        }
    }
};
```

### [1117. H2O 生成](https://leetcode-cn.com/problems/building-h2o/)

> 现在有两种线程，氧 oxygen 和氢 hydrogen，你的目标是组织这两种线程来产生水分子。
>
> 存在一个屏障（barrier）使得每个线程必须等候直到一个完整水分子能够被产生出来。
>
> 氢和氧线程会被分别给予 releaseHydrogen 和 releaseOxygen 方法来允许它们突破屏障。
>
> 这些线程应该三三成组突破屏障并能立即组合产生一个水分子。
>
> 你必须保证产生一个水分子所需线程的结合必须发生在下一个水分子产生之前。
>
> 换句话说:
>
> 如果一个氧线程到达屏障时没有氢线程到达，它必须等候直到两个氢线程到达。
> 如果一个氢线程到达屏障时没有其它线程到达，它必须等候直到一个氧线程和另一个氢线程到达。
> 书写满足这些限制条件的氢、氧线程同步代码。
>
>  
>
> 来源：力扣（LeetCode）
> 链接：https://leetcode-cn.com/problems/building-h2o
> 著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。

这个参考了题解区大佬的思路，2个h,1个o是第一层限制，通过第一层限制后，如果是h,告诉半个o可以通过第二次限制，如果是o,告诉2个h可以通过第二层限制。

```c++
#include <semaphore.h>

class H2O {
    sem_t hjob, ojob;
    sem_t hlimit, olimit;
public:
    H2O() {
        sem_init(&hjob,0,0);// H 反应条件信号量
        sem_init(&ojob,0,0);// O 反应条件信号量
        sem_init(&hlimit,0,2);// H线程信号量
        sem_init(&olimit,0,1);// O线程信号量
    }

    void hydrogen(function<void()> releaseHydrogen) {
        sem_wait(&hlimit);// 保证只有2个H线程进入执行
        sem_post(&hjob);// 释放H原子到达信号
        sem_wait(&ojob);// 等待O原子到达
        // releaseHydrogen() outputs "H". Do not change or remove this line.
        releaseHydrogen();        
        sem_post(&hlimit);// 相当于唤醒1个H线程
    }

    void oxygen(function<void()> releaseOxygen) {
        sem_wait(&olimit);// 保证只有1个O线程进入执行
        sem_post(&ojob);// 释放O原子到达信号，因为有2个H线程等待所以释放2个
        sem_post(&ojob);
        sem_wait(&hjob);// 等待H原子到达，2个原因同上
        sem_wait(&hjob);
        // releaseOxygen() outputs "O". Do not change or remove this line.
        releaseOxygen();
        sem_post(&olimit);// 相当于唤醒1个O线程
    }
};
```

### [1226. 哲学家进餐](https://leetcode-cn.com/problems/the-dining-philosophers/)

> 5 个沉默寡言的哲学家围坐在圆桌前，每人面前一盘意面。叉子放在哲学家之间的桌面上。（5 个哲学家，5 根叉子）
>
> 所有的哲学家都只会在思考和进餐两种行为间交替。哲学家只有同时拿到左边和右边的叉子才能吃到面，而同一根叉子在同一时间只能被一个哲学家使用。每个哲学家吃完面后都需要把叉子放回桌面以供其他哲学家吃面。只要条件允许，哲学家可以拿起左边或者右边的叉子，但在没有同时拿到左右叉子时不能进食。
>
> 假设面的数量没有限制，哲学家也能随便吃，不需要考虑吃不吃得下。
>
> 设计一个进餐规则（并行算法）使得每个哲学家都不会挨饿；也就是说，在没有人知道别人什么时候想吃东西或思考的情况下，每个哲学家都可以在吃饭和思考之间一直交替下去。
>
> 问题描述和图片来自维基百科 wikipedia.org
>
>  
>
> 哲学家从 0 到 4 按 顺时针 编号。请实现函数 void wantsToEat(philosopher, pickLeftFork, pickRightFork, eat, putLeftFork, putRightFork)：
>
>     philosopher 哲学家的编号。
>     pickLeftFork 和 pickRightFork 表示拿起左边或右边的叉子。
>     eat 表示吃面。
>     putLeftFork 和 putRightFork 表示放下左边或右边的叉子。
>     由于哲学家不是在吃面就是在想着啥时候吃面，所以思考这个方法没有对应的回调。
>
> 给你 5 个线程，每个都代表一个哲学家，请你使用类的同一个对象来模拟这个过程。在最后一次调用结束之前，可能会为同一个哲学家多次调用该函数。
>
> 来源：力扣（LeetCode）
> 链接：https://leetcode-cn.com/problems/the-dining-philosophers
> 著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。

只有同时最多4人就餐，就不可能循环等待

```c++
#include<semaphore.h>
//#include<pthread.h>

class DiningPhilosophers {
    sem_t sem, fork[5];
    //pthread_mutex_t fork[5];
public:
    DiningPhilosophers() {
        sem_init(&sem, 0, 3);
        for(int i = 0; i < 5; i++)
            sem_init(&fork[i], 0, 1);
            //pthread_mutex_init(fork+i, nullptr);
    }

    void wantsToEat(int philosopher,
                    function<void()> pickLeftFork,
                    function<void()> pickRightFork,
                    function<void()> eat,
                    function<void()> putLeftFork,
                    function<void()> putRightFork) {

		int right=philosopher;
		int left=(right+1)%5;
		
		sem_wait(&sem);
		
		//pthread_mutex_lock(fork+left);
		//pthread_mutex_lock(fork+right);
		sem_wait(&fork[left]);
		sem_wait(&fork[right]);

		pickLeftFork();
		pickRightFork();
		eat();
		putLeftFork();
		putRightFork();
		
        sem_post(&fork[left]);
        sem_post(&fork[right]);
		//pthread_mutex_unlock(fork+left);
		//pthread_mutex_unlock(fork+right);
		
		sem_post(&sem);
    }
};

```

