---
title: goroutine 的学习之旅
date: 2021-04-03
math: true
tags:  ["go", "goroutine", "并发"]
---
回想起来，我是因为对 goroutine 有兴趣而去了解 Go 的，因为 finger tree 而想学 Haskell，结果坑留了半年了（另一个坑留了快3年，Monad 还没弄明白），今天有时间先填上一个再说。

goroutine，Go 语言的协程，是一种比线程更轻量的存在，一个cpu可以支持上万的协程。Go 可以通过 goroutine 支持并发。

Go使用的是并发模型中的 CSP 模型，Communicating Sequential Processes，译为通信顺序进程、七周七并发第六章内容，这本书继续留坑有空再看。

<!--more-->

### go
在函数调用前使用 go 语句可以开启一个新的 goroutine。
```go
package main

import (
        "fmt"
        "time"
)

func say(s string) {
        for i := 0; i < 5; i++ {
                time.Sleep(100 * time.Millisecond)
                fmt.Println(s)
        }
}

func main() {
        go say("world")
        say("hello")
}
```

可以看到输出hello，world交替出现，程序不再是串行执行了。

```bash
earn_goroutine (main*) » go run a.go
hello
world
hello
world
hello
world
world
hello
hello
```

如果say("hello")前面也加上go呢？

```go
func main() {
        go say("world")
        go say("hello")
}
```

结果程序什么都不会输出！因为main函数执行完了程序就会退出，不会等所有 goroutine 退出。

解决方法就是在 main 函数结尾 sleep 到所有 goroutine 执行完毕。

然而 sleep 多长时间合适，就不太好确定了。 而 sync.WaitGroup 可以解决这个问题，Add 有几个 goroutine 要等待， goroutine 完成后调用 Done，Wait 进行等待直到所有 Add 的 goroutine Done 了。

```go
package main

import (
	"fmt"
	"time"
	"sync"
)

func main() {
	var wg sync.WaitGroup;
	wg.Add(2)
	go func() {
		say("world")
		wg.Done()
	}()
	go func() {
		say("hello")
		wg.Done()
	}()
	wg.Wait()
}

func say(s string) {
	for i := 0; i < 5; i++ {
		time.Sleep(100 * time.Millisecond)
		fmt.Println(s)
	}
}
```

### channel

通道（channel）是用来传递数据的一个数据结构，CSP模型的精髓。

通道可用于两个 goroutine 之间通过传递一个指定类型的值来同步运行和通讯。操作符 `<-` 用于指定通道的方向, 默认则为双向通道

使用chan关键字声明一个 channel。

发送和接收消息是阻塞的，如果没人接收，发送方会阻塞在那里，反之同理。

```go
// 将数组分成两半求和的程序
package main

import "fmt"

func sum(s []int, c chan int) {
	sum := 0
	for _, v := range s {
		sum += v
	}
	c <- sum
}

func main() {
	s := []int{1, 2, 3, 4, 5, 6}
	c := make(chan int)
	go sum(s[:len(s)/2], c)
	go sum(s[len(s)/2:], c)
	x,y := <-c, <-c
	fmt.Println(x, y, x+y)
}
```

通过 `close(c)` 可以关闭 channel,  `c<-` 的第二个参数表示 channel 的开闭。
```go
for {
	message,open := <-c
	if (!open) break
}
```
也可以用 range
```go
for message := range c {
}
```

#### 通道缓冲区
如果没有缓冲区，可能造成发送消息溢出，导致接收方永远收不到溢出的信息，一直阻塞，如下例

```go
package main

import "fmt"

func main() {
        ch := make(chan int)
        ch <- 1
        ch <- 2
        fmt.Println(<-ch)
        fmt.Println(<-ch)
}
```
运行结果
> learn_goroutine (main*) » go run c.go
> fatal error: all goroutines are asleep - deadlock!goroutine 1 [chan send]:
> main.main()
> 	/home/rew/CodeDrafts/learn_goroutine/c.go:12 +0x59
> exit status 2

我们可以通过 make 的第二个参数指定缓冲区大小：
```
ch := make(chan int, 100)
```

带缓冲区的通道允许发送端的数据发送和接收端的数据获取处于异步状态，就是说发送端发送的数据可以放在缓冲区里面，可以等待接收端去获取数据，而不是立刻需要接收端去获取数据。

```go
func main() {
       // 定义了一个可以存储整数类型的带缓冲通道 缓冲区大小为2
        ch := make(chan int, 2)
        ch <- 1
        ch <- 2
        fmt.Println(<-ch)
        fmt.Println(<-ch)
}
```
注意，如果缓冲区不够大，依然会溢出，

### select

select 是 Go 中的一个控制结构，类似于用于通信的 switch 语句。每个 case 必须是一个通信操作，要么是发送要么是接收。

select 随机执行一个可运行的 case。如果没有 case 可运行，它将阻塞，直到有 case 可运行。一个默认的子句应该总是可运行的。

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	c1 := make(chan string)
	c2 := make(chan string)
	go func() {
		for {
			c1 <- "Hello"
			time.Sleep(10 * time.Millisecond)
		}
	} ()
	go func() {
		for {
			c2 <- "World"
			time.Sleep(100 * time.Millisecond)
		}
	} ()
	for {
		fmt.Println(<- c1)
		fmt.Println(<- c2)
	}
}
```

在上例中，"Hello"，"World"交替输出，但是，明显"World"发送频率慢的多，拖了进度
下面，使用 select

``` go
	var a1, a2 string
	for {
		select {
		case a1 = <-c1:
			fmt.Println(a1)
		case a2 = <-c2:
			fmt.Println(a2)
        }
    }
```
输出变为
>World
Hello
Hello
Hello
Hello
Hello
Hello
Hello

不会再被慢的一方拖累了。

### 参考教程

[Golang高并发教程+实战 bilibili](https://www.bilibili.com/video/BV1qT4y1c77u)

[菜鸟教程的 GO](https://www.runoob.com/go/go-concurrent.html)