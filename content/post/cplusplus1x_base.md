+++

title = "初步认识c++1x的新特性"
date = "2020-08-19"
description = "c++11"
tags = [
    "c++"
]
noToc = true

+++

### (1) nullptr

用<font color='red'> nullptr </font>代替NULL,

NULL是一个宏定义，在c和c++中的定义不同，c中NULL为（void*)0,而c++中NULL为整数0

```cpp
void foo(char *);
void foo(int);
```

对于这两个函数,如果NULL定义为0的话,foo(NULL)将会出现歧义

使用NULL的情景均可用<font color='red'> nullptr </font>代替

### (2) constexpr

<font color='red'> constexpr </font>让用户显式的声明函数或对象构造函数在编译器为常数

<font color='red'> constexpr </font> 的函数可以使用递归，从 C++ 14 开始，<font color='red'> constexpr </font>函数可以在内部使用局部变量、循环和分支等简单语句，但 C++ 11 中是不可以的

<!--more-->

```cpp
constexpr int two() {
    return 2;
}

int _two() {
    return 2;
}

constexpr int fib(int n) {
    return n == 1 || n == 2 ? 1 : fib(n-1)+fib(n-2);
}

int main() {
    int a[two()];
    //int _a[_two()];  //编译出错
    int b[fib(5)];
    int x;
    cin >> x;
    int c[fib(x)];//错误用法，constexpr必须编译期可求
    cout << sizeof (c) / sizeof (int);
    return 0;
}
```

<font color='red'> constexpr </font>还能用于修饰类的构造函数，即保证如果提供给该构造函数的参数都是<font color='red'> constexpr </font>，那么产生的对象中的所有成员都会是<font color='red'> constexpr </font>，该对象也就是<font color='red'> constexpr </font>对象了，可用于各种只能使用<font color='red'> constexpr </font>的场合。注意，<font color='red'> constexpr </font>构造函数必须有一个空的函数体，即所有成员变量的初始化都放到初始化列表中。

```cpp
struct A {
    constexpr A(int xx, int yy): x(xx), y(yy) {}
    int x, y;
};
 
constexpr A a(1, 2);
enum {SIZE_X = a.x, SIZE_Y = a.y};
```

#### [C++中的const和constexpr ](https://www.cnblogs.com/fuzhe1989/p/3554345.html)

### (3) auto

 <font color='red'> auto </font> 的意义是使C++编译器可以在编译时推导数据类型，这样就不用每次都要声明数据类型了.

注意 <font color='red'> auto </font> 不能用于函数传参以及推导数组类型

```cpp
vector<int> vec{1,2,3};
for (auto it = vec.begin(); it != vec.end(); ++it)
    cout << *it << " ";//迭代器
auto n = 10;//int
auto x = new auto(12); //int*
```

### (4) decltype

有时我们希望从表达式的类型推断出要定义的变量类型，但是不想用该表达式的值初始化变量（初始化可以用<font color='red'> auto </font>）。为了满足这一需求，C++11新标准引入了<font color='red'> decltype </font> 类型说明符，它的作用是选择并返回操作数的数据类型，在此过程中，编译器分析表达式并得到它的类型，却不实际计算表达式的值。

```cpp
auto x = 1;
auto y = 2;
decltype(x+y) z = 0;   // z 是一个 int 型的
```

#### **[C++11新标准学习：decltype关键字](https://www.cnblogs.com/ghbjimmy/p/10636030.html)**



### (5)尾返回类型



```cpp
template<typename T, typename U>
auto add(T x, U y) -> decltype(x+y) {
    return x+y;
}
```

C++ 14 开始是可以直接让普通函数具备返回值推导

```cpp
template<typename T, typename U>
auto add(T x, U y) {
    return x+y;
}
```

### (6)基于范围的 for 循环

```cpp
int array[] = {1,2,3,4,5};
    for(auto &x : array) {
        std::cout << x << std::endl;
        x = -x;
    }
    for(auto x : array) {
        std::cout << x << std::endl;
    }
```

### (7)初始化列表 

```cpp
    int a[3]{1,2,3}; //统一的初始化语法
    vector<int> vetc{1,2,3};
    set<int> f{1,1,2,3};
```

### (8) 外部模板

传统 C++ 中，模板只有在使用时才会被编译器实例化。换句话说，只要在每个编译单元（文件）中编译的代码中遇到了被完整定义的模板，都会实例化。这就产生了重复实例化而导致的编译时间的增加。并且，我们没有办法通知编译器不要触发模板实例化。

```cpp
// test1.cpp
#include "test.h"
template void fun<int>(int); // 显式地实例化 
void test1()
{ 
    fun(1);
}
```

```cpp
// test2.cpp
#include "test.h"
extern template void fun<int>(int); // 外部模板的声明
void test2()
{
    fun(2);
}
```

### (9) 尖括号 ">"

```cpp
std::vector<std::vector<int>> wow;
```

两个>相连不用加空格了。

### (10) 类型别名模板

```cpp
template< typename T, typename U, int value>
class SuckType {
public:
    T a;
    U b;
    SuckType():a(value),b(value){}
};
template< typename U>

using NewType = SuckType<std::vector<int>, U, 1>;
NewType<int> Tmp;
```

```cpp
typedef int (*process)(void *);  // 定义了一个返回类型为 int，参数为 void* 的函数指针类型，名字叫做 process
using process = int(*)(void *); // 同上, 更加直观
```

### (11)默认模板参数

```cpp
template<typename T = int, typename U = int>
auto add(T x, U y) -> decltype(x+y) {
    return x+y;
}
```

### (12)变长参数模板

允许任意个数、任意类别的模板参数，同时也不需要在定义时将参数的个数固定。

```cpp
template<typename... Ts> class Magic;
```

模板类 Magic 的对象，能够接受不受限制个数的 typename 作为模板的形式参数，例如下面的定义：

```cpp
class Magic<int,
            std::vector<int>,
            std::map<std::string,
                     std::vector<int>>> darkMagic;
```

既然是任意形式，所以个数为 0 的模板参数也是可以的：`class Magic<> nothing;`。

除了在模板参数中能使用 `...` 表示不定长模板参数外，函数参数也使用同样的表示法代表不定长参数，这也就为我们简单编写变长参数函数提供了便捷的手段，例如：

```cpp
template<typename... Args> void printf(const std::string &str, Args... args);
```

那么我们定义了变长的模板参数，如何对参数进行解包呢？

首先，我们可以使用 `sizeof...` 来计算参数的个数，：

```cpp
template<typename... Args>
void magic(Args... args) {
    std::cout << sizeof...(args) << std::endl;
}
```

我们可以传递任意个参数给 `magic` 函数：

```cpp
magic();        // 输出0
magic(1);       // 输出1
magic(1, "");   // 输出2
```

其次，对参数进行解包，到目前为止还没有一种简单的方法能够处理参数包，但有两种经典的处理手法：

**1. 递归模板函数**

递归是非常容易想到的一种手段，也是最经典的处理方法。这种方法不断递归的向函数传递模板参数，进而达到递归遍历所有模板参数的目的：

```cpp
#include <iostream>
template<typename T>
void printf(T value) {
    std::cout << value << std::endl;
}
template<typename T, typename... Args>
void printf(T value, Args... args) {
    std::cout << value << std::endl;
    printf(args...);
}
int main() {
    printf(1, 2, "123", 1.1);
    return 0;
}
```

**2. 初始化列表展开**

递归模板函数是一种标准的做法，但缺点显而易见的在于必须定义一个终止递归的函数。

这里介绍一种使用初始化列表展开的黑魔法：

```cpp
// 编译这个代码需要开启 -std=c++14
template<typename T, typename... Args>
auto print(T value, Args... args) {
    std::cout << value << std::endl;
    return std::initializer_list<T>{([&] {
        std::cout << args << std::endl;
    }(), value)...};
}
int main() {
    print(1, 2.1, "123");
    return 0;
}
```

在这个代码中，额外使用了 C++11 中提供的初始化列表以及 Lambda 表达式的特性，而 std::initializer_list 也是 C++11 新引入的容器。

通过初始化列表，`(lambda 表达式, value)...` 将会被展开。由于逗号表达式的出现，首先会执行前面的 lambda 表达式，完成参数的输出。唯一不美观的地方在于如果不使用 `return` 编译器会给出未使用的变量作为警告。

### (13)面向对象增强    

#### （1）委托构造

```cpp
class Base {
public:
    int value1;
    int value2;
    Base() {
        value1 = 1;
    }
    Base(int value) : Base() {  // 委托 Base() 构造函数
        value2 = 2;
    }
};

int main() {
    Base b(2);
    std::cout << b.value1 << std::endl;
    std::cout << b.value2 << std::endl;
}
```

#### （2）继承构造

```cpp
class Base {
public:
    int value1;
    int value2;
    Base() {
        value1 = 1;
    }
    Base(int value) : Base() {   // 委托 Base() 构造函数
        value2 = 2;
    }
};
class Subclass : public Base {
public:
    using Base::Base;  // 继承构造
};
int main() {
    Subclass s(3);
    std::cout << s.value1 << std::endl;
    std::cout << s.value2 << std::endl;
}
```

#### （3）显式虚函数重载

C++ 11 引入了 `override` 和 `final` 这两个关键字来防止上述情形的发生。

当重载虚函数时，引入 `override` 关键字将显式的告知编译器进行重载，编译器将检查基函数是否存在这样的虚函数，否则将无法通过编译：

```cpp
struct Base {
    virtual void foo(int);
};
struct SubClass: Base {
    virtual void foo(int) override; // 合法
    virtual void foo(float) override; // 非法, 父类没有此虚函数
};
```

`final` 则是为了防止类被继续继承以及终止虚函数继续重载引入的。

```cpp
struct Base {
        virtual void foo() final;
};
struct SubClass1 final: Base {
};                  // 合法

struct SubClass2 : SubClass1 {
};                  // 非法, SubClass 已 final

struct SubClass3: Base {
        void foo(); // 非法, foo 已 final
};
```

#### （6）显式禁用默认函数

在传统 C++ 中，如果程序员没有提供，编译器会默认为对象生成默认构造函数、复制构造、赋值算符以及析构函数。另外，C++ 也为所有类定义了诸如 `new` `delete` 这样的运算符。

```cpp
class Magic {
public:
    Magic() = default;  // 显式声明使用编译器生成的构造
    Magic& operator=(const Magic&) = delete; // 显式声明拒绝编译器生成构造
    Magic(int magic_number);
}
```

###  (14)强类型枚举    

C++ 11 引入了枚举类（enumaration class），并使用 `enum class` 的语法进行声明：

```cpp
enum class new_enum : unsigned int {
    value1,
    value2,
    value3 = 100,
    value4 = 100
};
```

这样定义的枚举实现了类型安全，首先他不能够被隐式的转换为整数，同时也不能够将其与整数数字进行比较，更不可能对不同的枚举类型的枚举值进行比较。但相同枚举值之间如果指定的值相同，那么可以进行比较。

### (15) Lambda 表达式

C++ 11 中的 Lambda 表达式用于定义并创建匿名的函数对象，以简化编程工作。
Lambda 的语法形式如下：

```cpp
[函数对象参数] (操作符重载函数参数) mutable 或 exception 声明 -> 返回值类型 {函数体}
```

####  函数对象参数

标识一个 Lambda 表达式的开始，这部分必须存在，不能省略。函数对象参数是传递给编译器自动生成的函数对象类的构造函数的。函数对象参数只能使用那些到定义 Lambda 为止时 Lambda 所在作用范围内可见的局部变量 (包括 Lambda 所在类
的 this)。函数对象参数有以下形式：

- 空。没有任何函数对象参数。
- =。函数体内可以使用 Lambda 所在范围内所有可见的局部变量（包括 Lambda 所在类的 this），并且是值传递方式（相当于编译器自动为我们按值传递了所有局部变量）。
- &。函数体内可以使用 Lambda 所在范围内所有可见的局部变量（包括 Lambda 所在类的 this），并且是引用传递方式（相当于是编译器自动为我们按引用传递了所有局部变量）。
- this。函数体内可以使用 Lambda 所在类中的成员变量。
- a。将 a 按值进行传递。按值进行传递时，函数体内不能修改传递进来的 a 的拷贝，因为默认情况下函数是 const 的，要修改传递进来的拷贝，可以添加 mutable 修饰符。
- &a。将 a 按引用进行传递。
- a，&b。将 a 按值传递，b 按引用进行传递。
- =，&a，&b。除 a 和 b 按引用进行传递外，其他参数都按值进行传递。
- &，a，b。除 a 和 b 按值进行传递外，其他参数都按引用进行传递。

#### 操作符重载函数参数

标识重载的 () 操作符的参数，没有参数时，这部分可以省略。参数可以通过按值（如: (a, b)）和按引用 (如: (&a, &b)) 两种方式进行传递。

#### mutable 或 exception 声明

这部分可以省略。按值传递函数对象参数时，加上 mutable 修饰符后，可以修改传递进来的拷贝（注意是能修改拷贝，而不是值本身）。exception 声明用于指定函数抛出的异常，如抛出整数类型的异常，可以使用 throw (int)。

#### -> 返回值类型

标识函数返回值的类型，当返回值为 void，或者函数体中只有一处 return 的地方（此时编译器可以自动推断出返回值类型）时，这部分可以省略。

#### 函数体

标识函数的实现，这部分不能省略，但函数体可以为空。

#### 实例

```cpp
[] (int x, int y) { return x + y; } // 隐式返回类型
[] (int& x) { ++x;} // 没有 return 语句 -> Lambda 函数的返回类型是 'void'
[] () { ++global_x;  } // 没有参数，仅访问某个全局变量
[] { ++global_x; } // 与上一个相同，省略了 (操作符重载函数参数)
```

[Lambda参考博客](www.baidu.com/link?url=gmSPVa8wBs-YawFgG3RQPsIDReUkHR3HoqjeYKElDAeW6AE17Y1_IbAa44kDW5_ar0WAFbAPQLeiMjuEkQP4ra&wd=&eqid=b9b6451b0004bc9e00000003600af1ed)