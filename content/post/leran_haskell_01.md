---
title: Haskell 常用函数笔记
date: 2021-06-21
math: false
tags:  ["Haskell"]
---



### 运算符函数

###### (!!) 取第n个元素

```haskell
Prelude> :t (!!)
(!!) :: [a] -> Int -> a

Prelude> [0,1,2,3,4]!!3
3
Prelude> [0,1,2,3,4]!!0
0
```

###### 乘方 (^), (^^),(**)

```haskell
Prelude> :t (^)
(^) :: (Integral b, Num a) => a -> b -> a
Prelude> 54.5^5
4.8081998590625e8

Prelude> :t (^^)
(^^) :: (Fractional a, Integral b) => a -> b -> a
Prelude> 5.6^^6
30840.979455999986

Prelude> :t (**)
(**) :: Floating a => a -> a -> a
Prelude> 0.2**0.1
0.8513399225207846
```

<!--more-->

###### 取余函数

```haskell
Prelude> mod (-2) 4
2
Prelude> rem (-2) 4
-2
```

###### 求商函数

```haskell
Prelude> div (-5) 3
-2
Prelude> quot (-5) 3
-1
```

###### (:)  连接元素与列表

```haskell
Prelude> 1:[2,3]
[1,2,3]
```

###### (++) 连接两个列表

```haskell
Prelude> [1,2,3]++[4,5,6]
[1,2,3,4,5,6]
```

###### ($) 改变运算优先级

($) 有最低优先级，并且右结合

```haskell
Prelude> gcd 4 $ mod 5 6 
1
```


### 预加载库(Prelude)函数



##### 恒指函数 id

```haskell
Prelude> :t id
id :: a -> a
Prelude> id 2333.3
2333.3
```

###### 常值函数 const

返回俩个参数的第一个

```haskell
Prelude> :t const
const :: a -> b -> a
Prelude> const "434" 5
"434"
```

###### 参数反置函数flip

将二元函数参数顺序颠倒

```haskell
Prelude> :t flip
flip :: (a -> b -> c) -> b -> a -> c
Prelude> flip const 1 2
2
```

###### 错误函数error

```haskell
Prelude> :t error
error :: [Char] -> a
Prelude> a = error "error case a"
Prelude> a
*** Exception: error case a
CallStack (from HasCallStack):
  error, called at <interactive>:28:5 in interactive:Ghci11
```

###### undefined

```haskell
Prelude> :t undefined
undefined :: a
Prelude> undefined
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:31:1 in interactive:Ghci12
```

###### min,max 取最值

```haskell
Prelude> :t min
min :: Ord a => a -> a -> a
Prelude> :t max
max :: Ord a => a -> a -> a

Prelude> max 42 (-4) --注意括号
42
Prelude> max 2.1 4.5
4.5
```

### 基于列表的函数

###### null 判断是否为空

```haskell
Prelude> :t null
null :: Foldable t => t a -> Bool
Prelude> null []
True
Prelude> null [[]]
False
```

###### length 长度

```haskell
Prelude> :t length
length :: Foldable t => t a -> Int
Prelude> length []
0
Prelude> length [1,3,2,-2]
4
```

###### reverse 翻转列表

```haskell
Prelude> :t reverse
reverse :: [a] -> [a]
Prelude> reverse [0,1,2,3,4]
[4,3,2,1,0]
```

###### head,last 

```haskell
Prelude> :t head
head :: [a] -> a

Prelude> head [1,2,3]
1
Prelude> :t last
last :: [a] -> a
Prelude> last [1,2,3]
3
```

###### init,tail

```haskell
Prelude> :t init 
init :: [a] -> [a]
Prelude> init [1,2,3]
[1,2]

Prelude> :t tail
tail :: [a] -> [a]
Prelude> tail [1,2,3]
[2,3]
```

###### map 将一个函数应用到列表每个元素

```haskell
Prelude> :t map
map :: (a -> b) -> [a] -> [b]
Prelude> map (+1) [1,2,3,4]
[2,3,4,5]
Prelude> map (1+) [1,2,3,4]
[2,3,4,5]
Prelude> map (\x -> x*2+1) [1,2,3,4]
[3,5,7,9]
```

###### filter 过滤出满足条件的元素

```haskell
Prelude> :t filter
filter :: (a -> Bool) -> [a] -> [a]
Prelude> filter odd [1,2,3,4,5,6]
[1,3,5]
```

###### take,drop 从头、尾连续取n个元素

```haskell
Prelude> :t take
take :: Int -> [a] -> [a]
Prelude> take 6 [1..]
[1,2,3,4,5,6]

Prelude> :t drop
drop :: Int -> [a] -> [a]
Prelude> drop 2 [1,2,3,4]
[3,4]
```

###### span,break 

```haskell
span :: (a -> Bool) -> [a] -> ([a], [a])
Prelude> span even [2,4,6,1,2,3,5,7]
([2,4,6],[1,2,3,5,7])

break :: (a -> Bool) -> [a] -> ([a], [a])

Prelude> break odd [2,4,6,1,2,3,5,7]
([2,4,6],[1,2,3,5,7])
```
###### takeWhile,dropWhile

```haskell

takeWhile :: (a -> Bool) -> [a] -> [a]
Prelude> takeWhile (<7) [1,2,3,5,6,7,8,8]
[1,2,3,5,6]

dropWhile :: (a -> Bool) -> [a] -> [a]
Prelude> dropWhile (<7) [1,2,3,5,6,7,8,8]
[7,8,8]
```
###### splitAt 将列表在任何位置分开

```haskell

Prelude> :t splitAt
splitAt :: Int -> [a] -> ([a], [a])
Prelude> splitAt 4 "abcdef"
("abcd","ef")

Prelude> splitAt 4 [1,2,3,4,5,6]
([1,2,3,4],[5,6])
```
###### 重复函数repeat, 复制函数replicate 

```haskell
repeat :: a -> [a]
Prelude> take 5 $ repeat 6
[6,6,6,6,6]

replicate :: Int -> a -> [a]
Prelude> replicate 5 6
[6,6,6,6,6]
```
###### any,all

```haskell
any :: Foldable t => (a -> Bool) -> t a -> Bool
Prelude> any even [1,2,3]
True
Prelude> any even [1,3]
False

all :: Foldable t => (a -> Bool) -> t a -> Bool
Prelude> all even [2,4,6,1]
False
```
###### elem,notElem

```haskell
elem :: (Foldable t, Eq a) => a -> t a -> Bool
Prelude> elem 3 [1,2,3]
True
Prelude> elem 3 [1,2,4]
False

notElem :: (Foldable t, Eq a) => a -> t a -> Bool
Prelude> notElem 3 [1,2,3]
False
```
###### iterate 把一个函数对一个元素重复应用无数次

```haskell
iterate :: (a -> a) -> a -> [a]
Prelude> take 10 $ iterate (*2) 1
[1,2,4,8,16,32,64,128,256,512]
```
###### until 把一个函数对一个元素重复应用至满足条件

```haskell
until :: (a -> Bool) -> (a -> a) -> a -> a
Prelude> until (>1024) (*2) 1
2048
```
###### zip 把两个列表合成一个元组的列表

```haskell
zip :: [a] -> [b] -> [(a, b)]
Prelude> zip [1,2,3,4] "abcd"
[(1,'a'),(2,'b'),(3,'c'),(4,'d')]
```
###### concat 将列表中的列表相连
```haskell
concat :: Foldable t => t [a] -> [a]
Prelude> concat [[1,2],[3],[4,5,6]]
[1,2,3,4,5,6]
```
###### concatMap

```haskell
concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
Prelude> map (replicate 2) [1,2,3]
[[1,1],[2,2],[3,3]]
Prelude> concatMap (replicate 2) [1,2,3]
[1,1,2,2,3,3]
```

### 字符串处理

###### show 转化为字符串

```haskell
show :: Show a => a -> String
Prelude> show True
"True"
Prelude> show 33.3
"33.3"
```

###### read 从字符串读取，需要注明类型

```haskell
read :: Read a => String -> a
Prelude> (\str->read str::Int) "2333"
2333
```

###### lines,unlines 以换行分开

```haskell
lines :: String -> [String]
Prelude> lines "abc\nccc\ngt"
["abc","ccc","gt"]

unlines :: [String] -> String
Prelude> unlines ["a","b"]
"a\nb\n"
```

###### word,unword 以空格分开

```haskell
words :: String -> [String]
Prelude> words "hi,I am Li Huan\nline2"
["hi,I","am","Li","Huan","line2"]

unwords :: [String] -> String
Prelude> unwords ["1","2","cd"]
"1 2 cd"
```