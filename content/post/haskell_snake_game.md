---
title: "Codeworld: 使用Haskell实现一个贪吃蛇小游戏"
date: "2021-08-20"
math: false
tags:  ["Haskell"]
---

haskell 的一些教学可以参考：

[https://www.seas.upenn.edu/~cis194/fall16/index.html](https://www.seas.upenn.edu/~cis194/fall16/index.html)

CodeWorld：可以在线运行hs代码，基于ghc.js，~~四舍五入还是在本地跑~~

[https://code.world/haskell](https://code.world/haskell#)

下面是利用 CodeWorld 画圆的小例子

```haskell
import CodeWorld

main :: IO()
main = do
       drawingOf $ circle 1
```

<!--more-->

![wishimg](https://cdn.jsdelivr.net/gh/wineee/MarkDownPIC@master/img/73d0de53a33166008f9673fe5976f472.png)

还可以画多边形

```haskell
import CodeWorld

main :: IO ()
main = drawingOf triangle

triangle :: Picture
triangle = polygon [(9,9),(0,-9),(-9,9)]
```

![wishimg](https://cdn.jsdelivr.net/gh/wineee/MarkDownPIC@master/img/7e06ec4a9da1e8b17a91fd75adae136e.png)

通过 colored 函数添加颜色， 参数是: color picture

```haskell
import CodeWorld

main :: IO ()
main = drawingOf ourPicture

ourPicture = colored green $ solidCircle 2
```

![wishimg](https://cdn.jsdelivr.net/gh/wineee/MarkDownPIC@master/img/e2c8030079aab9ed7cd2c7effba5b7f9.png)

& 函数可以将图片合并

```haskell
import CodeWorld

main :: IO ()
main = drawingOf $ ourPicture & (solidCircle 5)

ourPicture = colored green $ solidCircle 2
```

![wishimg](https://cdn.jsdelivr.net/gh/wineee/MarkDownPIC@master/img/1e31b82ac61ce2e4a1d909ecab53d400.png)

translated 函数实现移位，参数dx dy picture

```haskell
import CodeWorld

main :: IO ()
main = drawingOf $ ourPicture & translated 1 2 $ solidCircle 1

ourPicture = colored green $ solidCircle 1
```

![wishimg](https://cdn.jsdelivr.net/gh/wineee/MarkDownPIC@master/img/983d76eadcdbad3600cf0320007000f6.png)

下面实现贪吃蛇，代码参照了[此教程](https://www.bilibili.com/video/BV19v411n7ww)，我添加了一些注释

Haskell 类型系统确实强大，看名字和标注就能知道这个函数有什么用。编译器可以排查大部分错误，可惜我把 unfoldr i+1写成 i-1，无限递归了，找了半天bug（我都不知道怎么debug）

```haskell
import CodeWorld
import Data.List
import Data.Text(pack, unpack)
import System.Random

type RandomNumber = Int
gridSize = 0.5
-- 为了美观，
gridSizeHf = 0.25
-- 坐标（0，0）为中心， 经调整（0，0）的格子占用（0，0）-（0.5，0.5）空间

main :: IO()
main = do
       gen <- getStdGen
       activityOf (initialWorld gen) handleEvent drawWorld
       --         初始化              事件相应     绘制
       
-- 定义游戏状态，GameIdle              
data GameState = GameIdle 
               | GameRunning
               | GameOver
               deriving(Show, Eq)
              
data World = World {
        rnds :: [RandomNumber]
      , state :: GameState
      , snake :: Snake
      , apple :: Apple
      , areaWidth :: Double
      , areaHeight :: Double
      }

initialWorld :: StdGen -> World
initialWorld gen = restartWorld rnds width height
      where rnds = randomRs (round(0-width), round(width-2)) gen
            width = 20 :: Double
            height = 20 :: Double
      
restartWorld :: [RandomNumber] -> Double -> Double -> World
restartWorld rnds w h = World rnds' GameRunning snake apple w h
    where snake = mkSnake (-3, 0) 3 blue
          apple = mkApple ((fromIntegral r1) / 2, (fromIntegral r2) / 2) green
          (r1:r2:rnds') = rnds
 
drawWorld :: World -> Picture
drawWorld world
  | state world == GameOver = draw world
                              & translated 0 (1)
                                (styledLettering Bold Handwriting 
                                  (pack "Game Over"))
                              & translated 0 (-1)
                                (styledLettering Bold Handwriting 
                                  (pack score_str))
  | otherwise = draw world
  where draw world = drawSnake (snake world)
                   & drawApple (apple world)
                   & rectangle (areaWidth world) (areaHeight world)
        score_str = "Playing Score is: " ++ (show $ score $ snake world)

-- 蛇的方向
data Direction = DirectUp
               | DirectDown
               | DirectLeft
               | DirectRight
               deriving(Show, Eq, Ord)
-- 蛇的动作
data SnakeAction = SnakeNoAct
                 | SnakeMove
                 | SnakeEat
                 | SnakeDead
                 deriving(Show, Eq)
-- 定义蛇 
data Snake = Snake {
    bodyPoints :: [Point]
  , ds :: Double
  , score :: Double
  , direct :: Direction
  , color :: Color
  , width :: Double
 }
 
-- 初始化蛇 
mkSnake :: Point -> Int -> Color -> Snake
mkSnake startPoint len color = Snake body 0 0 DirectUp color w
  -- unfoldr 展开函数，从一个点生成其他点，直到产生Nothing
  where body = unfoldr (\\i -> if i < len 
                              then Just((x, y - w * fromIntegral i), i+1) 
                              else Nothing) 
                        0 -- 初始值
        (x, y) = startPoint
        w = gridSize
--  foldr1 相当于 foldr 起始值为列表第一个值
--  . 函数与数学定义相似，f.g(x) 相当于 g(f(x))
drawSnake :: Snake -> Picture
drawSnake snake = foldr1 (&) blks 
  where blks = map ((colored snkColor) . drawBodyBlk) snkBody
        snkColor = color snake
        snkBody = bodyPoints snake
        drawBodyBlk (x, y) = translated (x + gridSizeHf) (y + gridSizeHf)
                           $ solidRectangle (w - 0.05) (w - 0.05)
        -- solidRectangle 画图默认中心（0，0）， translated 加上 gridSizeHf 使其正好对应方格
        w = width snake

-- 蛇转向
turnSnake :: Direction -> Snake -> Snake
turnSnake dir snake = if isConflictDirect dir (direct snake)
                         then snake
                         else snake { direct = dir }
                         where isConflictDirect dir1 dir2
                                 | dir1 == DirectUp && dir2 == DirectDown = True
                                 | dir1 == DirectDown && dir2 == DirectUp = True
                                 | dir1 == DirectRight && dir2 == DirectLeft = True
                                 | dir1 == DirectLeft && dir2 == DirectRight = True
                                 | otherwise = False
-- 蛇前进一个单位  
moveSnake :: Snake -> Snake
moveSnake snake = snake { ds = 0, bodyPoints = pts }
                  where ptsOrg = bodyPoints snake
                        pts = pt : init ptsOrg -- 去尾加头
                        pt = translatedPoint dx dy $ head ptsOrg
                        (dx, dy) = getSnakeDxDy snake
-- 按当前方向蛇头的移动dx，dy                        
getSnakeDxDy snake@(Snake _ _ _ direct _ w)
  | direct == DirectUp = (0, w)
  | direct == DirectDown = (0, -w)
  | direct == DirectLeft = (-w, 0)
  | direct == DirectRight = (w, 0)

-- 吃苹果
eatingSnake :: Snake -> Snake
eatingSnake snake = snake { ds = 0, score = score', bodyPoints = pts}
                      where ptsOrg = bodyPoints snake
                            pts = pt : ptsOrg
                            pt = translatedPoint dx dy $ head ptsOrg
                            (dx, dy) = getSnakeDxDy snake
                            score' = score snake + 1
                                                
data Apple = Apple {
    positionA :: Point
  , colorA :: Color
  , widthA :: Double
    }
mkApple :: Point -> Color -> Apple
mkApple pos color = Apple pos color 0.25

-- 显示苹果
drawApple :: Apple -> Picture
drawApple apple@(Apple pos color width)
    = translated (x + gridSizeHf) (y + gridSizeHf)
      $ colored color
      $ solidCircle width
      where (x, y) = pos

-- 随机生成苹果     
randowApple :: World ->  (Apple, [RandomNumber])
randowApple world@(World rnds _ _ apple w h) = (mkApple pos color, rnds')
  where pos = ((fromIntegral r1) / 2, (fromIntegral r2) /2)
        color = if colorOrg == red then green else red
        (r1:r2:rnds') = rnds
        colorOrg = colorA apple
        
handleEvent :: Event -> World -> World

-- 计时事件
handleEvent (TimePassing dt) w
  | state w == GameOver = w
  | otherwise = handleSnakeAction snake' action 
                $ w
  where (snake', action) = checkSnakeAction dt w (snake w)

-- 按键事件
handleEvent (KeyPress keyText) w
  | state w == GameOver
     && unpack keyText == "Enter" = restartWorld (rnds w) areaW areaH
  | unpack keyText == "Down" = w { snake = turnSnake DirectDown snake' }
  | unpack keyText == "Up" = w { snake = turnSnake DirectUp snake' }
  | unpack keyText == "Left" = w { snake = turnSnake DirectLeft snake' }
  | unpack keyText == "Right" = w { snake = turnSnake DirectRight snake' }
  | otherwise = w
    where snake' = snake w
          areaW = areaWidth w
          areaH = areaHeight w
handleEvent _ w = w

-- 进行一步动作
handleSnakeAction :: Snake -> SnakeAction -> World -> World
handleSnakeAction snake' action w
  | action == SnakeMove = w {snake = moveSnake snake'}
  | action == SnakeEat = w { snake = eatingSnake snake'
                           , apple = apple' -- 重新生成苹果
                           , rnds = rnds'}
  | action == SnakeNoAct = w { snake = snake' }
  | action == SnakeDead = w { state = GameOver }
    where (apple', rnds') = randowApple w

snakeSpeed = 0.05

-- 碰撞检测部分
checkSnakeAction :: Double -> World -> Snake -> (Snake, SnakeAction)
checkSnakeAction dt world snakeS
  = if ds' > snkW then (snakeS { ds = 0 }, action ) -- 动了超过一格
                  else (snakeS { ds = ds' }, action1 )
    where ds' = ds snakeS + dt + snakeSpeed
          snkW = width snakeS
          headOrg = head $ bodyPoints $ snake world -- 蛇头
          headN = translatedPoint dx dy headOrg -- 新蛇头
          (dx, dy) = getSnakeDxDy snakeS
          
          appleOrg = apple world
          (snkHead:snkTail) = bodyPoints $ snake world
          -- 动了超过一格，判断吃或只移动（死了下一帧判断）
          action = if headN == positionA appleOrg then SnakeEat else SnakeMove
          -- 判断蛇头是否撞蛇身或出界
          action1 = if headOrg `elem` snkTail
                     || outofBound headOrg
                    then SnakeDead else SnakeNoAct
          outofBound (x, y) = if x < 0 - maxW || x > maxW - gridSize
                                || y < 0 - maxH || y > maxH - gridSize
                              then True else False
          maxW = areaWidth world / 2
          maxH = areaHeight world / 2
```

游戏效果：

![wishimg](https://cdn.jsdelivr.net/gh/wineee/MarkDownPIC@master/img/9b9b7cf044edeaee63cff5e87dba6e58.png)

