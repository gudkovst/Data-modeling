{-
class Semigroup a => Monoid a where
  mappend :: a -> a -> a   -- ассоциативная бинарная операция на а. В Semigroup (классе, где нет нейтрального элемента) обозначается: <>
  mempty :: a  -- нейтральный элемент: forAll x из а: х <> mempty == mempty <> х == mempty
  mconcat :: [a] -> a  -- перемножение всех элементов списка (с помощью mappend)
-}

l0 = [1, 2, 3] ++ []

l1 = (^2) . id

l2 = mappend [1..5] [1..8]

l3 = mappend (Just [1..5]) (Just [1..8])

l4 = mappend (Just [1..5]) Nothing


{-  Контейнеры, все элементы которых можно агрегировать в один элемент
class Foldable t where
  fold :: Monoid m => t m -> m

  foldMap :: Monoid m => (a -> m) -> t a -> m

  GHC.Internal.Data.Foldable.foldMap' :: Monoid m =>
                                         (a -> m) -> t a -> m

  foldr :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b


  GHC.Internal.Data.Foldable.foldr' :: (a -> b -> b) -> b -> t a -> b
  foldl' :: (b -> a -> b) -> b -> t a -> b
  foldr1 :: (a -> a -> a) -> t a -> a
  foldl1 :: (a -> a -> a) -> t a -> a

  toList :: t a -> [a]
  null :: t a -> Bool
  length :: t a -> Int
  elem :: Eq a => a -> t a -> Bool
  maximum :: Ord a => t a -> a
  minimum :: Ord a => t a -> a
  sum :: Num a => t a -> a
  product :: Num a => t a -> a
  {-# MINIMAL foldMap | foldr #-}
-}

l5 = length $ Just [1..10]


data Tree a = Nil | Node (Tree a) a (Tree a) deriving Show

instance Foldable Tree where
 foldMap :: Monoid m => (a -> m) -> Tree a -> m
 foldMap _ Nil = mempty
 foldMap fun (Node left x right) = (foldMap fun left) <> fun x <> (foldMap fun right)

 foldl fun u Nil = u
 foldl fun u (Node left x right) = foldl fun ((foldl fun u left) `fun` x) right

 foldr fun u Nil = u
 foldr fun u (Node left x right) = foldr fun (x `fun` (foldr fun u right)) left


testTree = Node (Node (Node Nil 1 Nil) 2 (Node Nil 3 Nil)) 
                4 
                (Node (Node Nil 5 Nil) 6 (Node Nil 7 Nil))


-- Ленивые вычисления - значения выражения вычисляется только тогда, когда от этого значения зависит 
-- дальнейшее исполнение вычислений: условные переходы. Пока можно не вычислять - хранится в невычисленном виде.
-- Экономия времени на вычисления, расход памяти на хранение.
-- Благодаря этому - возможность работать с бесконечными структурами данных.

{-
ghci> lst = [1..10] :: [Int]
ghci> :p lst
lst = (_t34::[Int])
ghci> length lst
10
ghci> :p lst
lst = [1,2,3,4,5,6,7,8,9,10]
ghci> l = map (+1) lst
ghci> :p l
l = (_t35::[Int])
ghci> length l
10
ghci> :p l
l = [(_t36::Int),(_t37::Int),(_t38::Int),(_t39::Int),(_t40::Int),
     (_t41::Int),(_t42::Int),(_t43::Int),(_t44::Int),(_t45::Int)]
ghci> l !! 5
7
ghci> :p l
l = [(_t46::Int),(_t47::Int),(_t48::Int),(_t49::Int),(_t50::Int),7,
     (_t51::Int),(_t52::Int),(_t53::Int),(_t54::Int)]
ghci> l' = (5 * 7):l
ghci> :p l'
l' = [(_t55::Int),(_t56::Int),(_t57::Int),(_t58::Int),(_t59::Int),
      (_t60::Int),7,(_t61::Int),(_t62::Int),(_t63::Int),(_t64::Int)]
-}

x1 = undefined || True
x2 = True || undefined

x1' = undefined * 0
x2' = 0 * undefined
-- нет pattern-matching: умножение делается в ассемблере - undefined нужно вычислить перед загрузкой в регистр

x3 = (\_ -> 2026) undefined

lx = [True, False] ++ repeat undefined

lx1 = take 2 lx

lx2 = take 3 lx

lx3 = foldr (||) False lx
-- foldr fun u (x:xs) = x `fun` (foldr fun u xs)

lx3' = foldl (||) False lx
-- foldl fun u (x:xs) = foldl fun (u `fun` x) xs
-- foldl - ленивый, (u `fun` x) на каждом шаге не вычисляется, 
-- а просто копится в памяти как выражение

x3' = (\_ -> 2026) $! undefined

a |!| b = (a ||) $! b

lx3'' = foldr (|!|) False lx
-- True |!| (foldr (|!|) False [False, undefined ..])) ~>
-- True |!| (False |!| (foldr (|!|) False [undefined ..]))
-- пока не будет вычислен второй аргумент |!|, не вычислится и результат |!|

lx2' = foldr (|!|) False lx2

lx4 = foldl' (||) False lx
-- foldl' fun u (x:xs) = (\t -> foldl' fun t xs) $! fun u x
-- (u `fun` x) на каждом шаге вычисляется, память не тратится

lx4' = foldl' (|!|) False lx

bigList = [1..200000000]

s = foldl' (+) 0 bigList

-- Обычно применяют так: если структура данных бесконечна - то foldr (только он и сможет что-то сделать, 
--                                                                    да и то, если функция, применяемая для свёртки, - ленива);
-- если конечна и операция не полагается на ленивость - то foldl'.