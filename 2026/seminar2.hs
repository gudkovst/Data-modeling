-- Списки гомогенные - все элементы должны быть одного типа
-- Кортеж (tuple) (и пара - частный случай - кортеж длины 2) - может содержать элементы разных типов (геторогенны).
-- Список кортежей: все кортежи должны иметь одинаковую структуру (как кортежи иметь одинаковый тип)

pr1 = (1, 'v')

pr2 = (,) 1 'v'

p1 = fst pr1
p2 = snd pr1

-- Максимальная длина кортежа - 64.
-- :t (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)


divs n = [(a, b) | b <- [1..n], a <- [1..b], a * b == n]


-- Ещё раз о Pattern matching
bf [] = []
bf [_, 2, x] = [x - 2]
bf [1, 3, 5] = [(-1)]
bf list@(x:xs) = (x * sum xs):list -- as-pattern: можно разобрать дважды по-разному, трижды нельзя


bfp (x, y) = (x, (x, y))

bfp' = \(x, y) -> (x, (x, y))

bfp'' = \p@(x, _) -> (x, p)


-- Композиция функций:
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- ($) :: (a -> b) -> a -> b
-- $ имеет очень низкий приоритет и нужен для отделения аргумента от функции

f1 x = (3*x + 1) ^ 2
f2 = (1+)

q1 = f2 . f1 $ 2
q2 = (f2 . f1) 2

ff = f2 . f1

w1 = ff 2 + 3
w2 = ff $ 2 + 3


-- map, filter, zip, zipWith, foldl, foldr
-- scanl, scanr

ff_ns = map ff [1..]

tr_ns = map (\x -> 4*x - 3) [1..]

ziped = zip [1..5] [1..7]

zipWith' fun lst1 lst2 = [fun x y | (x, y) <- zip lst1 lst2]


myFoldl fun acc [] = acc
myFoldl fun acc (x:xs) = myFoldl fun cur xs
 where
  cur = acc `fun` x


subl = foldl1 (-) [1..3]

subr = foldr1 (-) [1..3]

sub_scanl = scanl1 (-) [1..3]

sub_scanr = scanr1 (-) [1..3]


-- (1 - 2) - 3
-- 1 - (2 - 3)


-- Хвостовая рекурсия
fac 0 = 1
fac n = n * fac (n - 1)


fac_inner 0 acc = acc
fac_inner n acc = fac_inner (n - 1) (acc * n)

fac' n = fac_inner n 1
