a = 5

_a5 = 5

--a = 10


lst = [1, 2, 3]

lst1 = [1..3]

concate_lst = lst ++ lst1

ns = [1..]

rev = [20, 19..1]



gh1 = let x = 3; y = 2 in x ^ 2

gh2 = x ^ 2
 where x = 3

fr x = x ^ 2 / (1 + x)


-- Pattern Matching
myHead [] = error "List is empty"
myHead (x:xs) = x


mySecond [] = error "List is empty"
mySecond [x] = error "List is too short"
mySecond (x:y:xs) = y


myLast [] = error "List is empty"
myLast [x] = x
myLast (x:xs) = myLast xs


myTake k [] = []
myTake 0 lst = []
myTake k (x:xs) = x:(myTake (k - 1) xs)


myTake' _ [] = []
myTake' 0 _ = []
myTake' k (x:xs) = x:(myTake' (k - 1) xs)



--генератор списков
ns2 = [n * 2 | n <- ns]

ns2v1 = [n | n <- ns, mod n 2 == 0]

ns2v2 = [n | n <- ns, n `mod` 2 == 0]



-- xn = {n! / n^n}
seqn = [(fac n) / (denom n) | n <- [1..]]
 where 
  fac 1 = 1
  fac n = n * (fac (n - 1))
  denom n = n ** n


seqn1 = [let fac 1 = 1
             fac n = n * (fac (n - 1))
             denom = (n ** n)
         in (fac n) / denom | n <- [1..]]


-- let <определения> in <выражение>. Определения let сами по себе являются выражениями. Определения в секциях where – просто синтаксические конструкции.


ns2v3 = [n | n <- ns, cond n]
 where cond n = n `mod` 2 == 0

ns2v4 = [n | n <- ns, let cond = n `mod` 2 == 0
                      in cond]


--guards
myCompare a b
 | a == b = EQ
 | a < b = LT
 | otherwise = GT

a `myCompare'` b
 | a == b = EQ
 | a < b = LT
 | otherwise = GT

