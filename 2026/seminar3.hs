{- Тип данных определяет:

  множество допустимых значений;
  внутреннее представление;
  набор допустимых операций;
  правила преобразования и сравнения;
  ограничения на хранение.
-}

--  :i Bool

a = 5 :: Int  -- ограниченный: [-2^29 .. 2^29-1],  maxBound :: Int, работает быстро
b = 5 :: Integer  -- неограниченный, память выделяется динамически

s = fromIntegral a + b
s' = a + fromIntegral b

c = 5

q1 = a + c
--q2 = b + c


x = x


data MyType = Foo | Bar  -- определили тип

-- Класс типов - множество типов, поддерживающих общие операции (удовлетворяющие общим условиям)
{-
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}
-}

instance Eq MyType where  -- поместили тип MyType в класс Eq
 Foo == Foo = True
 Bar == Bar = True
 _ == _ = False

 _ /= _ = True


{-
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
  {-# MINIMAL compare | (<=) #-}
-}

instance Ord MyType where
 Bar <= Foo = False
 _ <= _ = True

 Foo < Foo = True
 _ < _ = False


{-
class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS
  {-# MINIMAL showsPrec | show #-}
-}

instance Show MyType where
 show Foo = "Foo'"
 show Bar = "Bar'"

deriving instance Read MyType  -- автоматически генерируемые парсеры

m1 = read "Foo" :: MyType
m1' = read "Foo'" :: MyType


{-
class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]
  {-# MINIMAL toEnum, fromEnum #-}
-}

instance Enum MyType where
 fromEnum Foo = 1
 fromEnum Bar = 2

 toEnum 1 = Bar
 toEnum 0 = Foo
 --toEnum _ = error "Out of enum"
 toEnum n = toEnum $ n `mod` 2

m2 = take 10 [(Foo)..]


{-
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
-}

instance Num MyType where
 Foo + Foo = Foo
 Bar + Foo = Bar
 Foo + Bar = Bar
 Bar + Bar = Foo

 t1 * t2 = toEnum $ (fromEnum t1) * (fromEnum t2)
 negate = id
 abs = id
 signum = id
 fromInteger n = toEnum $ (fromIntegral n) `mod` 2

m3 = Bar * (Foo + Bar) + 7


-- Конструкторы с параметрами
data Date = Date (Int, Int, Int) | Date' Int Int Int 
 | Date'' {day :: Int, month :: Int, year :: Int}
 deriving (Eq, Show)


d3 = Date'' 2 5 8
d4 = Date'' {year = 1861, day = 19, month = 2}

day_7 = Date'' 7


getYear (Date (_, _, y)) = y
getYear (Date' _ _ y) = y
getYear (Date'' {year = y}) = y
