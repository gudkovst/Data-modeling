fun x y g = g (y x)

{-
T(x) = a
T(y) = a -> b
T(g) = b -> c
T(fun) = a -> (a -> b) -> (b -> c) -> c

В общем виде: если fun a1 a2 ... an = res,
то T(fun) = T(a1) -> T(a2) -> ... -> T(an) -> T(res)
-}

-- rand() = rand(World)

-- Type  IO a = World -> (World, a)
-- Зависит от состояния внешнего мира, взаимодействуя с ним, меняет его состояние и возвращает значение типа a

-- getChar :: IO Char  -- нам из внешнего мира прилетела посылка, в которой упакован Char
-- putChar :: Char -> IO ()  -- мы во внешний мир отправили Char, нам выдали "квитанцию об отправке нашей посылки" - 
                             -- коробку, в которой лежит пустой кортеж (потому что каждая функция должна что-то возвращать)


test_equal = do x <- getChar  -- вытащили из IO Char (из коробки) сам Char - x
                y <- getChar  -- IO Char ~> Char

                let b = (x == y) in  -- чистый код: сравнили два Char и связали результат с именем b :: Bool
                  do putStrLn $ if b then "Yes: " ++ [x] ++ " == " ++ [y] else "No: " ++ [x] ++ "|/" ++ [y]  -- IO () - вычислили чистый код if, результат отправили во внешний мир. Вернулась - квитанция
                     return 313  -- IO Int - просто запаковали Int в посылку, ничего не отправляли
                     return b  -- IO Bool - просто запаковали b в посылку, ничего не отправляли


getLine' = do x <- getChar
              if x == '\n' then return []
              else do xs <- getLine'
                      return (x:xs)


putStrLn' [] = do putChar '\n'
                  return ()
putStrLn' (x:xs) = do putChar x
                      putStrLn' xs


{- Чтобы построить do нужно:
(>>=) :: IO a -> (a -> IO b) -> IO b
(>>) :: IO a -> IO b -> IO b
return :: a -> IO a  -- ныне рекомендуется использовать pure - то же самое, но не вызывает у программиста ложных ассоциаций
-}

a = getLine >>= (\s -> putStrLn s >> putStrLn (reverse s))

a' = do s <- getLine
        putStrLn s
        putStrLn $ reverse s


-- act1 >> act2 = act1 >>= (\_ -> act2)
a1 = putStrLn "foo" >>= (\_ -> putStrLn "bar")


a2 = getLine >>= \s -> pure (if even . length $ s then "even" else "odd")

a3 = a2 >>= putStrLn . reverse


sequence' :: [IO a] -> IO [a] -- принимает список действий и возвращает список их результатов, все действия - независимы
sequence' [] = pure []
sequence' (a:as) = do x <- a
                      xs <- sequence' as
                      pure (x:xs)

-- print = putStrLn . show
a4 = sequence $ map print [1..8]
a4_ = sequence_ $ map print [1..8]

putStrLn'' cs = sequence_ [putChar c | c <- (cs ++ "\n")]


{-
class Functor (f :: * -> *) where
 fmap :: (a -> b) -> f a -> f b

Общее, абстрактное определение функтора: такое преобразование типов, fmap для которого удовлетовряет свойствам:
 1) fmap id = id
 2) fmap (g . h) = (fmap g) . (fmap h)

Интуитивное понимание: функтор - это контейнер (система (или структура) коробок, хранящих значения одинакового типа),
который позволяет выполнять поэлементные преобразования содержимого.

Однако такое интуитивное понимание не ко всем функторам очевидно прикладывается. Например:
(-> a) - функции из а в какой-нибудь тип (преобразователь типов - принимает конкретный тип b и возвращает конкретный тип - функцию из а в b: (a -> b))

instance Functor ((->) a) where
 fmap :: (b -> c) -> (a -> b) -> (a -> c)

 fmap = (.) -- композиция функций
-}


{-
fmap :: (a -> b) -> f a -> f b

fmap2 :: (a -> b -> c) -> f a -> f b -> f c

Например, хотим складывать Maybe Int: fmap2 (+) :: Maybe Int -> Maybe Int -> Maybe Int
Но :t fmap (+) :: (Functor f, Num a) => f a -> f (a -> a) -- то есть Maybe Int -> Maybe (Int -> Int)
ghci> m = Just 5
ghci> fm = fmap (+) m
:t fm :: Num a => Maybe (a -> a)
А мы бы хотели, чтобы fm :: Maybe Int -> Maybe Int
-}

{-
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c

  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
-}

{-
ghci> liftA2 (+) (Just 5) (Just 2)
Just 7
ghci> liftA2 (+) (Just 5) Nothing
Nothing
ghci> pure (+1) <*> (Just 5)
Just 6

ghci> pure (+) <*> (Just 1) <*> Nothing
Nothing
ghci> :t pure (+)
pure (+) :: (Applicative f, Num a) => f (a -> a -> a)
ghci> :t pure (+) <*> (Just 1)
pure (+) <*> (Just 1) :: Num a => Maybe (a -> a)

ghci> Nothing <*> (Just 5)
Nothing
-}

{-
instance Applicative [] where
  pure x = [x]
  -- (<*>) :: [a -> b] -> [a] -> [b]
  fs <*> xs = [f x | f <- fs, x <- xs]
-}

l1 = [(+1), (+2), (+3)] <*> [1, 2, 3]

l2 = pure (*) <*> [1, 2, 3] <*> [4, 5]

cart lst = pure (,) <*> lst <*> lst


newtype ZipList a = Zip [a] deriving (Show, Functor)

instance Applicative ZipList where
  pure x = Zip (repeat x)
  (<*>) (Zip fs) (Zip xs) = Zip $ map (\(f, x) -> f x) (zip fs xs)

l3 = liftA2 (+) (Zip [1..5]) (Zip [1..12])


getChars :: Int -> IO String
getChars 0 = pure []
getChars n = liftA2 (:) getChar (getChars $ n - 1)

l4 = pure putStrLn <*> getLine  -- IO (String -> IO ()) <*> IO String ==> IO (IO ())

-- join :: Monad m => m (m a) -> m a
--(>>=) :: Monad m => m a -> (a -> m b) -> m b
l4' = l4 >>= id  -- IO (IO ()) -> (IO (IO ()) -> IO ()) -> IO ()

{-
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  {-# MINIMAL (>>=) #-}

  return = pure

   Законы монад:
  1) pure x >>= f = f x
  2) mx >>= pure = mx
  3) (mx >>= f) >>= g = mx >>= (\x -> (f x >>= g))

 Интуитивное понимание (далеко не всегда очевидное): вычисления с побочным эффектом
-}

{-
instance Monad [] where
  (>>=) :: [a] -> (a -> [b]) -> [b]
-}

dels = \x -> [n | n <- [1..x], x `mod` n == 0]
l5 = [1..10] >>= dels
