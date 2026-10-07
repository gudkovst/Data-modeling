-- тип как перечисление значений (конечный)
data MyType = Foo | Bar

-- тип как декартово произведение ранее определённых типов
data Date = Date {day :: Int, month :: Int, year :: Int}

-- индуктивное определение (бесконечный тип из ничего)
data Nat = Zero | S Nat
 deriving (Eq, Ord, Show)

d2 = S $ S Zero

infinity = S infinity

e = d2 == infinity


integerToNat :: Integer -> Nat
integerToNat n 
 | n < 0 = error "Not Nat"
 | n == 0 = Zero
 | otherwise = S (integerToNat $ n - 1)


add Zero nat = nat
add (S m) nat = S (add m nat)


-- параметризованное семейство типов

-- :i []
-- type List :: * -> *
-- data List a = [] | a : [a]

-- дерево с метками типа а - у узла есть левое поддерево, метка и правое поддерево
data Tree a = Nil | Node (Tree a) a (Tree a)
 deriving Show

-- Tree :: * -> *
-- Tree - конструктор типов - функция, действующая в пространстве имён типов

tree = Node (Node Nil 'f' Nil) 'o' (Node Nil 'o' Nil)
-- :t tree
tr = Node Nil (Node Nil (5 :: Int) Nil) Nil

infBranchR = Node Nil 1 infBranchR

infBranchL = Node infBranchL 1 Nil


flatten :: Tree a -> [a]
flatten Nil = []
flatten (Node left x right) = flatten left ++ [x] ++ flatten right


-- data Maybe a = Nothing | Just a

integer2MaybeNat :: Integer -> Maybe Nat
integer2MaybeNat n 
 | n < 0 = Nothing
 | n == 0 = Just Zero
 | otherwise = Just $ S (fromMaybe . integer2MaybeNat $ n - 1)

fromMaybe m = maybe (error "Not Nat") id m


instance (Eq a) => Eq (Tree a) where
 (Node lx x rx) == (Node ly y ry) = x == y && lx == ly && rx == ry
 Nil == Nil = True
 _ == _ = False


{-
class Functor f where
  fmap :: (a -> b) -> f a -> f b

  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}
-}

-- map :: (a -> b) -> [a] -> [b]

-- Конструктор списка лежит в классе функторов
{-
instance Functor [] where
  fmap = map
-}

-- Типы, которые могут вести себя как контейнеры по отношению к другим типам, могут быть функторами.

{-
instance Functor Maybe where
  fmap fun (Just x) = Just (fun x)
  fmap fun Nothing = Nothing
-}