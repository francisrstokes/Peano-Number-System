data Nat = O
         | S Nat
         deriving (Show, Eq);

instance Num Nat where
  (+) = add
  (-) = sub
  (*) = mul
  abs = id
  signum a = 1
  fromInteger = fromInt'

instance Ord Nat where
  (<=) = lte

successor :: Nat -> Nat
successor n = S n

predecessor :: Nat -> Nat
predecessor (S n) = n
predecessor O = O

add :: Nat -> Nat -> Nat
add a O = a
add O b = b
add a b = add (S a) (predecessor b)

sub :: Nat -> Nat -> Nat
sub a O = a
sub O b = O
sub a b = sub (predecessor a) (predecessor b)

mul :: Nat -> Nat -> Nat
mul a O = O
mul O b = O
mul a (S b) = a + (mul a b)

lte :: Nat -> Nat -> Bool
lte O O = True
lte a O = False
lte O b = True
lte (S a) (S b) = a == b || lte a b

toInt :: Nat -> Int
toInt O = 0
toInt (S n) = f n 1 where
  f (S n) i = f n (i + 1)
  f O i = i

fromInt :: Int -> Maybe Nat
fromInt 0 = Just O
fromInt n | n < 0     = Nothing
          | otherwise = f n O
            where f n x = if (n > 0) then f (n - 1) (S x) else Just x

fromInt' :: Integer -> Nat
fromInt' 0 = O
fromInt' n | n < 0    = O
          | otherwise = f n O
            where f n x = if (n > 0) then f (n - 1) (S x) else x
