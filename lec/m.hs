module Bob where
elem' _ []     = False
elem' x (y:ys) = x==y || elem' x ys


data Tree a = Leaf a
            | Branch (Tree a) (Tree a)
            deriving (Eq, Show)

{-
instance Eq a => Eq (Tree a) where
  Leaf a == Leaf b             =  a == b
  Branch l1 r1 == Branch l2 r2 = l1 == l2 && r1 == r2
  _ == _                       = False
-}

