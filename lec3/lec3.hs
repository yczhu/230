module Bob where
data Unshowable = A | B | C
    --  Deriving show or instance show
    --deriving (Eq, Show)

instance Show Unshowable where
-- This type declaraction is not allowed in the instance.
--show :: Unshowable ->  String
  show A = "Random A"
  show B = "Mess it B"
  show C = "Blob C"

{-
class Show a where
    ...
    show :: a -> String
    ...
-}

data BST k v = Empty 
             | Node k v (BST k v) (BST k v) 
             -- The Eq is not good for BST
             --deriving (Show, Eq)
             deriving (Show)
-- Define eq for BST
instance (Eq k, Eq v) => Eq (BST k v) where
    t1 == t2 = toList t1 == toList t2

menu :: BST String Double
menu = Node "pizza" 2.05
    (Node "burrito" 7.5 Empty Empty)
    (Node "salad" 2.5 Empty Empty)

--Quiz b. BST k v -> [k]

foldBST op base Empty          = base 
foldBST op base (Node k v l r) = op k v ll rr
  where
   ll                          = foldBST op base l 
   rr                          = foldBST op base r 

-- Definition of Maybe
--data Maybe a = Just a | Nothing

find :: (Ord k) => k -> BST k v -> Maybe v
find key Empty     = Nothing
find key (Node k v l r)
    | key == k     = Just v
    | key  < k     = find key l
    | key  > k     = find key r

-- Ord class is used for ordered datatypes

-- derving Eq, Ord

insert :: (Ord k) => k -> v -> BST k v -> BST k v
insert k' v' Empty              = Node k' v' Empty Empty
insert k' v' (Node k v l r)
    | k' == k   = Node k v' l r
    | k' < k    = Node k v (insert k' v' l) r
    | otherwise = Node k v l (insert k' v' r)

t0 = insert "burrito"     4.50 Empty
t1 = insert "chimichanga" 5.25 t0 
t2 = insert "frijoles"    2.75 t1

ofList :: (Ord k) => [(k,v)] ->  BST k v
ofList = foldl (\t (k, v) -> insert k v t) Empty

t = ofList [ ("chimichanga", 5.25)
           , ("burrito"    , 4.50)
           , ("frijoles"   , 2.75)]

toList :: (BST k v) -> [(k, v)]
toList =  foldBST (\k v l r -> l ++ [(k, v)] ++ r) []

--delete :: (Ord k) => k -> BST k v -> BST k v
-- TODO write a delete of BST
