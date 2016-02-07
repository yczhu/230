module Bob where
bob x y = (x, y) 
foo1 = (`bob` 2) -- x -> bob x 2

inc = map(+1)
sqr = map(^2)

convertList f [] = []
convertList f (x:xs) = f x : convertList f xs

convert2List :: (a1->a2->b) -> [a1] -> [a2] -> [b]
convert2List f _ [] = []
convert2List f [] _ = []
convert2List f (x:xs) (y:ys) = f x y : convert2List f xs ys

-- data Maybe a = Just a | Nothing
convertMaybe :: (a -> b) -> Maybe a -> Maybe b
convertMaybe f z  = case z of
            Just x-> Just (f x)
            Nothing -> Nothing

data BST k v = Emp 
             | Node k v (BST k v) (BST k v)
             --deriving (Show, Functor)

--convert (Emp) = Emp
-- convert (Node k v l r) = Node k (show v) (convert l) (convert r)
convertBST f Emp = Emp
convertBST f (Node k v l r) = Node k (f v) (convert f l) (convert f r)

--bstText = convertBST (show)
bstPlus = convertBST (+1)

class Convertable t where
  convert :: (a->b) -> t a -> t b

instance Convertable [] where
  convert = convertList

instance Convertable Maybe where
  convert = convertMaybe

instance Convertable (BST k) where
  convert = convertBST

convertIO :: (a->b) -> IO a -> IO b
convertIO f action = do a <- action
                        return (f a)

bst :: BST String Int
bst = Node "ran" 0 (Node "bob" 12 Emp Emp) Emp
bst' = convert (2 *) bst

class Convertable2 t where
  convert2 :: (a1 -> a2 -> b) -> t a1 -> t a2 -> t b

data Expr1 = Val1 Int
           | Div1 Expr1 Expr1
             deriving (Show)

eval1 :: Expr1 -> Int
eval1 (Val1 n) = n
eval1 (Div1 x y) = eval1 x `div` eval1 y

e1 = Val1 12
e2 = Val1 6
e3 = Div1 e1 e2
e4 = Div1 e3 e2
e5 = Div1 e4 e2
e6 = Div1 e1 e5

safeDiv :: Int -> Int -> Maybe Int
safeDiv x 0 = Nothing
safeDiv x y = Just (x `div` y)

eval2 :: Expr1 -> Maybe Int
eval2 (Val1 n) = Just n
eval2 (Div1 x y) = case eval2 x of
                   Nothing -> Nothing
                   Just nx -> case eval2 y of
                                Nothing -> Nothing
                                Just ny -> nx `safeDiv` ny

-- seqn
-- apply

{-
(>>=) :: Maybe a-> (a -> Maybe b) -> Maybe b
m >>= f = case m of
            Nothing -> Nothing
            Just x -> f x
-}

eval (Val1 n) = Just n
eval (Div1 x y) =  eval x >>= \n ->
                       eval y >>= \m ->
                          safeDiv n m
