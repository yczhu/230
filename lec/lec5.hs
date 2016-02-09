module Bob where
    
{-
class Monad m where 
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
-}

{-
instance Monad [] where 
  return = returnForList
  (>>=)  = bindForList

returnForList :: a-> [a]
returnForList x = [x]

bindForList :: [a] -> (a -> [b]) -> [b]

-- concat(map f as)
bindForList as f = concatMap f as

--bindForList [] f =  []
--bindForList (x:xs) f = f x ++ bindForList xs f

-}

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys =  do x <- xs
                  y <- ys
                  -- xs >>= \x ->
                  -- ys >>= \y ->
                  return (x, y)

ans2 = do
    x <- [1,2,3]
    y <- ["cat", "dog"]
    return (x,y)

{-
ans2 = [1,2,3] >> = \x ->
    ["cat", "dog"] >>= \y ->
    [(x, y)]

ans2 = [1,2,3] >>= body
    = body 1 ++ body 2 ++ body 3

body = \x ->
    ["cat", "dog"] >>= innterbody
    = innerbody "cat" ++ innerbody "dog"

innerbody y = [(x,y)]
-}

-- Behave like a loop
pairs' xs ys =
    [(x,y) | x <- xs, y <- ys]

foo xs = do
    x <- xs
    return (x+1)

ans1 = foo [1,2,3]

{-
    = do x <- [1,2,3]
        return (x+1)

    = [1,2,3] >>= \x ->
        return (x+1)

    = [1,2,3] >>= \x ->
        [(x+1)]

    = [1+1] ++ [2+1] ++ [3+1]

    = [2,3,4]
-}

foo' xs = do
    x <- xs
    return (show x)

