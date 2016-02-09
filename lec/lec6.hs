{-# LANGUAGE TypeSynonymInstances #-}
module Bob where
import Control.Applicative
import Control.Monad (liftM, ap)

type State = Int

data ST0 a = S0 (State -> (a, State))

apply0 :: ST0 a -> State -> (a, State)
apply0 (S0 f) x = f x

instance Functor ST0 where
  fmap = liftM

instance Applicative ST0 where
  pure  = return
  (<*>) = ap

instance Monad ST0 where
  -- return :: a -> ST a
  return x   = S0 (\s -> (x, s))
  -- (>>=)  :: ST a -> (a -> ST b) -> ST b
  st >>= f   = S0 $ \s -> let (x, s') = apply0 st s in 
                          apply0 (f x) s'

--(>>) :: Monad m => m a -> m b -> m b

fresh :: ST0 Int
fresh =  S0 (\n -> (n, n+1))

wtf1 = fresh >>= \_ ->
         fresh >>= \_ ->
           fresh >>= \_ ->
             fresh  
{-

wtf1 = do fresh
    fresh
    fresh
    fresh

wtf1 = fresh >> fresh >> fresh >> fresh 
-}
--apply0 wtf1 0   a. (3,4)

wtf2 = fresh >>= \n1 ->
         fresh >>= \n2 ->  
           fresh >>
             fresh >>
               return [n1, n2]

-- apply0 wtf2 0    c. ([0,1], 4)

{-
wtf3 = do n1 <- fresh
          n2 <- fresh
          fresh
          fresh
          return [n1, n2]
-}
