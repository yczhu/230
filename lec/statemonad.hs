module Bob where
newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
    fmap f (State x) = State $ \s -> let (a, s') = x s
                                 in ((f a), s')

instance Applicative (State s) where
    pure x = State $ \s -> (x, s) 
    -- State s (a->b) -> State s a -> State s b
    (State f)  <*> (State h) = State $ \s -> let (a, newState) = h s
                                                 (f', _) = f s
                                                 b = f' a
                                                 in (b, newState)

instance Monad (State s) where
    return x = State $ \s -> (x, s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                        in g newState


type Stack = [Int]
pop :: State Stack Int
pop = State $ \(x:xs) -> (x,xs)
 
push :: Int -> State Stack ()
push a = State $ \xs -> ((),a:xs)

stackManip :: State Stack Int
stackManip = do
    push 3
    a <- pop
    pop

m = runState stackManip [5,8,2,1]
