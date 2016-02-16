-- ---
-- title: Homework #2, Due Friday 2/12/16
-- ---

{-# LANGUAGE TypeSynonymInstances #-}
module Hw2 where

import Control.Applicative hiding (empty, (<|>))
import Data.Map
import Control.Monad.State hiding (when)
import Text.Parsec hiding (State, between)
import Text.Parsec.Combinator hiding (between)
import Text.Parsec.Char
import Text.Parsec.String

-- Problem 0: All About You
-- ========================

-- Tell us your name, email and student ID, by replacing the respective
-- strings below

myName  = "Yuanchao Zhu"
myEmail = "yuz063@eng.ucsd.edu"
mySID   = "A53098001"


-- Problem 1: All About `foldl`
-- ============================

-- Define the following functions by filling in the "error" portion:

-- 1. Describe `foldl` and give an implementation:

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f b []      = b
myFoldl f b (x:xs)  = let b' = f b x
                      in myFoldl f b' xs

-- 2. Using the standard `foldl` (not `myFoldl`), define the list reverse function:

myReverse :: [a] -> [a]
myReverse xs = Prelude.foldl (\l n -> [n] ++ l) [] xs

-- 3. Define `foldr` in terms of `foldl`:

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f b xs = Prelude.foldl (\base x -> f x base) b xs

-- 4. Define `foldl` in terms of the standard `foldr` (not `myFoldr`):

myFoldl2 :: (a -> b -> a) -> a -> [b] -> a
myFoldl2 f b xs = Prelude.foldr (\base x -> f x base) b xs

-- 5. Try applying `foldl` to a gigantic list. Why is it so slow?
--    Try using `foldl'` (from [Data.List](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-List.html#3))
--    instead; can you explain why it's faster?

-- Part 2: Binary Search Trees
-- ===========================

-- Recall the following type of binary search trees:

data BST k v = Emp
             | Bind k v (BST k v) (BST k v)
             deriving (Show)

-- Define a `delete` function for BSTs of this type:
foldBST op base Emp            = base
foldBST op base (Bind k v l r) = op k v ll rr
  where
   ll                          = foldBST op base l
   rr                          = foldBST op base r


toList =  foldBST (\k v l r -> l ++ [(k, v)] ++ r) []

instance (Eq k, Eq v) => Eq (BST k v) where
  t1 == t2 = Hw2.toList t1 == Hw2.toList t2

ofList :: (Ord k) => [(k, v)] -> BST k v
ofList = Prelude.foldl (\t (k, v) -> Hw2.insert k v t) Emp

insert :: (Ord k) => k -> v -> BST k v -> BST k v
insert k v Emp = Bind k v Emp Emp
insert k v (Bind k' v' l r)
  | k == k'      = Bind k v l r
  | k <  k'      = Bind k' v' (Hw2.insert k v l) r
  | otherwise    = Bind k' v' l (Hw2.insert k v r)

-- Remove the biggest, i.e. the rightest node in a BST tree, return the removed node's key & value, and the new BST tree
removeBiggest :: (Ord k) => BST k v -> Maybe (k, v, BST k v)
removeBiggest Emp            = Nothing
removeBiggest (Bind k v l r) = 
    case removeBiggest r of
        --Nothing -> Just(k, v, Bind k v l r)
        Nothing -> Just(k, v, Emp)
        Just (k', v', t) -> Just (k', v', Bind k v l t)

delete :: (Ord k, Eq k, Eq v) => k -> BST k v -> BST k v
delete k (Bind k' v' l r)
    | k == k' && l == Emp   = r
    | k == k' && r == Emp   = l
    | k == k'               = case removeBiggest l of
                                Just (k'', v'', l') -> Bind k'' v'' l' r
    | k < k'    = Bind k' v' (Hw2.delete k l) r
    | k > k'    = Bind k' v' l (Hw2.delete k r)
delete k Emp  = Emp

root = Bind 2 "Poi" bstl bstr 
bstl = Bind 1 "Kantai" Emp Emp
bstr = Bind 3 "Shimakaze" Emp Emp 

-- Part 3: An Interpreter for WHILE
-- ================================
type Variable = String

data Statement =
    Assign Variable Expression          -- x = e
  | If Expression Statement Statement   -- if (e) {s1} else {s2}
  | While Expression Statement          -- while (e) {s}
  | Sequence Statement Statement        -- s1; s2
  | Skip                                -- no-op
  deriving (Show)

data Expression =
    Var Variable                        -- x
  | Val Value                           -- v
  | Op  Bop Expression Expression
  deriving (Show)

-- and binary operators are simply two-ary functions

data Bop =
    Plus     -- (+)  :: Int  -> Int  -> Int
  | Minus    -- (-)  :: Int  -> Int  -> Int
  | Times    -- (*)  :: Int  -> Int  -> Int
  | Divide   -- (/)  :: Int  -> Int  -> Int
  | Gt       -- (>)  :: Int -> Int -> Bool
  | Ge       -- (>=) :: Int -> Int -> Bool
  | Lt       -- (<)  :: Int -> Int -> Bool
  | Le       -- (<=) :: Int -> Int -> Bool
  deriving (Show)

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show)

type Store = Map Variable Value

evalOp :: Bop -> Value -> Value -> Value
evalOp Plus   (IntVal i) (IntVal j)  = IntVal (i+j)
evalOp Minus  (IntVal i) (IntVal j)  = IntVal (i-j)
evalOp Times  (IntVal i) (IntVal j)  = IntVal (i*j)
evalOp Divide (IntVal i) (IntVal j)  = IntVal (div i j)
evalOp Lt (IntVal i) (IntVal j)  = BoolVal (i<j)
evalOp Le (IntVal i) (IntVal j)  = BoolVal (i<=j)
evalOp Gt (IntVal i) (IntVal j)  = BoolVal (i>j)
evalOp Ge (IntVal i) (IntVal j)  = BoolVal (i>=j)
-- Exception handle
evalOp _  _          _           = IntVal (0)

evalE :: Expression -> State Store Value
evalE (Var x)      = do s <- get
                        return (findWithDefault (Hw2.IntVal 0) x s)
evalE (Val v)      = return v

evalE (Hw2.Op o e1 e2) = do v1 <- evalE e1 
                            v2 <- evalE e2
                            return (evalOp o v1 v2)

evalS :: Statement -> State Store ()

evalS (Assign x e )    = do s <- get
                            v <- evalE e 
                            put (Data.Map.insert x v s)

evalS w@(While e s)    = do v <- evalE e
                            case v of
                                 BoolVal b -> case b of
                                                True  -> evalS s
                                                False -> return ()
                                 IntVal i  -> return ()
                            evalS w

evalS Skip             = return ()

evalS (Sequence s1 s2) = do st1 <- evalS s1
                            st2 <- evalS s2
                            return ()

evalS (If e s1 s2)     = do v <- evalE e 
                            case v of 
                                 BoolVal b -> case b of
                                                 True  -> evalS s1
                                                 False -> evalS s2
                                 IntVal i  -> return ()

execS :: Statement -> Store -> Store
execS stmt store =  let s = evalS stmt
                    in execState s store

-- such that `execS stmt store` returns the new `Store` that results
-- from evaluating the command `stmt` from the world `store`.
-- **Hint:** You may want to use the library function

-- ~~~~~{.haskell}
-- execState :: State s a -> s -> s
-- ~~~~~

-- When you are done with the above, the following function will
-- "run" a statement starting with the `empty` store (where no
-- variable is initialized). Running the program should print
-- the value of all variables at the end of execution.

run :: Statement -> IO ()
run stmt = do putStrLn "Output Store:"
              putStrLn $ show $ execS stmt empty

-- Here are a few "tests" that you can use to check your implementation.

w_test = (Sequence (Assign "X" (Op Plus (Op Minus (Op Plus (Val (IntVal 1)) (Val (IntVal 2))) (Val (IntVal 3))) (Op Plus (Val (IntVal 1)) (Val (IntVal 3))))) (Sequence (Assign "Y" (Val (IntVal 0))) (While (Op Gt (Var "X") (Val (IntVal 0))) (Sequence (Assign "Y" (Op Plus (Var "Y") (Var "X"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1))))))))

w_fact = (Sequence (Assign "N" (Val (IntVal 2))) (Sequence (Assign "F" (Val (IntVal 1))) (While (Op Gt (Var "N") (Val (IntVal 0))) (Sequence (Assign "X" (Var "N")) (Sequence (Assign "Z" (Var "F")) (Sequence (While (Op Gt (Var "X") (Val (IntVal 1))) (Sequence (Assign "F" (Op Plus (Var "Z") (Var "F"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1)))))) (Assign "N" (Op Minus (Var "N") (Val (IntVal 1))))))))))

-- As you can see, it is rather tedious to write the above tests! They
-- correspond to the code in the files `test.imp` and `fact.imp`. When you are
-- done, you should get

-- ~~~~~{.haskell}
-- ghci> run w_test
-- Output Store:
-- fromList [("X",IntVal 0),("Y",IntVal 10)]

-- ghci> run w_fact
-- Output Store:
-- fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]
-- ~~~~~

-- Problem 4: A Parser for WHILE
-- =============================

-- It is rather tedious to have to specify individual programs as Haskell
-- values. For this problem, you will use parser combinators to build a parser
-- for the WHILE language from the previous problem.

-- Parsing Constants
-- -----------------

-- First, we will write parsers for the `Value` type

valueP :: Parser Value
valueP = intP <|> boolP

-- To do so, fill in the implementations of

intP :: Parser Value
intP = error "TBD"

-- Next, define a parser that will accept a
-- particular string `s` as a given value `x`

constP :: String -> a -> Parser a
constP s x = error "TBD"

-- and use the above to define a parser for boolean values
-- where `"true"` and `"false"` should be parsed appropriately.

boolP :: Parser Value
boolP = error "TBD"

-- Continue to use the above to parse the binary operators

opP :: Parser Bop
opP = error "TBD"


-- Parsing Expressions
-- -------------------

-- Next, the following is a parser for variables, where each
-- variable is one-or-more uppercase letters.

varP :: Parser Variable
varP = many1 upper

-- Use the above to write a parser for `Expression` values

exprP :: Parser Expression
exprP = error "TBD"

-- Parsing Statements
-- ------------------

-- Next, use the expression parsers to build a statement parser

statementP :: Parser Statement
statementP = error "TBD"

-- When you are done, we can put the parser and evaluator together
-- in the end-to-end interpreter function

runFile s = do p <- parseFromFile statementP s
               case p of
                 Left err   -> print err
                 Right stmt -> run stmt

-- When you are done you should see the following at the ghci prompt

-- ~~~~~{.haskell}
-- ghci> runFile "test.imp"
-- Output Store:
-- fromList [("X",IntVal 0),("Y",IntVal 10)]

-- ghci> runFile "fact.imp"
-- Output Store:
-- fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]
-- ~~~~~





