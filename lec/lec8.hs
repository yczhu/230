module Lec8 where
-- This lecture mainly talks about Parser is actually a Monad
-- Define Applicative and Functor first
-- Or deriving Functor from data Parser

import Control.Applicative
import Control.Monad (liftM, ap)
import Data.Char

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap

{-
-- Some problem with this
instance Functor Parser where
  fmap f (P pa) = P (\cs -> [(f xa, cs') | (xa, cs') <- pa cs])
  
instance Applicative Parser where
  pure x = P (\cs -> [(x, cs)])
  (P fP) <*> (P xP) = P (\cs -> [(f x, cs') | (f, cs') <- fP cs
                                            , (x, cs'') <- xP cs'])
-}

data Parser a = P (String -> [(a, String)])

doParse :: Parser a -> String -> [(a, String)]
doParse(P p) s = p s

oneChar = P (\cs -> case cs of
                c:cs' -> [(c, cs')]
                _     -> [])

instance Monad Parser where
  -- return :: a -> Parser a
  return x = P (\cs -> [(x, cs)])
  (>>=)    = bindP

bindP :: Parser a -> (a -> Parser b) -> Parser b
bindP p1 fp2 = P $ \cs -> [(y, cs'') | (x, cs')  <- doParse p1 cs
                                     , (y, cs'') <- doParse (fp2 x) cs']

pairP :: Parser a -> Parser b -> Parser (a, b)
pairP px py = do x <- px
                 y <- py
                 return (x, y)

failP = P (const [])

satP ::  (Char -> Bool) -> Parser Char
satP p = do c <- oneChar 
            if p c then return c else failP

lowercaseP = satP isAsciiLower
uppercaseP = satP isAsciiUpper

alphaChar = satP isAlpha
digitChar = satP isDigit

digitInt :: Parser Int
digitInt  = do c <- digitChar
               return ((read [c]) :: Int)

strP :: String -> Parser String
strP []      = return ""
-- Parser that leaves input untouched, return as output"
strP (c: cs) = do x <- charP c
                  xs <- strP cs
                  return (x:xs)

oddP = strP ""
res = doParse oddP "cat"

charP :: Char -> Parser Char
charP c = satP (c ==)

grabn :: Int -> Parser String
grabn n
  | n <= 0    = return ""
  | otherwise = do c  <- oneChar
                   cs <- grabn (n-1)
                   return (c:cs)

chooseP :: Parser a -> Parser a -> Parser a
p1 `chooseP` p2 = P $ \cs -> doParse p1 cs ++ doParse p2 cs

alphaNumChar = alphaChar `chooseP` digitChar

grab2or4 = grabn 2 `chooseP` grabn 4

take4P :: Parser String
take4P = do
    x1 <- oneChar
    x2 <- oneChar
    x3 <- oneChar
    x4 <- oneChar
    return [x1,x2,x3,x4]

takeNP :: Int -> Parser String
{-
takeNP 0 = return ""
takeNP n = do
    x <- oneChar
    xs <- takeNP (n-1)
    return (x: xs)
-}
takeNP n = sequence (replicate n oneChar)

intOp      = plus `chooseP` minus `chooseP` times `chooseP` divide
  where
    plus   = charP '+' >> return (+)
    minus  = charP '-' >> return (-)
    times  = charP '*' >> return (*)
    divide = charP '/' >> return div

take2or4P = (takeNP 2) `chooseP` (takeNP 4)

catars = doParse take2or4P "catars"

calc = do x <- digitInt
          o <- intOp
          y <- digitInt
          return (x `o` y)

-- Examples with calc
divide8 = doParse calc "8/2"
multiplycat = doParse calc "8*2cat"
-- "8*20cat" -> [(16,"0cat")], calc cannot deal with this correctly
wrong_calc = doParse calc "8*20cat"

manyP :: Parser a -> Parser [a]
manyP p   = many1 `chooseP` many0
  where
    many0 = return []
    many1 = do x  <- p
               xs <- manyP p
               return (x:xs)

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = P $ \cs -> case doParse (p1 `chooseP` p2) cs of
                         []  -> []
                         x:_ -> [x]

mmanyP     :: Parser a -> Parser [a]
mmanyP p   = mmany1 <|> mmany0
  where
    mmany0 = return []
    mmany1 = do x  <- p
                xs <- mmanyP p
                return (x:xs)
