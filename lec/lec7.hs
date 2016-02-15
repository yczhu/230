module Lec7 where
-- A first idea
--type Parser a = String -> (a, String)
--  String -> (a, String) for The remaining string
--  Better than String -> a

--  parse "2+1+5"
data Expr = Number Int
          | Plus   Expr Expr

--exprP :: Parser Expr
-- Ambiguous
--(Plus (Plus (Number 2) (Number 1)) (Number 5))
--(Plus (Number 2) (Plus (Number 1) (Number 5)))

doParse :: Parser a -> String -> [(a, String)]
doParse(P p) s = p s

-- Return all the possible parsing
data Parser a = P (String -> [(a, String)])
                   --deriving (Functor)
            
{-
    type    X = ... Alias
    data    X = ... Create a new data type
    newtype X = ... Fast
-}

-- Write a single character parser 
-- "cat" -> ['c', "at`"]

oneChar = P (\cs -> case cs of
        c:cs' -> [(c, cs')]
        _     -> [])

cat = doParse oneChar "cat"
-- "cat" -> [('c', "at")]


twoChar :: Parser (Char, Char)
twoChar  = P (\cs -> case cs of
            c1:c2:cs' -> [((c1, c2), cs')]
            _         -> [])

hey = doParse twoChar "hey!"
-- "hey!" -> [(('h', 'e'), "y!")]

pairP :: Parser a -> Parser b -> Parser(a, b)
pairP p1 p2 = P(\cs -> 
            [((x, y), cs'') | (x, cs')  <- doParse p1 cs,
                              (y, cs'') <- doParse p2 cs'])

-- twoChar' is identical to twoChar
twoChar' = pairP oneChar oneChar


