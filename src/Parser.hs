module Parser where

import Data.Char

----- Monadic parser ------------------------------------------------
type Parser a = String -> [(a,String)]

bind :: Parser a -> (a -> Parser b) -> Parser b
p `bind` f = \inp -> concat [f v inp' | (v,inp') <- p inp]

result :: a -> Parser a
result v = \inp -> [(v,inp)]

zero :: Parser a
zero = \inp -> []

plus :: Parser a -> Parser a -> Parser a
p `plus` q = \inp -> (p inp ++ q inp)

----- General combinators -------------------------------------------
many :: Parser a -> Parser [a]
many p = force (neWord `plus` result [])
  where neWord = p `bind` \x -> many p `bind` \xs -> result (x : xs)

many1 :: Parser a -> Parser [a]
many1 p = p `bind` \x -> many p `bind` \xs -> result (x : xs)

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep =
  p `bind` \x -> many (sep `bind` \_ -> p) `bind` \xs -> result (x : xs)

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) `plus` result []

chainl
  :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op v = (p `chainl1` op) `plus` result v

chainl1
  :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = p `bind` rest
  where rest x = (op `bind` \f -> p `bind` \y -> rest (f x y)) `plus` result x

chainr
  :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op v = (p `chainr1` op) `plus` result v

chainr1
  :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op =
  p `bind`
  \x ->
    (op `bind` \f -> (p `chainl1` op) `bind` \y -> result (f x y)) `plus`
    result x

-- force increases laziness
force :: Parser a -> Parser a
force p =
  \inp ->
    let x = p inp
    in (fst (head x),snd (head x)) : tail x

first :: Parser a -> Parser a
first p =
  \inp ->
    case p inp of
      [] -> []
      (x:xs) -> [x] -- pattern match instead of take to prevent space leak

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = first (p `plus` q)

----- Parsers -------------------------------------------------------
item :: Parser Char
item =
  \inp ->
    case inp of
      [] -> []
      (x:xs) -> [(x,xs)]

sat :: (Char -> Bool) -> Parser Char
sat p =
  item `bind`
  \x ->
    if p x
       then result x
       else zero

char :: Char -> Parser Char
char x = sat (\y -> x == y)

digit :: Parser Char
digit = sat (\x -> '0' <= x && x <= '9')

lower :: Parser Char
lower = sat (\x -> 'a' <= x && x <= 'z')

upper :: Parser Char
upper = sat (\x -> 'A' <= x && x <= 'Z')

letter :: Parser Char
letter = lower `plus` upper

alphanum :: Parser Char
alphanum = letter `plus` digit

word :: Parser String
word = many letter

string :: String -> Parser String
string "" = result ""
string (x:xs) = char x `bind` \_ -> string xs `bind` \_ -> result (x : xs)

ident :: Parser String
ident = lower `bind` \x -> many alphanum `bind` \xs -> result (x : xs)

nat :: Parser Int
nat = (digit `bind` \x -> result (digitToInt x)) `chainl1` result op
  where m `op` n = 10 * m + n

natSlow :: Parser Int
natSlow = many1 digit `bind` \xs -> result (eval xs)
  where eval xs = foldl1 op $ map digitToInt xs
        m `op` n = 10 * m + n

int :: Parser Int
int = parseInteger `plus` nat
  where parseInteger = char '-' `bind` \_ -> nat `bind` \n -> result (negate n)

bracket
  :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close =
  open `bind` \_ -> p `bind` \x -> close `bind` \_ -> result x

ints :: Parser [Int]
ints =
  bracket (char '[')
          (int `sepby1` char ',')
          (char ']')

----- A simple arithmetic parser ------------------------------------
-- expr     ::= expr op factor | factor
-- op       ::= addop | expop
-- addop    ::= + | -
-- expop    ::= ^
-- factor   ::= nat | ( expr )
-- expop has precedence over addop
expr :: Parser Int
expr = term `chainl1` addop

term :: Parser Int
term = factor `chainr1` expop

ops :: [(Parser a,b)] -> Parser b
ops xs = foldr1 plus [p `bind` \_ -> result op | (p,op) <- xs]

addop :: Parser (Int -> Int -> Int)
addop = ops [(char '+',(+)),(char '-',(-))]

expop :: Parser (Int -> Int -> Int)
expop = ops [(char '^',(^))]

factor :: Parser Int
factor =
  nat `plus`
  bracket (char '(')
          expr
          (char ')')

---- Lexical issues ------------------------------------------------
spaces :: Parser ()
spaces = many1 (sat isSpace) `bind` \_ -> result ()

comment :: Parser ()
comment = singleLineComment `plus` multilineComments

singleLineComment :: Parser ()
singleLineComment =
  string "--" `bind` \_ -> many (sat (\x -> x /= '\n')) `bind` \_ -> result ()

multilineComments :: Parser ()
multilineComments =
  string "{-" `bind`
  \_ ->
    many (sat (\x -> isSpace x || isPrint x)) `bind`
    \_ ->
      --many (alphanum `plus` char ' ' `plus` char '\n') `bind` \_ ->
      string "-}" `bind`
      \_ -> result ()

junk :: Parser ()
junk = many (spaces +++ comment) `bind` \_ -> result ()

parse :: Parser a -> Parser a
parse p = junk `bind` \_ -> p `bind` \v -> result v

token :: Parser a -> Parser a
token p = p `bind` \v -> junk `bind` \_ -> result v

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

identifier :: [String] -> Parser String
identifier ks =
  token (ident `bind`
         \xs ->
           if xs `notElem` ks
              then result xs
              else result "")

----- A parser for λ-expressions ------------------------------------
data Expr = App Expr Expr           -- applications
          | Lam String Expr         -- lambda abstraction
          | Let String Expr Expr    -- local definition
          | Var String              -- variable
          deriving (Show,Eq)

lambdaExpr :: Parser Expr
lambdaExpr = atom `chainl1` result App

atom = lam +++ local +++ var +++ paren

lam =
  symbol "\\" `bind`
  \_ ->
    variable `bind`
    \x -> symbol "->" `bind` \_ -> lambdaExpr `bind` \e -> result (Lam x e)

local =
  symbol "let" `bind`
  \_ ->
    variable `bind`
    \x ->
      symbol "=" `bind`
      \_ ->
        lambdaExpr `bind`
        \e ->
          symbol "in" `bind` \_ -> lambdaExpr `bind` \e' -> result (Let x e e')

var = variable `bind` \x -> result (Var x)

paren =
  bracket (symbol "(")
          lambdaExpr
          (symbol ")")

variable = identifier ["let","in"]
