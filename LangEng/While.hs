module While where

import Yoda



data Aexp
  = N Int
  | Var Var
  | Aexp :+: Aexp
  | Aexp :*: Aexp
  | Aexp :-: Aexp
  deriving Show

data Bexp
  = T
  | F
  | Aexp :=: Aexp
  | Aexp :<=: Aexp
  | Bexp :&&: Bexp
  | Not Bexp
  deriving Show

type Var = String

data Stm
  = Var := Aexp
  | Skip
  | Stm :> Stm
  | If Bexp Stm Stm
  | While Bexp Stm
  | Begin Dec Stm
  deriving Show

data Dec
   = Cont Var Aexp Dec
   | Empty
 deriving Show

type Z = Int
type T = Bool
type State = Var -> Z


{-
-- The code below implements left-associative operations in decreasing
-- precedence. It is superceded by the `precedence` function below.


aexp, aexp', aexp'', aexp''' :: Parser Aexp
aexp    = chainl aexp'   ((:*:) <$ tok "*")
aexp'   = chainl aexp''  ((:+:) <$ tok "+")
aexp''  = chainl aexp''' ((:-:) <$ tok "-")
aexp''' = Num <$> num
     <|> Var <$> var
     <|> tok "(" *> aexp <* tok ")"
-}

precedence :: [[Parser (a -> a -> a)]] -> Parser a -> Parser a
precedence ops arg = foldl build arg ops
  where build term ops = chainl term (asum ops)

aexp = precedence [[(:*:) <$ tok "*"]
                  ,[(:+:) <$ tok "+", (:-:) <$ tok "-"
                  ]]
     $ N <$> num
   <|> Var <$> var
   <|> tok "(" *> aexp <* tok ")"

bexp :: Parser Bexp
bexp = precedence [[(:&&:) <$ tok "&"]]
      $ T <$ tok "true"
    <|> F <$ tok "false"
    <|> (:=:) <$> aexp <* tok "=" <*> aexp
    <|> (:<=:) <$> aexp <* tok "<=" <*> aexp
    <|> Not <$ tok "!" <*> bexp
    <|> tok "(" *> bexp <* tok ")"

stms :: Parser Stm
stms = chainl stm ((:>) <$ tok ";")

stm = (:=) <$> var <* tok ":=" <*> aexp
   <|> Skip <$  tok "skip"
   <|> If <$ tok "if" <*> bexp <* tok "then" <*> stm <* tok "else" <*> stm
   <|> While <$ tok "while" <*> bexp <* tok "do" <*> stm
   <|> Begin <$ tok "begin" <*> decl <*> stm <* tok "end"
   <|> tok "(" *> stms <* tok ")"

decl :: Parser Dec
decl = undefined

chainl p op = p >>= rest where
  rest x = do f <- op
              y <- p
              rest (f x y)
       <|> return x

num :: Parser Int
num = read <$> some (oneOf ['0' .. '9']) <* whitespace

var :: Parser String
var = some (oneOf ['a' .. 'z']) <* whitespace

whitespace :: Parser ()
whitespace = () <$ many (oneOf " \t\n\r")

tok :: String -> Parser String
tok t = string t <* whitespace


--Semantics
nval :: Int -> Z
nval = id

avaluation :: Aexp -> State -> Z
avaluation (N n) s = nval n
avaluation (Var x) s = s x
avaluation (a1 :*: a2)  s = (avaluation a1 s) * (avaluation a2 s)
avaluation (a1 :+: a2)  s = (avaluation a1 s) + (avaluation a2 s)
avaluation (a1 :-: a2)  s = (avaluation a1 s) - (avaluation a2 s)

bvaluation :: Bexp -> State -> T
bvaluation T  s = True
bvaluation F  s = False
bvaluation (a :=: b)  s = (avaluation a s) == (avaluation b s)
bvaluation (a :<=: b) s = (avaluation a s) <= (avaluation b s)
bvaluation (a :&&: b) s = (bvaluation a s) && (bvaluation b s)
bvaluation (Not a) s    = not(bvaluation a s)

-- TEST VARIABLES
a :: Aexp
a = (Var "x" :+: Var "y") :*: (Var "z" :-: N 1)

s:: State
s "x" = 1
s "y" = 2
s "z" = 3
s _   = 0

bvaluation :: Bexp -> State -> T
bvaluation T  s = True
bvaluation F  s = False
bvaluation (a :=: b)  s = (avaluation a s) == (avaluation b s)
bvaluation (a :<=: b) s = (avaluation a s) <= (avaluation b s)
bvaluation (a :&&: b) s = (bvaluation a s) && (bvaluation b s)
bvaluation (Not a) s    = not(bvaluation a s)
