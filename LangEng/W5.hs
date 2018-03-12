import Yoda

--parseMaybe (string "Hola como estas?") "Hola"
-- output: Nothing
-- parseMaybe (string "Hola") "Hola"
-- output: Just "Hola"
--parse (many(string "Hola")) "test" "HolaHola"
-- output: Right ["Hola","Hola"]
--parseMaybe (many(string "Hola")) "HolaHola"
-- output: Just ["Hola","Hola"]
--parse (some(string "Hola"))"Test" "jHola"
--Left (TrivialError (SourcePos {sourceName = "Test", sourceLine = Pos 1, sourceColumn = Pos 1} :| []) (Just (Tokens ('j' :| "Hol"))) (fromList [Tokens ('H' :| "ola")]))
--Some must have at least one match otherwise it will reach an error
whitespace :: Parser ()
whitespace = () <$ many (oneOf " \t\n")

whitespaces :: Parser ()
whitespaces = whitespace <|> commas

commas :: Parser()
commas = () <$ many (oneOf ",;")

tok :: String -> Parser String
tok x = whitespace *> string x <* whitespace

number:: Parser Int
number = read <$> some (oneOf "0123456789")

--Robot Parser
data Robot = Forward Int
            |L
            |R
            |S
            deriving Show

parserRobot :: Parser Robot
parserRobot = Forward <$ tok "forward" <*> number
            <|>  L <$ tok "rotate left"
            <|>  R <$ tok "rotate right"
            <|>  S <$ tok "stop"

data DataB =  B Int DataB
            | A Int
            | D
            deriving Show

parseBad :: Parser DataB
parseBad =      B <$ tok "b" <*> number <*> parseBad
            <|> A <$ tok "a" <*> number
            <|> D <$ tok "d"
