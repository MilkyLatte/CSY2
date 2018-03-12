import Yoda
import Data.Monoid

data Chess = Turn Move Move Chess
            | EndGame
      deriving Show

data Move = Move MoveP Quant
          | Cslt Bool
          | End Winner
      deriving Show

data Quant =  Prom Piece Quant
            | Chck Quant
            | Null
        deriving Show

data MoveP = Alg Piece Cell
            | Smh Cell Cell
            | AlgDis Piece Cell Cell
            | Tke Piece Cell
        deriving Show


data Winner = White
            | Black
            | Draw
            | AO
        deriving Show

data Cell = Cell Char Int
        deriving(Show, Eq)

data Piece = King
           | Queen
           | Rook
           | Knight
           | Bishop
           | Pawn
        deriving (Show, Eq)

whitespace :: Parser ()
whitespace = () <$ many (oneOf " .\t\n")

tok :: String -> Parser String
tok x = whitespace *> string x <* whitespace

number :: Parser Int
number = read <$> some (oneOf "12345678")

parsePlus :: Parser Char
parsePlus = char '+'

parseTke :: Parser Char
parseTke = char 'x'

parseCheck :: Parser Char
parseCheck = oneOf "+#"

charac :: Parser Char
charac = oneOf "abcdefgh"

parseCell :: Parser Cell
parseCell = Cell <$> charac <*> number

parsePiece :: Parser Piece
parsePiece =  King <$ char 'K'
            <|> Queen <$ char 'Q'
            <|> Rook  <$ char 'R'
            <|> Knight <$ char 'N'
            <|> Bishop <$ char 'B'
            <|> Pawn <$ pure ()

parsePieceNotPawn :: Parser Piece
parsePieceNotPawn = King <$ char 'K'
                 <|> Queen <$ char 'Q'
                 <|> Rook  <$ char 'R'
                 <|> Knight <$ char 'N'
                 <|> Bishop <$ char 'B'


parseNothing :: Parser ()
parseNothing = pure ()

parseMoveP :: Parser MoveP
parseMoveP =  Tke     <$> parsePiece <* parseTke <*> parseCell
          <|> Smh    <$> parseCell <*> parseCell
          <|> AlgDis <$> parsePiece <*> parseCell <*> parseCell
          <|> Alg    <$> parsePiece <*> parseCell

parseQuant :: Parser Quant
parseQuant = Prom <$> parsePieceNotPawn <*> parseQuant
          <|> Chck <$ parseCheck <*> parseQuant
          <|> Null <$ pure ()

parseWinner :: Parser Winner
parseWinner = White <$ string "1-0"
           <|> Black <$ string "0-1"
           <|> Draw  <$ string "0-0"
           <|> AO    <$ pure ()

parseCslt :: Parser Bool
parseCslt =  True  <$ string "0-0-0"
         <|> False <$ string "0-0"


parseMove :: Parser Move
parseMove = Move    <$> parseMoveP <*> parseQuant
         <|> Cslt   <$> parseCslt
         <|> End    <$> parseWinner


parseChess :: Parser Chess
parseChess = Turn <$ number <* whitespace <*> parseMove <* whitespace <*> parseMove <* whitespace  <*> parseChess
          <|> EndGame <$ pure ()
