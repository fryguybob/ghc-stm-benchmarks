module RunTracker.Parse (parseSTMStats) where

import Control.Applicative hiding (many)
import Control.Monad

import Data.List
import Data.List.Split (chunksOf)

import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Token

import qualified Text.PrettyPrint.Boxes as B
import Text.PrettyPrint.Boxes ((<+>))


data Table a = Table { _columns :: [String], _widths :: [Int] , _data :: [[a]] }

instance Show a => Show (Table a) where
--  show (Table hs rs) = unwords hs ++ "\n" ++ (unlines . map (unwords . map show)) rs 
  show (Table hs ws rs) = show ws ++ "\n" 
                       ++ (B.render . B.hsep 2 B.top . map (B.vcat B.right) . zipWith (:) (map B.text hs) $ cs)
    where
      cs = map (map (B.text . showComma)) $ transpose rs

      showComma = reverse . concat . intersperse "," . chunksOf 3 . reverse . show

spaces :: Parser ()
spaces = pure () <* many (oneOf " \t") <?> "spaces"

word :: Parser String
word = many1 (choice [letter,char '-']) <?> "word"

sepBy1WithWidths :: Parser String -> Parser sep -> Parser ([String],[Int])
sepBy1WithWidths word sep = do
    
    w <- word
    (ss,ws) <- unzip . many ((,) <$> sep <*> word)

    return (ss,ws)

headers :: Parser [String]
headers = word `sepBy1WithWidths` spaces <?> "headers"

table :: a -> Parser a -> Parser (Table a)
table z cell = do
    (ws,hs) <- unzip <$> headers <* newline
    rs <- many1 row

    return (Table hs ws rs)

  where
    row = spaces *> cell `sepBy` spaces <* newline

commaNumber :: Parser Int
commaNumber = read . concat <$> (many1 digit `sepBy1` char ',')

parseSTMTable :: String -> Either ParseError (Table Int)
parseSTMTable s = parse (table (-1) commaNumber) "(unknown)" s

getLines :: IO [String]
getLines = reverse <$> go []
  where
    go ls = do
        l <- getLine
        case l of
            "" -> return ls
            _  -> go (l:ls)

main = do
    putStrLn "Input:"
    ls <- getLines

    case parseSTMTable (unlines ls) of
        Left e -> putStrLn "Error:" >> print e
        Right t -> print t

