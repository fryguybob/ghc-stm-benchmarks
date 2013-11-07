{-# LANGUAGE TemplateHaskell #-}
module Performance.Perf.Parse where

import Control.Lens

import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char

import Control.Monad
import Control.Applicative hiding (many, (<|>))
import Data.Maybe
import Data.Either
import Data.Function
import qualified Data.Map as M

import System.Environment

data Perf = Perf
    { _name :: String
    , _counts :: M.Map String Int
    , _time :: Double
    }
    deriving (Show)

makeLenses ''Perf

parseCount :: Parser Int
parseCount = read . filter (not . (== ',')) <$> many (digit <|> oneOf ",") <?> "Count"

parseLine :: Parser (String,Int)
parseLine = flip (,) <$> (spaces *> parseCount <* spaces) 
                     <*> (many (noneOf "\n ") <* many (char ' ') <* newline) 
                     <?> "Line"

parseTime :: Parser Double
parseTime =  read . filter (not . (== ',')) 
         <$> (spaces *> many (digit <|> oneOf ",.")
         <*  string " seconds time elapsed" <* newline) 
         <?> "Time"

parsePerf :: Parser Perf
parsePerf = Perf <$> (string " Performance counter stats for '" 
                  *> many (noneOf "'") <* string "':" <* newline <* newline)
                 <*> (M.fromList <$> manyTill parseLine newline)
                 <*> parseTime <* many newline <?> "Perf"

parseResults :: Parser [Perf]
parseResults = many newline *> many parsePerf <* eof
            <?> "Perfs"

aggregateResults :: Show a => [Either a b] -> Either String [b]
aggregateResults rs = case partitionEithers rs of
    ([],ds) -> Right ds
    (ls,_)  -> Left (unlines . map show $ ls)

parseInputs :: [FilePath] -> IO (Either String [[Perf]])
parseInputs fs = aggregateResults <$> mapM (parseFromFile parseResults) fs
