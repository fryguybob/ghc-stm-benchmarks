{-# LANGUAGE TemplateHaskell #-}
import Control.Lens

import Graphics.Rendering.Chart.Simple
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Backend.Diagrams
import Diagrams.Prelude
import Diagrams.Backend.Postscript
import Diagrams.Backend.Postscript.CmdLine

import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char

import qualified Options.Applicative as O
import Options.Applicative hiding (Parser)

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

parseCount :: Parser Int
parseCount = read . filter (not . (== ',')) <$> many (digit <|> oneOf ",") <?> "Count"

parseLine :: Parser (String,Int)
parseLine = flip (,) <$> (spaces *> parseCount <* spaces) <*> (many (noneOf "\n ") <* many (char ' ') <* newline) <?> "Line"

parseTime :: Parser Double
parseTime =  read . filter (not . (== ',')) 
         <$> (spaces *> many (digit <|> oneOf ",.")
                    <* string " seconds time elapsed" <* newline) <?> "Time"

parsePerf :: Parser Perf
parsePerf = Perf <$> (string " Performance counter stats for '" 
                  *> many (noneOf "'") <* string "':" <* newline <* newline)
                 <*> (M.fromList <$> manyTill parseLine newline)
                 <*> parseTime <* many newline <?> "Perf"

parseResults :: Parser [Perf]
parseResults = many newline *> many parsePerf <* eof
            <?> "Perfs"

check rs = case partitionEithers rs of
    ([],ds) -> Right ds
    (ls,_)  -> Left (unlines . map show $ ls)

testParse s = do
    rs <- parseFromFile parseResults s
    return rs

fromJust' m (Just x) = x
fromJust' m _        = error (show m)

getData :: String -> Perf -> Double
getData "time" (Perf n m t) = t
getData "commit" (Perf n m t)
    = case ("cpu/tx-commit/" `M.lookup` m) of
        (Just s) -> fromIntegral s
        _        -> error ("missing data." ++ show m)
getData "start" (Perf n m t)
    = case ("cpu/tx-start/" `M.lookup` m) of
        (Just s) -> fromIntegral s
        _        -> error ("missing data." ++ show m)
getData "rate" (Perf n m t)
    = case ("cpu/tx-commit/" `M.lookup` m, "cpu/tx-start/" `M.lookup` m) of
        (Just c, Just s) -> fromIntegral c / fromIntegral s
        _                -> error ("missing data." ++ show m)
getData l _ = error ("Unknown query " ++ l)


data Options = Options
    { _output :: FilePath
    , _inputs :: [FilePath]
    , _query  :: String
    }

makeLenses ''Options

optionsParse :: O.Parser Options
optionsParse = Options
    <$> strOption (mconcat
        [ long "output"
        , short 'o'
        , help "Output file"
        ])
    <*> arguments1 Just (help "Input files")
    <*> strOption (mconcat
        [ long "query"
        , short 'q'
        , help "Query"
        ])

main = do
    prog <- getProgName
    let p = info (helper <*> optionsParser)
          $ mconcat [ fullDesc, progDesc "Perf plot.", header prog ]
    opts <- execParser p
    rs <- mapM (parseFromFile parseResults) (as^.inputs)

    case check rs of
      Left  e  -> putStrLn e
      Right ds' -> do

        let ds = map (map getData) ds' :: [[Double]]

        let l = fromIntegral (maximum . map length $ ds) :: Double

        e <- defaultEnv vectorAlignmentFns 600 600

        let (p,_) = runBackendR e $ layoutDddToRenderable $ plot [1.0..l] (ds!!0) -- (ds!!1)

        withArgs ["-o", "results.eps", "-w", "600"] $ defaultMain p

