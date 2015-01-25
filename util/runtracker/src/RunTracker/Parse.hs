{-# LANGUAGE TemplateHaskell #-}
module RunTracker.Parse 
    ( parseStatsSTM
    , parseRunsFromFile
    , parseStatsHTM
    , statsHTM
    , manyStatsHTM
    , testParser
    , testParserFile
    , ParsedRun(..)
    ) where

import Control.Applicative hiding (many)
import Control.Lens
import Control.Monad

import Data.Char (isSpace)
import Data.Default.Class
import Data.List
import Data.List.Split (chunksOf)
import Data.Maybe
import Data.Time

import System.Directory
import System.IO

import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (haskellDef)
import Text.Parsec.Prim (parserFail)

import qualified Text.PrettyPrint.Boxes as B
import Text.PrettyPrint.Boxes ((<+>))

import RunTracker.Database


data ParsedRun = ParsedRun
    { _filePath     :: FilePath
    , _flags        :: [(String,Int)]
    , _parsedConfig :: String
    , _run          :: Run
    , _threads      :: Int
    , _stats        :: Stats
    , _htmStats     :: Maybe StatsHTM
    , _stmStats     :: [StatsSTM]
    }
    deriving (Show, Eq)

makeLenses ''ParsedRun

instance Default ParsedRun where
    def = ParsedRun "" [] "" def 0 def Nothing []

trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

lexer = P.makeTokenParser haskellDef

float      = P.float lexer
whiteSpace = P.whiteSpace lexer
integer    = P.integer lexer


data Table a = Table { _columns :: [String], _widths :: [Int], _data :: [[a]] }

instance Show a => Show (Table a) where
--  show (Table hs rs) = unwords hs ++ "\n" ++ (unlines . map (unwords . map show)) rs 
  show (Table hs ws rs) = show ws ++ "\n" 
                       ++ (B.render . B.hsep 2 B.top . map (B.vcat B.right) 
                          . zipWith (:) (map B.text hs) $ cs)
    where
      cs = map (map (B.text . showComma)) $ transpose rs

      showComma = reverse . concat . intersperse "," . chunksOf 3 . reverse . show

spaces :: Parser ()
spaces = pure () <* many (oneOf " \t") <?> "spaces"

word :: Parser String
word = many1 (choice [letter,char '-']) <?> "word"

sepByWithWidths :: Parser String -> Parser [sep] -> Parser ([String],[Int])
sepByWithWidths word sep = do
    is <- sep <?> "initial spaces"
    w <- word
    s <- sep

    (ws,ss) <- unzip <$> many ((,) <$> word <*> sep)

    let ws' = w:ws
        widths = zipWith (+) (map length ws') (map length ss)

    return (ws', over _head (+length is) widths)

headers :: Parser ([String],[Int])
headers = word `sepByWithWidths` many (char ' ') <?> "headers"

table :: a -> Parser a -> Parser (Table a)
table z cell = do
    (hs,ws) <- headers <* newline
    rs <- many1 row

    return (Table hs ws rs)

  where
    row = spaces *> cell `sepBy` spaces <* newline

commaNumber :: Parser Int
commaNumber = read . concat <$> (many1 digit `sepBy1` char ',')

perfEntries :: Parser [(String,Int)]
perfEntries = do
    manyTill perfEntry newline

perfEntry :: Parser (String,Int)
perfEntry = do
    whiteSpace                    <?> "Perf entry leading whitespace"
    n <- commaNumber              <?> "Perf value"
    whiteSpace                    <?> "Perf entry separator"
    k <- manyTill anyChar newline <?> "Perf entry key"
    return (trim k,n)

perfTime :: Parser Double
perfTime = do
    whiteSpace                    <?> "Perf time leading whitespace"
    n <- float                    <?> "Perf time value"
    string "seconds time elapsed" <?> "Perf time units"
    return n

statsHTM :: Parser (String,Double,StatsHTM)
statsHTM = do
    cmdLine <- (string "Performance counter stats for '" <?> "Perf header")
            *> (manyTill anyChar (char '\'') <* string ":" <?> "cmdline")
    (newline >> newline) <?> "Perf header gap"
    es <- perfEntries    <?> "Perf entry"
    t <- perfTime        <?> "Perf time"

    case catMaybes . map (`lookup` es) $ ["cpu/tx-start/", "cpu/tx-capacity/", "cpu/tx-conflict/"] of
        [s,cap,conf] -> return (cmdLine, t, StatsHTM Nothing 0 s cap conf)
        _            -> parserFail $ "Expected start, capacity, and conflict.  Saw: " ++ show es

manyStatsHTM :: Parser [(String,Double,StatsHTM)]
manyStatsHTM = whiteSpace *> many (statsHTM <* whiteSpace)

askUser :: Read a => String -> IO a
askUser prompt = do
    putStr prompt
    hFlush stdout
    l <- getLine

    case reads (trim l) of
        [(v,[])] -> return v
        _        -> putStrLn "Failed to parse input." >> askUser prompt

parseCmdLine :: String -> [(String,Int)]
parseCmdLine s = either [] id $ parse parseCmdLine "(command line)" s
  where
    -- TODO skip other stuff.
    parseFlag = do
        f <- char '-' *> word
        whiteSpace
        n <- integer
        whiteSpace
        return (f, fromIntegral n)

    parseCmdLine = do
       manyTill anyChar whiteSpace
       whiteSpace
       fs <- many parseFlag
       return fs
 


parseRun :: Parser ParsedRun
parseRun = do
--    stm <- try statsSTM
    htm <- optionMaybe statsHTM

    case htm of
      Just (c, t, stats) -> do
        return $ 
          ( def & cmdLine .~ c
          , def & testRuntime .~ t
          , case stats of
              StatsHTM _ _ 0 0 0 -> Nothing
              _                  -> Just stats
          , [] -- stm
          )
      Nothing -> return (def, def, Nothing, [])

parseRuns :: Parser [ParsedRun]
parseRuns = whiteSpace *> many (parseRun <* whiteSpace)
    

parseRunsFromFile :: FilePath -> IO [ParsedRun]
parseRunsFromFile f = parseFromFile parseRuns f >>= either (error . show) handle
  where
    handle rs = do
        t <- getModificationTime f

        -- Find all the instances where we can't get certain information,
        -- then adjust things.
        
        rs' <- forM rs $ \r -> do
            
            ts <- lookupOrAsk "thread count" (r^.cmdLine) ["-threads","-t"] (r^.flags)
            conf <- maybe (ask "configuration" (r^.cmdLine)) return $ getConfig (r^.cmdLine)
            
            return $ r & run.date .~ t
                       & run.threads .~ ts
                       & parsedConfig .~ conf
        
        lookupOrAsk 

        flags <- parseOrAskCmdLine c

        return (rs <&> _1.date .~ t)

    ask s c = askUser $ unlines [ "Looking for " ++ s ++ " from command line:"
                                , "   $ " ++ c
                                , "> "
                                ]

    lookupOrAsk s c ks fs = maybe (ask s c) id . listToMaybe . catMaybes . map (`lookup` fs) $ ks


        -- From the file path we will try to get the n


parseSTMTable :: String -> Either ParseError (Table Int)
parseSTMTable s = parse (table (-1) commaNumber) "(unknown)" s

parseStatsHTM :: String -> Either ParseError (String,Double,StatsHTM)
parseStatsHTM s = parse statsHTM "(unknown)" s

parseStatsSTM :: String -> Either ParseError StatsSTM
parseStatsSTM s = Right $ StatsSTM Nothing 0  0 0 0 0

getLines :: IO [String]
getLines = reverse <$> go []
  where
    go ls = do
        l <- getLine
        case l of
            "" -> return ls
            _  -> go (l:ls)

testParser :: Show a => (String -> Either ParseError a) -> IO ()
testParser p = do
    putStrLn "Input:"
    ls <- getLines

    case p (unlines ls) of
        Left  e -> putStrLn "Error: " >> print e
        Right r -> print r

testParserFile :: Show a => Parser a -> FilePath -> IO a
testParserFile p f = do
    r <- parseFromFile p f
    case r of
        Left  e -> error ("Error: " ++ show e)
        Right r -> return r
    
    
