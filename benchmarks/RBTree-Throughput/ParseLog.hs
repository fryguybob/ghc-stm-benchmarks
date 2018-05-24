{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE ViewPatterns    #-}
module ParseLog
    ( Run(..)
    , CmdEntry(..)
    , Perf(..)
    , Table(..)

    , runs
    , testParserFile

    , benchQuery
    , perfQuery
    , cmdLineQuery
    , heapQuery

    , selectProg
    , selectCap
    ) where

import Control.Applicative hiding (many, (<|>))
import Control.Lens hiding (noneOf)
import Control.Monad

import qualified Data.Aeson as A
import Data.Aeson (ToJSON(..), genericToEncoding, defaultOptions, fieldLabelModifier)
import Data.Char (isSpace)
import Data.Default.Class
import Data.List
import Data.List.Split (chunksOf)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Time
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Vector as V

import System.Directory
import System.IO
import System.Environment (getArgs)

import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (haskellDef)
import Text.Parsec.Prim (parserFail)

import qualified Text.PrettyPrint.Boxes as B
import Text.PrettyPrint.Boxes ((<+>))

import GHC.Generics (Generic)

-- A typical log file contains runs with some or all of the following parts:
--  -  benchdata: output from the test
--  -  STM stats: output from RTS STM
--  -  HTM stats: output from RTS HTM
--  -  sizes: debugging RTS output giving the sizes of various structures
--  -  perf: stats collected by perf
--
-- benchdata:
--   one line starting with "benchdata: " of key-value pairs.  The keys and values
--   have no spaces.
--
-- STM stats:  Start with two lines then a variable header line.  Unfortunately some headers have
--   spaces, but no consecutive spaces.  They are aligned, but for parsing it is probably easier to
--   just delimit by a pair of spaces.  The header is followed by a row for each thread and a final
--   summary row that is missing a thread id.  These are space delimited numbers with commas.
--
-- HTM stats:  Same, but with different fields.
--
-- sizes:  header line, column header then one row of data.  Columns are not aligned :(.
--
-- Heap stats: A line for STM-committed, STM-aborted, and HTM.  Each with heap
--   pointer allocation, allocate allocation, and both together.  Ending with the
--   reporting type (ave, min, max) and commit percentage.  These are summarys
--   of data that is already in the STM and HTM stats.
--
-- perf:  header line containing commandline followed by lines of value key pairs separated by spaces
--   ending with running time line ending in "seconds time elapsed".

data Table a = Table { _columns :: [String], _rows :: [[a]] }
    deriving (Show, Generic)

instance ToJSON a => ToJSON (Table a) where
--    toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = drop 1 }

    toJSON     (Table (map T.pack -> cs) rs) = A.Array $ V.fromList $ map    (A.object . zipWith (A..=) cs) rs
--    toEncoding (Table (map T.pack -> cs) rs) = foldr1 (A.><) $ map (A.pairs . foldr1 (<>) . zipWith (A..=) cs) rs

table :: Parser a -> Parser (Table a)
table dataParser = Table
                    <$> (header <* newline)
                    <*> (many1 (spaces *> many1Till (dataParser <* spaces) newline))

many1Till :: Parser a -> Parser end -> Parser [a]
many1Till p e = (:) <$> p <*> manyTill p e

colTitle :: Parser String
colTitle = spaces *> nospace1 <?> "col title"
  where
    nospace = nospace1 <|> return ""
    nospace1 = do
        c <- noneOf "\n"
        case c of
          ' ' -> sawSpace
          _   -> (c:) <$> nospace
    sawSpace = do
        c <- noneOf "\n"
        case c of
          ' ' -> many (char ' ') >> return ""
          _   -> (' ':) . (c:) <$> nospace

header :: Parser [String]
header = many1 colTitle <?> "header"

spaces :: Parser ()
spaces = pure () <* many (oneOf " \t") <?> "spaces"

word :: Parser String
word = many1 (choice [letter,char '-']) <?> "word"

commaNumber :: Parser Int
commaNumber = read . concat <$> (many1 digit `sepBy1` char ',') <?> "comma number"

number :: Parser Int
number = read <$> many1 digit <?> "number"

lexer = P.makeTokenParser haskellDef
float = P.float lexer

matchLine :: String -> Parser ()
matchLine s = string s >> newline >> return ()

title :: String -> Parser ()
title s = do
    matchLine s
    (many1 (char '-') >> newline >> return ()) <|> return ()

stmStats :: Parser (Table Int)
stmStats = title "STM stats:" *> table commaNumber <?> "STM stats"

htmStats :: Parser (Table Int)
htmStats = title "HTM stats:" *> table commaNumber <?> "HTM stats"

sizeStats :: Parser (Table Int)
sizeStats = title "Sizes:" *> table commaNumber <?> "Sizes"

bloomStats :: Parser (Table Int)
bloomStats = title "Bloom stats:" *> table commaNumber <?> "Bloom stats"

numberOrInf :: Parser Int
numberOrInf = (string "inf" *> return 0) <|> number

heapStats :: Parser (Table Int)
heapStats = do 
    title "Heap stats:"
    rs <- sequence . map row $ ["STM-committed:", "STM-aborted  :", "HTM          :"]
    newline
    return $ Table ["Hp-alloc", "allocate", "both"] rs
  where 
    row s = do
        string s >> spaces >> string "Hp-alloc" >> spaces
        hp <- numberOrInf
        char ',' >> spaces >> string "allocate" >> spaces
        al <- numberOrInf
        char ',' >> spaces >> string "both"     >> spaces
        bo <- numberOrInf
        manyTill anyChar (try newline)
        return [hp,al,bo]
-- STM-committed: Hp-alloc         21, allocate          0, both         21 ave  100%
-- STM-aborted  : Hp-alloc         28, allocate          0, both         28 ave
-- HTM          : Hp-alloc          0, allocate          0, both          0 ave    0%
-- EOF


data Perf = Perf { _program :: String
                 , _cmdLine :: [(String, String)]
                 , _values :: [(String, Int)]
                 , _time :: Double
                 }
    deriving (Show, Generic)

instance ToJSON Perf where
    toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = drop 1 }

data CmdEntry = Key String | Value String
    deriving (Show, Eq)

-- If the entry starts with '-' or '+' it is a key.  Later
-- we will take list of CmdEntry and make key-value pairs.
cmdLineEntry :: Parser CmdEntry
cmdLineEntry = do
    spaces
    ((char '-' <|> char '+') >> Key <$> entry)  <|> Value <$> entry
  where
    entry = many1 (choice [char '-', char '=', alphaNum])

-- We can have keys on their own, or keys and values.
-- TODO: Real programs also have arguments :D
-- TODO: split keys with '=' in them
-- TODO: be +RTS aware?
mkKeyValues :: [CmdEntry] -> [(String, String)]
mkKeyValues (Key k:Value v:es) = (k,v):mkKeyValues es
mkKeyValues (Key k:es@(Key _:_)) = (k,""):mkKeyValues es
mkKeyValues [Key k] = [(k,"")]
mkKeyValues es = error ("Invalid command line.  Saw: " ++ show es)

perfStats :: Parser Perf
perfStats = do
    string " Performance counter stats for '"
    p <- manyTill anyChar (char ' ') -- TODO: this could be better.
    s <- manyTill cmdLineEntry (char '\'')
    char ':' >> newline >> newline

    ks <- manyTill valueKey newline

    t <- spaces *> float
    string "seconds time elapsed"
    newline
    newline
    return $ Perf p (mkKeyValues s) ks t

benchdata :: Parser [(String,String)]
benchdata = do
    string "benchdata: "
    many1 pair <* newline
  where
    pair = do
        spaces
        k <- manyTill (noneOf "\n ") (char ' ')
        spaces
        v <- many1 (noneOf "\n ")
        return (k,v)

valueKey = spaces *> (flip (,) <$> commaNumber <*> (spaces *> word)
                            <* option "" (char ':' *> word) <* spaces <* newline)

data Run = Run { _bench :: [(String,String)]
               , _stm   :: Table Int
               , _htm   :: Maybe (Table Int)
               , _bloom :: Maybe (Table Int)
               , _sizes :: Maybe (Table Int)
               , _heap  :: Maybe (Table Int)
               , _perf  :: Perf
               }
    deriving (Show, Generic)

instance ToJSON Run where
    toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = drop 1 }

makeLenses ''Table
makeLenses ''Run
makeLenses ''Perf

benchQuery :: String -> Run -> Maybe String
benchQuery k r = r^.bench.to (lookup k)

heapQuery :: String -> Run -> Maybe [Int]
heapQuery k r = do
    h <- r^.heap
    i <- elemIndex k (h^.columns)
    sequence $ map (^? ix i) (h^.rows)

selectProg :: String -> [Run] -> [Run]
selectProg p = filter (\r -> benchQuery "prog" r == Just p)

-- Assumes first column is Cap.
selectCap :: Maybe Int -> Table Int -> Maybe (M.Map String Int)
selectCap Nothing  t = Just . M.fromList $ zip (t^.columns.to tail) (t^.rows.to last)
selectCap (Just c) t = do
    r <- find (\r -> head r == c) (t^.rows)
    return . M.fromList $ zip (t^.columns) r

tmCounts :: M.Map String Int -> Maybe (M.Map String Int)
tmCounts m = do
    -- The hybrid uses HTM in the following way:
    --
    -- Fast-path                        fallback                                   fallback-commit
    --
    -- count(Starts)
    --
    --   for 1..htm-retry
    --     XBEGIN
    --      FAIL -> count(HTM-fail)
    --                fallback or loop    count(HTM-fallback)
    --      XEND -> count(HTM-commit)        
    --
    --                                    commit:
    --                                     for 1..hle-retry
    --                                       XBEGIN
    --                                         FAIL -> count(HLE-fail)
    --                                                   fallback-commit or loop    count(HLE-fallback)
    --                                         XEND -> count(HLE-commit)
    --
    -- perf counts starts, commits, aborts.  Abort due to conflict appears to count too much! 
    -- 
    -- HTM-fail + HLE-fail = total aborts
    -- HTM-commit + HLE-commit = total commits
    -- total aborts + total commits = total starts
    --
    htmC <- "HTM-commit" `M.lookup` m
    hleC <- "HLE-commit" `M.lookup` m
    htmF <- "HTM-fail" `M.lookup` m
    hleF <- "HLE-fail" `M.lookup` m

    let ta = htmF + hleF
        tc = htmC + hleC

    return (M.fromList [ ("total aborts",  ta)
                       , ("total commits", tc)
                       , ("total starts",  ta + tc)
                       ])



--  checkTMCounts :: Run -> IO ()
--  checkTMCounts r = do
--      let s = selectCap Nothing r^.stm
--          h = r^.htm >>= selectCap Nothing
--          c = s `M.merge` h
--  
--      tmCounts c >>= print

perfQuery :: String -> Run -> Maybe Int
perfQuery k r = r^.perf.values.to (lookup k)

cmdLineQuery :: String -> Run -> Maybe String
cmdLineQuery k r = r^.perf.cmdLine.to (lookup k)

run = Run <$> benchdata
          <*> stmStats
          <*> optionMaybe htmStats
          <*> optionMaybe bloomStats
          <*> optionMaybe sizeStats
          <*> optionMaybe heapStats
          <*> (newline *> perfStats)
runs = many1 run

parseBenchdata = parse benchdata "(unknown)"
parseHeader = parse header "(unknown)"
parseTable = parse (table commaNumber) "(unknown)"
parseRun = parse run "(unknown)"
parseRuns = parse runs "(unknown)"
parseCmdLine = parse (mkKeyValues <$> (many1 cmdLineEntry)) "(unknown)"

testParser :: Show a => (String -> Either ParseError a) -> IO a
testParser p = do
    putStrLn "Input:"
    ls <- getLines

    case p (unlines ls) of
        Left  e -> error ("Error: " ++ show e)
        Right r -> return r

testParserFile :: Show a => Parser a -> FilePath -> IO a
testParserFile p f = do
    r <- parseFromFile p f
    case r of
        Left  e -> error ("Error: " ++ show e)
        Right r -> return r

getLines :: IO [String]
getLines = reverse <$> go []
  where
    go ls = do
        l <- getLine
        case l of
            "EOF" -> return ls
            _  -> go (l:ls)

{-
main = do
    [a] <- getArgs
    rs <- testParserFile runs a
    print $ map (cmdLineQuery "m") rs


main = do
    [a] <- getArgs
    rs <- testParserFile runs a

    let rs' = selectProg "Main-stmtrie-tstruct" rs

    print $ length rs'
-}
