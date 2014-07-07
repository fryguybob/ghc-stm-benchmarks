#!/u/ryates/local/bin/runhaskell
module Process where

import Text.PrettyPrint.Boxes
import Control.Applicative
import Data.List

grep n [] = []
grep n (l:ls) = 
    case words l of
        ws | any (== n) ws -> ws : grep n ls
        _                  -> grep n ls

cut fs ls = map (ls!!) fs

process ls = map (map unwords) . map ($ ls) $
    [ map (cut [4..9]) . grep "Performance" 
    , map (cut [0])    . grep "time"
    , map (cut [0])    . grep "cpu/tx-start/"
    , map (cut [0])    . grep "cpu/tx-capacity/"
    , map (cut [0])    . grep "cpu/tx-conflict/"
    ]

leftRights (a:b:as) = vcat left a : vcat left b : map (vcat right) as


main = do
    ls <- lines <$> readFile "log.txt"

    let fields = ["Name","Time","Start","Conflict","Capacity"]

    let ds = transpose $ fields : transpose (process ls)

    printBox . hsep 1 top . leftRights . map (map text) $ ds
    
