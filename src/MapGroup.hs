module MapGroup (mapGroup) where

import Data.List (groupBy)
import Data.Tuple (swap)
import System.Process (readProcess)
import Text.Regex.Posix ((=~))

mapGroup :: String -> String -> [String] -> IO [String]
mapGroup f g l = (sequence . map (either left right) . group g . lineno $ l) >>= return . concat
   where left ::(Integer, String) -> IO [String]
         left = return . return .snd
         right :: [(Integer, String)] -> IO [String]
         right x = readProcess (cmd f) (args f) (unlines . map snd $ x) >>= return . lines
         cmd  = head . words
         args = tail . words

group :: String -> [(Integer, String)] -> [Either (Integer,String) [(Integer, String)]]
group pattern = map (\ ls -> if length ls == 1
                             then Left  (head ls)
                             else Right ls)
              . groupBy (\ (_,l) (_,l') -> (l =~ pattern) && (l' =~ pattern))

lineno :: [String] -> [(Integer,String)]
lineno = map swap . flip zip [1..]
