module MapGroup (mapGroup) where

import Data.List (groupBy)
import Data.Tuple (swap)
import System.Process (readProcess)
import Text.Regex.Posix ((=~))

-- | map a command to groups of consecutive lines matching a pattern
mapGroup :: String   -- ^ filename
         -> String   -- ^ pattern
         -> [String] -- ^ lines
         -> IO [String]
mapGroup f g l = (sequence . map (either left right) . group g . lineno $ l) >>= return . concat
   where
      left ::(Integer, String) -> IO [String]
      left = return . return . snd

      right :: [(Integer, String)] -> IO [String]
      right x = readProcess (cmd f) (args f) (unlines . map snd $ x) >>= return . lines
      cmd  = head . words
      args = tail . words

-- | group
group :: String              -- ^ pattern
      -> [(Integer, String)] -- ^ lines including line numbers
      -> [Either (Integer,String) [(Integer, String)]]
group pattern = map (\ ls -> if length ls == 1
                             then Left  (head ls)
                             else Right ls
                    )
              . groupBy (\ (_, l) (_, l') -> (l =~ pattern) && (l' =~ pattern))

-- | append line numbers starting by one
lineno :: [String] -- ^ lines
       -> [(Integer,String)]
lineno = map swap . flip zip [1..]
