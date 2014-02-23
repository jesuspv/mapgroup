#!runhaskell -isrc
-- vim: set filetype=haskell:

module Main (main) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import qualified MapGroup (mapGroup)
import System.Console.CmdTheLine ( defTI
                                 , opt
                                 , optDoc
                                 , optInfo
                                 , optName
                                 , pos
                                 , posDoc
                                 , posInfo
                                 , posName
                                 , required
                                 , run
                                 , Term
                                 , TermInfo
                                 , termDoc
                                 , termName
                                 , value
                                 , version
                                 )
import System.Directory (copyFile, removeFile)

-- Main --

main :: IO ()
main = run (mapGroupT, mapGroupInfo)

-- Term Infos --

mapGroupInfo :: TermInfo
mapGroupInfo = defTI
   { termName = "mapgroup"
   , termDoc  = "map a command to groups of consecutive lines matching a pattern"
   , version  = "1.0"
   }

-- Terms --

mapGroupT :: Term (IO ())
mapGroupT = mapGroup <$> suffixT
                     <*> cmdT
                     <*> patternT
                     <*> fileT

-- Options --

suffixT :: Term (Maybe String)
suffixT = value $ opt Nothing (optInfo ["s", "suffix"])
   { optName = "SUFFIX"
   , optDoc  = "suffix for the backup file (.orig by default) or 'none' to not retain a backup of the original file"
   }

-- Positional Arguments --

cmdT :: Term String
cmdT = required $ pos 0 Nothing posInfo
   { posName = "COMMAND"
   , posDoc  = "command to execute by group"
   }

patternT :: Term String
patternT = required $ pos 1 Nothing posInfo
   { posName = "PATTERN"
   , posDoc  = "grouping pattern"
   }

fileT :: Term FilePath
fileT = required $ pos 2 Nothing posInfo
   { posName = "FILE"
   , posDoc  = "file to process"
   }

-- Commands --

mapGroup :: Maybe String -> String -> String -> FilePath -> IO ()
mapGroup suffix' cmd pattern file =
       copyFile file bak
   >>  readFile bak
   >>= MapGroup.mapGroup cmd pattern . lines
   >>= writeFile file . unlines
   >>  when none (removeFile bak)
   where suffix = maybe ".orig" (\s -> if s == "none" then ".__none__" else s) suffix' -- none suffix collitions should be avoided
         bak    = file ++ suffix
         none   = maybe False (== "none") suffix'
