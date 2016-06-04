#!runhaskell -isrc
-- vim: set filetype=haskell:

module Main (main) where

import Control.Monad             ( when )
import Options.Applicative       ( (<>)
                                 , argument
                                 , execParser
                                 , fullDesc
                                 , header
                                 , help
                                 , info
                                 , long
                                 , metavar
                                 , optional
                                 , progDesc
                                 , short
                                 , str
                                 , strOption
                                 )
import Options.Applicative.Extra ( helper )
import Options.Applicative.Types ( Parser )
import System.Directory          ( copyFile, removeFile )

import qualified MapGroup ( mapGroup )

-- Main --

main :: IO ()
main = execParser opts >>= mapGroup
   where opts = info (helper <*> mapGroupParser)
                     ( fullDesc
                    <> progDesc "map a command to groups of consecutive lines matching a pattern"
                    <> header "mapgroup"
                     )

-- Commands --

mapGroupParser :: Parser MapGroup
mapGroupParser = MapGroup
   <$> extensionParser
   <*> commandParser
   <*> patternParser
   <*> fileParser

-- Options --

extensionParser :: Parser (Maybe String)
extensionParser = optional $ strOption
   ( long "extension"
  <> short 's'
  <> metavar "EXTENSION"
  <> help "extension for the backup file (.orig by default) or 'none' to not retain a backup of the original file"
   )

-- Positional Arguments --

commandParser :: Parser String
commandParser = argument str (metavar "COMMAND" <> help "full path of the command to execute by group")

patternParser :: Parser String
patternParser = argument str (metavar "PATTERN" <> help "grouping pattern, metacharacters allowed")

fileParser :: Parser FilePath
fileParser = argument str (metavar "FILE" <> help "file to process")

-- Logic --

data MapGroup = MapGroup
   { extension :: Maybe String
   , command   :: String
   , pattern   :: String
   , file      :: FilePath
   } deriving Show

mapGroup :: MapGroup -> IO ()
mapGroup g =
       copyFile (file g) bak
   >>  readFile bak
   >>= MapGroup.mapGroup (command g) (pattern g) . lines
   >>= writeFile (file g) . unlines
   >>  when none (removeFile bak)
   where ext  = maybe ".orig"
                      (\s -> if s == "none" then ".__none__" else s)
                      (extension g)
                   -- none suffix collisions should be avoided
         bak  = (file g) ++ ext
         none = maybe False (== "none") (extension g)
