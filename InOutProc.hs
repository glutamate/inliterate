module Main where

import Language.Haskell.Exts
--import Language.Haskell.Exts.Pretty
import Data.Maybe
import System.Environment
import Text.PrettyPrint
import InOut

main = do
  
  original:src:out:_ <- getArgs
  mmod <- fromParseResult `fmap` parseFile src
  --(print) $  mmod
  writeFile out $ prettyPrint $ addImport "System.IO.Unsafe" "SysIOUnface"  $ onDecls inoutxform mmod

 