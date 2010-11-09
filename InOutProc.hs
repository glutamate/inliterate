module Main where

import Language.Haskell.Exts
--import Language.Haskell.Exts.Pretty
import Data.Maybe
import System.Environment
import Text.PrettyPrint
import InOut

transform = filter (not . isSink) . addMain . map transSrc 

main = do
  
  original:src:out:_ <- getArgs
  mmod <- fromParseResult `fmap` parseFile src
  --(print) $  mmod
  writeFile out $ prettyPrint $ addImport impUnsafe $ onDecls transform mmod

 