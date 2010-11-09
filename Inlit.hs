module Main where

import Language.Haskell.Exts
--import Language.Haskell.Exts.Pretty
import Data.Maybe
import System.Environment
import Text.PrettyPrint
import Data.List
import InOut

splitByHyphen :: [String] -> ([String], [String])
splitByHyphen = partition f where
   f ('-':_) = False
   f _ = True

main = do
  (src:out:_, opts) <- splitByHyphen `fmap` getArgs
  lns <- lines `fmap` readFile src
  --mmod <- fromParseResult `fmap` parseFile src
  --(print) $  mmod
  writeFile out $ unlines $ splitInlit lns 

-- prettyPrint $ addImport impUnsafe $ onDecls transform mmod

 