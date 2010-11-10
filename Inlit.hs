module Main where

import Language.Haskell.Exts
--import Language.Haskell.Exts.Pretty
import Data.Maybe
import System.Environment
import Text.PrettyPrint
import Data.List
import InOut
import System.Cmd
import System.Process
import Control.Monad
import Data.List
import System.Exit

splitByHyphen :: [String] -> ([String], [String])
splitByHyphen = partition f where
   f ('-':_) = False
   f _ = True

beforePeriod = takeWhile (/='.')

pass = "{- INLIT_PASS -}"

th = ">> {-# LANGUAGE TemplateHaskell #-}"

nl s = s++"\n"

main = do
  (src:rest, opts) <- splitByHyphen `fmap` getArgs
  (bd, mn) <- (splitInlit . (th:) . lines) `fmap` readFile src
  --mmod <- fromParseResult `fmap` parseFile src
  --(print) $  mmod
  let out = case rest of 
              o:_ -> o
              [] -> beforePeriod src ++ ".hs"
  let codeIn = unlines $ bd ++ ["main = do"] ++ (map ("   "++) mn) 
      codeOut = (nl pass++) $ prettyPrint $ addImport impUnsafe $ 
                setModuleName "Main" $ onDecls inxform $ fromParseResult $ parseFileContents codeIn
  writeFile out codeOut
  let notMyOpts = opts \\ ["--make","--run"]
  when ("--make" `elem` opts || "--run" `elem` opts) $ do
         ec <- system ("ghc --make "++out++intercalate " " notMyOpts)
         when (ec == ExitSuccess && "--run" `elem` opts) $ system ("./"++beforePeriod out) >> return ()
--runProcess (beforePeriod out) [] Nothing Nothing Nothing Nothing Nothing >> return ()
-- prettyPrint $ addImport impUnsafe $ onDecls transform mmod

 
--todo: create pandoc
--inlitp for modules
--query type class