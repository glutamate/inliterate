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
nmr = ">> {-# LANGUAGE NoMonomorphismRestriction #-}"


nl s = s++"\n"

getMode = 
   ["theOutputMode <- do args <- SysEnv.getArgs",
    "                    return $ case args of",
    "                        _ | \"--html\" `elem` args -> Ask.HTML",
    "                        _ | \"--latex\" `elem` args -> Ask.LaTeX",
    "                        _ | \"--lhs2tex\" `elem` args -> Ask.Lhs2TeX",
    "                          | otherwise -> Ask.Text"]

main = do
  (src:rest, opts) <- splitByHyphen `fmap` getArgs
  (bd, mn) <- (splitInlit . (nmr:) . (th:) . lines) `fmap` readFile src
  --mmod <- fromParseResult `fmap` parseFile src
  --(print) $  mmod
  let out = case rest of 
              o:_ -> o
              [] -> beforePeriod src ++ ".hs"
  let codeIn = unlines $ bd ++ ["main = do"] ++ (map ("   "++) (getMode++mn)) 

  codeOut <- case  parseFileContents codeIn of
     ParseFailed _ err -> do putStr codeIn
                             fail $ "Parse Failure"++err
     ParseOk ast -> return $ (nl pass++) $ prettyPrint $ addImport "System.IO.Unsafe" "SysIOUnface" $ 
                             addImport "Ask" "Ask" $ addImport "System.Environment" "SysEnv" $
                             setModuleName "Main" $ onDecls inxform $ ast
  writeFile out codeOut
  let inlitOpts = ["--make","--run"]
  let runPossibleOpts = ["--html","--latex", "--lhs2tex"]
  let ghcOpts = opts \\ (inlitOpts++runPossibleOpts)
  let runOpts = filter (`elem` runPossibleOpts) opts
  when ("--make" `elem` opts || "--run" `elem` opts) $ do
         ec <- system ("ghc --make "++out++" "++intercalate " " ghcOpts)
         when (ec == ExitSuccess && "--run" `elem` opts) $ 
              system ("./"++beforePeriod out++" "++intercalate " " runOpts) >> return ()
--runProcess (beforePeriod out) [] Nothing Nothing Nothing Nothing Nothing >> return ()
-- prettyPrint $ addImport impUnsafe $ onDecls transform mmod

 
--todo: create pandoc
--inlitp for modules
--query type class