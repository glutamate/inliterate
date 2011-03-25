module Main where

import Language.Haskell.Exts
--import Language.Haskell.Exts.Pretty
import Data.Maybe
import System.Environment
import System.Directory
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

{-getMode = 
   ["theOutputMode <- do args <- SysEnv.getArgs",
    "                    return $ case args of",
    "                        _ | \"--html\" `elem` args -> Ask.HTML",
    "                        _ | \"--latex\" `elem` args -> Ask.LaTeX",
    "                        _ | \"--lhs2tex\" `elem` args -> Ask.Lhs2TeX",
    "                          | otherwise -> Ask.Text"]

getoutmode lines opts 
    | "--latex" `elem` opts = LaTeX
    | "--lhs2tex" `elem` opts = Lhs2TeX
    | "--html" `elem` opts = HTML
    | "--text" `elem` opts = Text
    | any ("<!DOCTYPE" `isPrefixOf`) (take 5 lines) = HTML
    | any ("%include" `isPrefixOf`) (take 5 lines) = Lhs2TeX
    | any ("\\documentclass" `isPrefixOf`) (take 5 lines) = LaTeX
    | otherwise = Text

modeRunCmds LaTeX nm = do
            system ("./"++nm++" >"++nm++".tex")
            system $ "pdflatex "++nm
modeRunCmds Lhs2TeX nm = do
            system ("./"++nm++" >"++nm++".lhs")
            system $ "lhs2TeX --math -o "++nm++".tex "++nm++".lhs"
            system $ "pdflatex "++nm
modeRunCmds HTML nm = system ("./"++nm++" >"++nm++".html")
modeRunCmds Text nm = system ("./"++nm)
-}

changeExt fnm ext = takeWhile (/='.') fnm ++ "."++ext

runCmds nm = do
  system $ ("./"++nm++" >"++nm++".md")
  args <-getArgs
  when ("--nopandoc" `elem` args) $ exitSuccess
  syn<- doesFileExist "style.css"
  synUp<- doesFileExist "../style.css"
  if syn 
     then system $ "pandoc -o "++changeExt nm "html" ++ " -c style.css "++changeExt nm "md"
     else if synUp
             then system $ "pandoc -o "++changeExt nm "html" ++ " -c ../style.css "++changeExt nm "md"
             else system $ "pandoc -o "++changeExt nm "html" ++ " "++changeExt nm "md"


main = do
  (src:rest, opts) <- splitByHyphen `fmap` getArgs
  fileLines <- fmap lines $ readFile src
  (bd, mn) <- return $ (splitInlit . (nmr:) . (th:)) fileLines
  --mmod <- fromParseResult `fmap` parseFile src
  --(print) $  mmod
  --let outmode = getoutmode fileLines opts
  let out = case rest of 
              o:_ -> o
              [] -> beforePeriod src ++ ".hs"
  let codeIn = unlines $ bd ++ ["main = do"] ++ 
                         (map ("   "++) (mn)) 

  codeOut <- case  parseFileContents codeIn of
     ParseFailed _ err -> do putStr codeIn
                             fail $ "Parse Failure"++err
     ParseOk ast -> return $ (nl pass++) $ prettyPrint $ addImport "System.IO.Unsafe" "SysIOUnface" $ 
                             addImport "Ask" "" $ addImport "Graphics.Gnewplot.Instances" "GnewInst" $ addImport "System.Environment" "SysEnv" $
                             setModuleName "Main" $ onDecls inxform $ ast
  writeFile out codeOut
  let inlitOpts = ["--nopandoc"]
  let runPossibleOpts = [] -- "--html","--latex", "--lhs2tex"]
  let ghcOpts = opts \\ (inlitOpts++runPossibleOpts)
  let runOpts = filter (`elem` runPossibleOpts) opts
  --when ("--make" `elem` opts || "--run" `elem` opts) $ do
  ec <- system ("ghc --make "++out++" "++intercalate " " ghcOpts)
  when (ec == ExitSuccess {-&& "--run" `elem` opts -}) $ do               
              runCmds $ beforePeriod out
              return ()


 
--todo: create pandoc
--inlitp for modules
--query type class