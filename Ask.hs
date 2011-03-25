{-# LANGUAGE FlexibleInstances, GADTs #-}

module Ask where

import Graphics.Gnewplot.Exec
import Graphics.Gnewplot.Types
import Data.Unique
import System.Cmd
import System.Random

--data OutputMode = HTML | LaTeX | Text | Lhs2TeX deriving Show

class Ask a where
   inlitAsk ::  [String] -> a -> IO ()

instance Ask Int where
   inlitAsk s x = inlitPrintCode $ s++ ["=> "++show x] --return $ (False, show x)

instance Ask Double where
   inlitAsk s x = inlitPrintCode $ s++["=> "++show x] 

instance Ask [Char] where 
   inlitAsk s x = inlitPrintCode $ s++["=> "++show x]

data PlotDims where
    PlotDims :: PlotWithGnuplot a => Int -> Int -> Int -> a -> PlotDims

plot x = GnuplotBox x

instance Ask GnuplotBox where
   inlitAsk s x = inlitAsk s (PlotDims 700 450 12 x)
     
instance Ask PlotDims where
   inlitAsk s (PlotDims w h fs x) = do
      nm  <- (('l':) . show . round . (*(100000::Double))) `fmap` randomIO
      cmd <- multiPlot unitRect x
      let start = "set datafile missing \"NaN\"\n"
      let term = "set terminal png font \"Helvetica,"
                 ++show fs++"\" size "++ show w++","
                 ++show h++"\n"-- crop\n"
      let plot1 = "set output '"++nm++".png'\n"++
                               (showMultiPlot cmd)
      let cmds = start++term ++plot1
      execGP cmds 
      cleanupCmds $ map snd cmd
      let addPrompt (s:ss) = ("?> "++s):ss
          addPrompt []= []
      inlitPrintCode $ s
      putStrLn $ "!["++unlines s++"]("++nm++".png)"
   
  
--getNm = (('l':) . show . hashUnique) `fmap` newUnique

{-askLatexPic (PlotDims w h fs (GnuplotBox x))= do
      nm<- getNm
      cmd <- multiPlot unitRect x
      let start = "set datafile missing \"NaN\"\n"
      let term = "set terminal pdfcairo color font \"Helvetica,"
                 ++show fs++"\" size "++ show w++" mm,"
                 ++show h++" mm\n"-- crop\n"
      let plot1 = "set output '"++nm++".pdf'\n"++
                               (showMultiPlot cmd)
      let cmds = start++term ++plot1
      execGP cmds 
      cleanupCmds $ map snd cmd
      return $ (True, "\\includegraphics{"++nm++"}")
-}
{-askHtmlPic s (PlotDims w h fs x) = do
      nm<- (('l':) . show . hashUnique) `fmap` newUnique
      cmd <- multiPlot unitRect x
      let start = "set datafile missing \"NaN\"\n"
      let term = "set terminal png font \"Helvetica,"
                 ++show fs++"\" size "++ show w++","
                 ++show h++"\n"-- crop\n"
      let plot1 = "set output '"++nm++".png'\n"++
                               (showMultiPlot cmd)
      let cmds = start++term ++plot1
      execGP cmds 
      cleanupCmds $ map snd cmd
      putStrLn $ "!["++nm++"]("++nm++".png \""++concat s++"\")" -}
-- "<img src=\""++nm++"\"></img>"


{-ask :: Ask a => OutputMode -> [String] -> a -> IO ()
ask om ss x = do
  --mapM_ (putStrLn . ("?> "++)) ss
  printCode om ss
  (isImage, s) <- askInlit om x
  if isImage 
     then putStrLn s 
     else printCode om ["=> "++s] -}

inlitPrintCode :: [String] -> IO ()
inlitPrintCode ss = do
  putStrLn $ unlines $["~~~~~~{.haskell}"] ++ ss++ ["~~~~~~"]

