{-# LANGUAGE FlexibleInstances, GADTs #-}

module Ask where

import PlotGnuplot
import Data.Unique
import System.Cmd

--data OutputMode = HTML | LaTeX | Text | Lhs2TeX deriving Show

class Ask a where
   ask ::  [String] -> a -> IO ()

instance Ask Int where
   ask s x = printCode $ s++ ["=> "++show x] --return $ (False, show x)

instance Ask Double where
   ask s x = printCode $ s++["=> "++show x] 

instance Ask [Char] where 
   ask s x = printCode $ s++["=> "++show x]

data PlotDims where
    PlotDims :: PlotWithGnuplot a => Int -> Int -> Int -> a -> PlotDims

instance Ask GnuplotBox where
   ask s x = ask s (PlotDims 89 127 16 x)
     
instance Ask PlotDims where
   ask = askHtmlPic
  
getNm = (('l':) . show . hashUnique) `fmap` newUnique

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
askHtmlPic s (PlotDims w h fs x) = do
      nm<- getNm
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
      putStrLn $ "!["++nm++"]("++nm++" \""++unlines s++"\")"
-- "<img src=\""++nm++"\"></img>"


{-ask :: Ask a => OutputMode -> [String] -> a -> IO ()
ask om ss x = do
  --mapM_ (putStrLn . ("?> "++)) ss
  printCode om ss
  (isImage, s) <- askInlit om x
  if isImage 
     then putStrLn s 
     else printCode om ["=> "++s] -}

printCode :: [String] -> IO ()
printCode ss = do
  putStrLn $ unlines $["~~~~~~{.haskell}"] ++ ss++ ["~~~~~~"]

