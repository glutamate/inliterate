{-# LANGUAGE FlexibleInstances #-}

module Ask where

import PlotGnuplot
import Data.Unique
import System.Cmd

data OutputMode = HTML | LaTeX | Text | Lhs2TeX

class Ask a where
   askInlit :: OutputMode -> a -> IO (Bool, String)

instance Ask Int where
   askInlit _ x = return $ (False, show x)

instance Ask Double where
   askInlit _ x = return $ (False, show x)

instance Ask [Char] where 
   askInlit _ s = return (False, s)

data PlotDims = PlotDims Int Int Int GnuplotBox

instance Ask GnuplotBox where
   askInlit om gb= askInlit om (PlotDims 89 127 16 gb)
       
getNm = (('l':) . show . hashUnique) `fmap` newUnique

askLatexPic (PlotDims w h fs (GnuplotBox x))= do
      nm<- getNm
      cmd <- multiPlot unitRect x
      let start = "set datafile missing \"NaN\"\n"
      let term = "set terminal pdfcairo color font \"Helvetica,"++show fs++"\" size "++ show w++" mm,"++show h++" mm\n"-- crop\n"
      let plot1 = "set output '"++nm++".pdf'\n"++
                               (showMultiPlot cmd)
      let cmds = start++term ++plot1
      execGP cmds 
      cleanupCmds $ map snd cmd
      return $ (True, "\\includegraphics{"++nm++"}")

askHtmlPic (PlotDims w h fs (GnuplotBox x))= do
      nm<- getNm
      cmd <- multiPlot unitRect x
      let start = "set datafile missing \"NaN\"\n"
      let term = "set terminal png font \"Helvetica,"++show fs++"\" size "++ show w++","++show h++"\n"-- crop\n"
      let plot1 = "set output '"++nm++".png'\n"++
                               (showMultiPlot cmd)
      let cmds = start++term ++plot1
      execGP cmds 
      cleanupCmds $ map snd cmd
      return $ (True, "<img src=\""++nm++"\"></img>")

instance Ask PlotDims where
   askInlit LaTeX = askLatexPic
   askInlit Lhs2TeX = askLatexPic
   askInlit Text = const $ return (True, "<<PLOT>>")

ask :: Ask a => OutputMode -> [String] -> a -> IO ()
ask om ss x = do
  mapM_ (putStrLn . ("?> "++)) ss
  (isImage, s) <- askInlit om x
  if isImage then putStrLn s else putStrLn $ "=> "++s

printCode :: OutputMode -> [String] -> IO ()
printCode _ = mapM_ (putStrLn . ("> "++))

printText :: OutputMode -> [String] -> IO ()
printText _ [""] = do
  putStrLn ""
printText HTML ss = do
  putStr "<p>" >> mapM_ putStrLn ss >> putStrLn "</p>" 
printText _ ss = do
  putStrLn "" >> mapM_ putStrLn ss >> putStrLn "" 
