{-# LANGUAGE FlexibleInstances #-}

module Ask where

import PlotGnuplot

data OutputMode = HTML | LaTeX | Text | Lhs2TeX

class Ask a where
   askInlit :: OutputMode -> a -> IO String

instance Ask Int where
   askInlit _ x = return $ show x

instance Ask Double where
   askInlit _ x = return $ show x

instance Ask [Char] where 
   askInlit _ = return 

data PlotDims = PlotDims Double Double Int GnuplotBox

instance Ask GnuplotBox where
   askInlit om (GnuplotBox x)= askInLit om (PlotDims 3.5 5.0 16)
       
instance Ask PlotDims where
   askInlit om (PlotDims w h fs (GnuplotBox x))= do
      nm<- (("l":) . show . hashUnique) `fmap` newUnique
      cmd <- multiPlot unitRect x
      let start = "set datafile missing \"NaN\"\n"
      let term = "set terminal postscript eps enhanced color \"Helvetica\" "++show fs++" size "++ show w++","++show h++"\n"-- crop\n"
      let plot1 = "set output '"++nm++".eps'\n"++
                               (showMultiPlot cmd)
      let cmds = start++term ++plot1
      execGP cmds 
      system $ "epstopdf "++nm++".eps"
      cleanupCmds $ map snd cmd


ask :: Ask a => OutputMode -> [String] -> a -> IO ()
ask om ss x = do
  mapM_ (putStrLn . ("?> "++)) ss
  s <- askInlit om x
  putStrLn $ "=> "++s

printCode :: OutputMode -> [String] -> IO ()
printCode _ = mapM_ (putStrLn . ("> "++))

printText :: OutputMode -> [String] -> IO ()
printText _ [""] = do
  putStrLn ""
printText HTML ss = do
  putStr "<p>" >> mapM_ putStrLn ss >> putStrLn "</p>" 
printText _ ss = do
  putStrLn "" >> mapM_ putStrLn ss >> putStrLn "" 
