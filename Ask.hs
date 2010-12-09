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
