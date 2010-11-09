{-# LANGUAGE TemplateHaskell, BangPatterns #-}
{-# OPTIONS_GHC -F -pgmF inoutproc #-}
module Main where
 
import System.Environment

--readSource "foo" args 
foo <* readFile "space.cabal"

foo *> putStr

--foz <* argz ()

--x = SysIOUnface.unsafePerformIO args 

-- !x = 5

--sig = {: 1 :}

--z <- readFile "fool"

--main = do
--   putStrLn "foo" 

--bar *> putStr_  
--baz *> putStr_ 4

--x = let !y = 5  
--    in y
{-
main = do print "hello"  
          let baz = 4::Int
          exp <- runQ [d| x = unsafePerformIO (print baz) |]
          print exp 
          print foo -}