module Main where

import Parsing
import Common

-- ::t print
-- cabal run
-- para ingresar un pormea:
-- "tu culo es lo único redondo/ en este edificio de oficinas/*"
readPoem :: Parser Poem
readPoem = do char '*'
              return []
              <|>
              do v <- readVerse
                 p <- readPoem
                 return (v:p)

readVerse :: Parser Verse
readVerse =  do c <- item
                if c == '/'
                   then return ""
                   else do l <- readVerse
                           return (c:l)
                               
main :: IO ()

main = do   putStrLn "Bienvenido a Méri"    
            putStrLn "Ingrese un poema. Para finalizar presione *"
            x <- getLine
            print x
            --putStrLn "Andy!" 
