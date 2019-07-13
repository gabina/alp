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

options = ["Cuarteto","Decima", "Lira", "Octava Real", "Redondilla", "Serventesio","Soneto"]

    
printEnum' :: [String] -> Int -> IO (Either String ())
printEnum' [] _ = return (Right ())
printEnum' (str:cs) i = do putStrLn (show i ++ " - " ++ str)
                           printEnum' cs (i+1)
                          
printEnum :: [String] -> IO (Either String ())
printEnum s = printEnum' s 1
 
                            
main :: IO ()
main = do   putStrLn "Bienvenido a Méri"    
            putStrLn "Ingrese un poema. Para finalizar presione *"
            --
            printEnum options
            x <- getLine
            print x
            --parse readPoem x
            --print x
            --putStrLn "Andy!" 
