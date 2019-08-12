module Main where

import Text.ParserCombinators.Parsec
import System.Environment (getArgs)
import Data.Char(digitToInt)
import Common
import Options
--cabal run -- -i
options = [("Cuarteto",cuarteto),("Redondilla",redondilla), ("Seguidilla",seguidilla), ("Romance",romance),("Soneto",soneto), ("Decima",decima),("Ayuda",help),("Salir",bye)]
 
printEnum' :: [Command] -> Int -> IO (Either String ())
printEnum' [] _ = return (Right ())
printEnum' (str:cs) i = do putStrLn (show i ++ " - " ++ (fst str))
                           printEnum' cs (i+1)
                          
printEnum :: [Command] -> IO (Either String ())
printEnum s = printEnum' s 1

isOption :: Int -> Bool
isOption n = (n > 0) && (n<= length (options)) 

getCommand :: [Command] -> IO ()
getCommand xs = do putStrLn " "
                   printEnum xs
                   putStrLn "Ingrese la opción correspondiente"
                   x <- getLine
                   case parse digit "" x of 
					(Right n) -> if (isOption (digitToInt n)) then do r <- runInput (snd (xs !! ((digitToInt n)-1)))
					                                                  case r of 
																		Right _ -> getCommand xs
																		Left e -> do putStrLn e
																		             return ()
														      else do putStrLn ("No es una opción válida, intente nuevamente")
														              getCommand xs
					_ -> do putStrLn ("No es una opción válida, intente nuevamente")
					        getCommand xs                     
run :: [String] -> IO ()
run [] = putStrLn "Argumentos faltantes"
run (m:xs) = case m of
				"-i" -> getCommand options
				"-f" -> putStrLn "Archivo"
				_ -> putStrLn "Opción desconocida"
                            
main :: IO ()
main = do   args <- getArgs    
            run args

