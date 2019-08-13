module Main where

import Text.ParserCombinators.Parsec
import System.Environment (getArgs)
import Data.Char(digitToInt)
import Common
import Options
import Parsers

--cabal run -- -i
options = [("Ayuda",help),("Salir",bye),("Cuarteto",cuarteto),("Redondilla",redondilla), ("Seguidilla",seguidilla), ("Romance",romance),("Soneto",soneto), ("Decima",decima)]
 
printEnum' :: [Command] -> Int -> IO (Either String ())
printEnum' [] _ = return (Right ())
printEnum' (str:cs) i = do putStrLn (show i ++ " - " ++ (fst str))
                           printEnum' cs (i+1)
                          
printEnum :: [Command] -> IO (Either String ())
printEnum s = printEnum' s 0

isOption :: Int -> Bool
isOption n =(n>0) && (n<= length (options)-1) 

interactive :: [Command] -> IO ()
interactive xs = do putStrLn " "
                    printEnum xs
                    putStrLn "Ingrese la opción correspondiente"
                    x <- getLine
                    case parse digit "" x of 
						Right n -> if (isOption opt && opt>1) then do poem <- get
						                                              r <- runInput (snd (xs!!opt) poem)
						                                              case r of 
																		Right _ -> interactive xs
																		Left e -> do putStrLn e
																		             return ()
															  else if (isOption opt) then do r <- runInput (snd (xs!!opt) [])
															                                 case r of 
																								Right _ -> interactive xs
																								Left e -> do putStrLn e
																								             return ()
														                             else do putStrLn ("No es una opción válida, intente nuevamente")
														                                     interactive xs
								  where opt = digitToInt n
						_ -> do putStrLn ("No es una opción válida, intente nuevamente")
						        interactive xs

fromFile :: [Command] -> [String] -> IO ()
fromFile _ [] = putStrLn "Argumentos faltantes"
fromFile _ (_:[]) = putStrLn "Argumentos faltantes"    					                       
fromFile xs [name,opt] = do file <- readFile name
                            poem <- getFromFile file
                            case parse digit "" opt of 
								Right n -> if (isOption opt && opt>1) then do r <- runInput (snd (xs!!opt) poem)
								                                              case r of 
																				Right _ -> return ()
																				Left e -> do putStrLn e
																				             return ()
																	  else if (isOption opt) then do r <- runInput (snd (xs!!opt) [])
																	                                 case r of 
																										Right _ -> return ()
																										Left e -> do putStrLn e
																										             return ()
														                                     else do putStrLn ("Opción inválida")
														                                             return ()
								  where opt = digitToInt n
								_ -> do putStrLn ("Opción inválida")
								        return ()

fromFile _ _ = putStrLn "Argumentos sobrantes"                     
                        
                        
run :: [String] -> IO ()
run [] = putStrLn "Argumentos faltantes"
run (m:xs) = case m of
				"-i" -> interactive options
				"-f" -> fromFile options xs
				_ -> putStrLn "Opción desconocida"
                            
main :: IO ()
main = do   args <- getArgs    
            run args

