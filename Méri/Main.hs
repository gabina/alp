module Main where

import Parsing
import Common
import Options
import qualified Data.Text.IO as Txt

-- ::t print
-- cabal run
-- para ingresar un pormea:
-- "tu culo es lo único redondo/ en este edificio de oficinas/*"

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
getCommand xs = do putStrLn "Ingrese la opción correspondiente"
                   x <- getLine
                   case parse nat x of 
                    [(n,"")] -> if (isOption n) then do r <- runInput (snd (xs !! (n-1)))
                                                        case r of 
															Right _ -> do putStrLn " "
															              putStrLn "Pulse (m) si quiere volver al menu"
															              putStrLn "Pulse cualquier otra tecla para salir del programa"
															              o <- getLine
															              if o == "m" then main else return ()
															Left e -> do putStrLn e
															             return ()
                                                else do putStrLn ("No es una opción válida, intente nuevamente")
                                                        getCommand xs
                    _ -> do putStrLn ("No es una opción válida, intente nuevamente")
                            getCommand xs                     
 
                            
main :: IO ()
main = do   putStrLn "Bienvenido a Méri"    
            --
            printEnum options
            getCommand options
            --if x == "í" then putStrLn "Bien" else putStrLn "Mal"
            --parse readPoem x
            --putStrLn "Andy!" 
