module Parsers where

import Common
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

readPoem :: Parser Poem
readPoem = do char '/' 
              fail "Se ingresó un verso vacío"
              <|> do verse <- manyTill anyToken (try (between spaces spaces (char '/')))
                     (do (char '*')
                         return [verse]
                         <|> do rest <- readPoem <?> "\n/ o * faltantes"
                                return (verse:rest))
                         
                                              
get :: IO Poem
get = do putStrLn ("Ingrese un poema. Para finalizar presione *")
         p <- getLine
         case (parse readPoem "" p) of
			Right x -> do putStrLn ("Poema leído")
			              return x
			Left x -> do putStrLn ("Error al ingresar el poema: "++ (showErrorMessages "" "" "" "" "" (errorMessages x)))
			             get
			             
getFromFile :: String -> IO Poem
getFromFile p = case (parse readPoem "" p) of
					Right x -> do putStrLn ("Poema leído")
					              return x
					Left x -> do putStrLn ("Error al ingresar el poema: "++ (showErrorMessages "" "" "" "" "" (errorMessages x)))
					             fail []
