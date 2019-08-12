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
                         
                                              
get :: Input Poem
get = do f_out ("Ingrese un poema. Para finalizar presione *")
         p <- f_in
         case (parse readPoem "" p) of
			Right x -> do f_out ("Poema leído")
			              return x
			Left x -> do f_out ("Error al ingresar el poema: "++ (showErrorMessages "" "" "" "" "" (errorMessages x)))
			             get

