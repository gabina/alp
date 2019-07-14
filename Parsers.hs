module Parsers where

import Common
import Parsing


readPoem :: Parser Poem
readPoem = do char '*'
              return []
              <|>
              do v <- token readVerse
                 p <- readPoem
                 return (v:p)

readVerse :: Parser Verse
readVerse =  do c <- item
                if c == '/'
                   then return ""
                   else do l <- readVerse
                           return (c:l)
                           
get :: Input Poem
get = do f_out ("Ingrese un poema. Para finalizar presione *")
         p <- f_in
         case (parse readPoem p) of
			[(x,"")] -> do f_out ("Poema leído")
			               return x
			_ -> do f_out ("Error al ingresar el poema")
			        get
