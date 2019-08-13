module Functions where

import Common
import Parsing
import Control.Monad.Writer
import Data.Char
import Data.Set
import qualified Data.Set as Set

showAnswer :: Writer [String] a -> Input ()
showAnswer m = f_out_list (execWriter m)

{-Conjuntos útiles para separar en sílabas-}

v = fromList ['a','e','i','o','u','á','é','í','ó','ú'] --vocales
c = fromList ['b','c','d','f','g','h','j','k','l','m','n','ñ','p','q','r','s','t','v','w','x','y','z'] --consonantes
vs = fromList ['a','e','o'] --vocales fuertes
vw = fromList['i','u'] --vocales débiles
vwa = fromList ['í','ú'] --vocales débiles tildadas
lr = fromList ['l','r'] --consonantes especiales
vt = fromList ['á','é','í','ó','ú'] -- vocales tildadas


{-Elimina puntuación en general, y pasa a minúsculas-}
neutralString :: String -> String
neutralString s = Prelude.map Data.Char.toLower (Prelude.filter Data.Char.isLetter s)

{-syllabifier separa una palabra en síalabas-}
syllabifier :: String -> [String]
syllabifier s = syllabifier' (neutralString s) [] ""

{- s es el string a separar en sílabas
   n es la lista de sílabas
   t es la sílaba hasta el momento-}
syllabifier' :: String -> [String] -> String -> [String]
syllabifier' [] n _ = n
syllabifier' (s0:[]) n t = n++[t++[s0]]
syllabifier' (s0:s1:xs) n t = case (hiato s0 s1) of
								True -> syllabifier' (s1:xs) (n++[t++[s0]]) ""
								False -> case (consonantes s0 s1) of
											True -> syllabifier' xs (n++[t]) [s0,s1]
											False -> case ((member s0 c) && (member s1 v)) of
												True -> syllabifier' (s1:xs) (n++[t]) [s0]
												False -> syllabifier' (s1:xs) n (t++[s0])


{- Determina si dos letras forman un caso especial de consonantes
c+h - l+l - r+r - consonante+l - consonante+r -}
consonantes :: Char -> Char -> Bool
consonantes l0 l1 = (((member l0 c) && (member l1 lr)) || ((l0 == 'c') && (l1 == 'h')))

{- Determina si dos letras forman un hiato -}
hiato :: Char -> Char -> Bool
hiato l0 l1 = (((member l0 vs) || (member l0 vwa)) && (member l1 vs)) || ((member l0 v) && (member l1 vwa))

giveVowels :: String -> String
giveVowels s = Prelude.filter (\x -> member x v) s

giveWord' :: Verse -> String -> String
giveWord' "" w = w
giveWord' (x:xs) w = if x == ' ' then w else (giveWord' xs (x:w)) 

{-Retorna la última palabra de un verso-}
giveLastWord :: Verse -> String
giveLastWord v = giveWord' (reverse v) ""

{- Retorna el string a partir de encontrar un elemento perteneciente al conjunto, teniendo en cuenta la excepció de la "qu" -}
fromThenOnQU:: String -> Set Char -> String
fromThenOnQU "" _ = ""
fromThenOnQU (s:xs) c = if (s=='q') then (tail xs) else if (member s c) then (s:xs) else (fromThenOn xs c)

{- Retorna el string a partir de encontrar un elemento perteneciente al conjunto -}
fromThenOn:: String -> Set Char -> String
fromThenOn "" _ = ""
fromThenOn (s:xs) c = if (member s c) then (s:xs) else (fromThenOn xs c)

{-Retorna el string hasta que encuentra un elemento perteneciente al conjunto-}
untilC :: String -> Set Char -> String
untilC "" _ = ""
untilC (s:xs) c = if (member s c) then [s] else (s:(untilC xs c))

{- Si hay un tilde en el string, retorna a partir de la vocal acentuada.
   Si no, retortna el string vacío -}
vocalTildada :: String -> String
vocalTildada s = fromThenOn s vt

{- Para palabras sin tildes:
   si termina en n, s o vocal, hay que retornar la penúltima sílaba a partir de la última vocal + la última sílaba
   en caso contrario, hay que retornar desde la última vocal de la última sílaba -}
vocalTonica :: [String] -> String
vocalTonica xs = let lastSyl = last xs;
					 lastLetter = last lastSyl;
					 penSyl = last (init xs);
					 grave = (member lastLetter v) || (lastLetter == 'n') || (lastLetter == 's')
				 in case grave of
					True -> (reverse (untilC (reverse penSyl) v))++lastSyl
					False -> reverse (untilC (reverse lastSyl) v)
   
{-Retorna las sílabas a partir de la vocal tónica -}
giveTonica :: [String] -> String
giveTonica s = let s' = concat s;
				   t = vocalTildada s'
			   in if t == "" then vocalTonica s else t	

{- Evalúa la lista de booleanos -}
f :: Writer [String] [Bool] -> Writer [String] Bool 
f w = let (boolList,s) = runWriter w in writer (and boolList, s)

{-Cambia la 'z' por 's'-}
toS :: Char -> Char
toS 'z' = 's'
toS c = c

{-Cambia la 'v' por 'b' -}
toB :: Char -> Char
toB 'v' = 'b'
toB c = c

{-Quita las tildes-}
sinTildes :: Char -> Char
sinTildes 'á' = 'a'
sinTildes 'é' = 'e'
sinTildes 'í' = 'i'
sinTildes 'ó' = 'o'
sinTildes 'ú' = 'u'
sinTildes c = c

equalSound :: String -> String -> Bool
equalSound s0 s1 = Prelude.map (sinTildes . toB . toS) s0 == Prelude.map (sinTildes . toB . toS) s1

{-Determina si dos sílabas son iguales fonéticamente -}
equalSyll :: String -> Int -> (Int,String) -> Writer [String] Bool
equalSyll s0 n0 (n1,s1) = if (equalSound s0 s1) then return True 
							        else do tell(["Fallo en rima versos "++show(n0)++" y "++show(n1),"Sílabas: "++s0++" "++s1])
							                return False

haveRhyme :: [(Int,String)] -> Writer [String] Bool
haveRhyme [] = return True
haveRhyme ((n0,s0):syls) =  f (Control.Monad.Writer.sequence (Prelude.map (equalSyll s0 n0) syls))

{-Modifca la métrica para hacer análisis en caso de que exista algún verso vacío -}
modifyMetric ::[(Verse,Int)] -> Metric -> Metric
modifyMetric [] m = m
modifyMetric ((x,n):xs) metric@(Consonante k ms) = case x of
												"" -> modifyMetric xs (Consonante k (Set.map (Set.delete n) ms))
												_ -> modifyMetric xs metric											
modifyMetric ((x,n):xs) metric@(Asonante k ms) = case x of
											"" -> modifyMetric xs (Asonante k (Set.map (Set.delete n) ms))
											_ -> modifyMetric xs metric											

{-Determina si existen versos vacíos -}											
checkEmptyVerse :: (Verse,Int) -> Writer [String] Bool
checkEmptyVerse ("", n) = do tell(["Verso "++show(n)++" vacío"])
                             return False
checkEmptyVerse (_,_) = return True

{- Dada una lista de sílabas y una lista de enteros indicando qué sílabas deben rimar, devuelve la lista de las sílabas -}
takeSyllables' :: [String] -> [Int] -> [(Int,String)]
takeSyllables' _ [] = []
takeSyllables' s (x:xs) = (x,(s!!x)) : (takeSyllables' s xs)

takeSyllables :: [String] -> Set Int -> [(Int,String)]
takeSyllables s verses = takeSyllables' s (toList verses)

satisfyMetric'' :: Poem -> Metric -> Writer [String] Bool
satisfyMetric'' p metric@(Consonante n ms) = do b1 <- f (Control.Monad.Writer.sequence (Prelude.map checkEmptyVerse p'))
                                                b2 <- f (Control.Monad.Writer.sequence (toList (Set.map haveRhyme rhymes)))
                                                if b1 && b2 then do tell (["Satisface rima"])
                                                                    return True
														    else return False
													where p' = zip p [0..(n-1)]
													      syls = Prelude.map (giveTonica . syllabifier . giveLastWord) p
													      Consonante n' ms' = modifyMetric p' metric
													      rhymes = Set.map (takeSyllables syls)	ms'									
												
satisfyMetric'' p metric@(Asonante n ms) =  do b1 <- f (Control.Monad.Writer.sequence (Prelude.map checkEmptyVerse p'))
                                               b2 <- f (Control.Monad.Writer.sequence (toList (Set.map haveRhyme rhymes)))
                                               if b1 && b2 then do tell (["Satisface rima"])
                                                                   return True
												           else return False
													where p' = zip p [0..(n-1)]
													      syls = Prelude.map (giveVowels . giveTonica . syllabifier . giveLastWord) p
													      Asonante n' ms' = modifyMetric p' metric
													      rhymes = Set.map (takeSyllables syls)	ms'	

{- -}										        
satisfyMetric' :: Poem -> Metric -> Int -> Writer [String] Bool										        
satisfyMetric' p metric@(Consonante n ms) i = let cantVerses = length p;
			    		                      in if cantVerses == 0 then return True 
			    		                                            else do tell (["Estrofa "++show(i)])
			    		                                                    b1 <- satisfyMetric'' (take n p) metric
			    		                                                    b <- satisfyMetric' (drop n p) metric (i+1)
			    		                                                    if b1 && b then do tell (["Satisface rima total"])
			    		                                                                       return True
																				       else return False
satisfyMetric' p metric@(Asonante n ms) i = let cantVerses = length p;
			    		                    in if cantVerses == 0 then return True 
			    		                                          else do tell (["Estrofa "++show(i)])
			    		                                                  b1 <- satisfyMetric'' (take n p) metric
			    		                                                  b <- satisfyMetric' (drop n p) metric (i+1)
			    		                                                  if b1 && b then do tell (["Satisface rima total"])
			    		                                                                     return True
														      					     else return False

{-Evalúa si el poema es vacío y si la cantidad de versos es múltiplo de lo que indica la métrica buscada.
En caso de que eso ande bien, evalúa las rimas llamando a satisfyMetric' -}
satisfyMetric :: Poem -> Metric -> Writer [String] Bool										        
satisfyMetric p metric@(Consonante n ms) = let cantVerses = length p;
							                   verses = (mod cantVerses n)
			    		                   in if cantVerses == 0 then do tell(["Poema vacío"])
			    		                                                 return False 
			    		                                         else case verses of
                                                    0 -> do tell (["Cantidad de versos adecuada"])
                                                            satisfyMetric' p metric 0
                                                    _ -> do tell (["Sobran "++show(verses)++" versos o faltan "++show(n-verses)++" versos"])
                                                            return False
satisfyMetric p metric@(Asonante n ms) = let cantVerses = length p;
							                   verses = (mod cantVerses n)
			    		                   in if cantVerses == 0 then do tell(["Poema vacío"])
			    		                                                 return False 
			    		                                         else case verses of
                                                    0 -> do tell (["Cantidad de versos adecuada"])
                                                            satisfyMetric' p metric 0
                                                    _ -> do tell (["Sobran "++show(verses)++" versos o faltan "++show(n-verses)++" versos"])
                                                            return False
