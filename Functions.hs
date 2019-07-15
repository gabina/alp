module Functions where

import Common
import Parsing
import Data.Ratio
import Control.Monad.Writer
import Data.Set
import qualified Data.Set as Set

showAnswer :: Writer [String] a -> Input ()
showAnswer a = let (b,xs) = runWriter a in f_out_list xs

{-Conjuntos útiles para separar en sílabas-}

v = fromList ['a','e','i','o','u','á','é','í','ó','ú'] --vocales
c = fromList ['b','c','d','f','g','h','j','k','l','m','n','ñ','p','q','r','s','t','v','w','x','y','z'] --consonantes
vs = fromList ['a','e','o'] --vocales fuertes
vw = fromList['i','u'] --vocales débiles
vwa = fromList ['í','ú'] --vocales débiles tildadas
lr = fromList ['l','r']
vt = fromList ['á','é','í','ó','ú'] -- vocales tildadas

syllabifier :: String -> [String]
syllabifier s = syllabifier' s [] ""

{- s es el string a separar en sílabas
   n es la lista de sílabas
   t es la sílaba hasta el momento-}
syllabifier' :: String -> [String] -> String -> [String]
syllabifier' [] n _ = n
syllabifier' (s0:[]) n _ = n
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


{-Métrica
abba -> [[0,3],[1,2]]
-} 


giveWord' :: Verse -> String -> String
giveWord' "" w = w
giveWord' (x:xs) w = if x == ' ' then w else (giveWord' xs (x:w)) 

{-Retorna la última palabra de un verso-}
giveLastWord :: Verse -> String
giveLastWord v = giveWord' (reverse v) ""

{- Retorna el string a partir de encontrar un elemento perteneciente al conjunto -}
fromThenOn:: String -> Set Char -> String
fromThenOn "" _ = ""
fromThenOn (s:xs) c = if (member s c) then (s:xs) else (fromThenOn xs c)

{- Si hay un tilde en el string, retorna a partir de la vocal acentuada.
   Si no, retortna el string vacío -}
vocalTildada :: String -> String
vocalTildada s = fromThenOn s vt

{- Para palabras sin tildes:
   si termina en n, s o vocal, hay que retornar la penúltima sílaba a partir de la vocal + la última sílaba
   en caso contrario, hay que retornar desde la vocal de la última sílaba -}
vocalTonica :: [String] -> String
vocalTonica xs = let lastSyl = last xs;
					 lastLetter = last lastSyl;
					 penSyl = last (init xs);
					 grave = (member lastLetter v) || (lastLetter == 'n') || (lastLetter == 's')
				 in case grave of
					True -> fromThenOn (penSyl++lastSyl) v
					False -> fromThenOn lastSyl v
   
{-Retorna las sílabas a partir de la vocal tónica -}
giveTonica :: [String] -> String
giveTonica s = let s' = concat s;
				   t = vocalTildada s'
			   in if t == "" then vocalTonica s else t	


haveRhyme :: [String] -> Bool
haveRhyme [] = True
haveRhyme (s:syls) = and (Prelude.map (== s) syls)

satisfyMetric :: Poem -> Metric -> Writer [String] Bool
satisfyMetric p (Consonante n ms) = case (mod length(p) n) of
										0 -> do tell (["Cantidad de versos adecuada"]
												let syls = Prelude.map (giveTonica . syllabifier . giveLastWord) p;
													takeSyllables _ [] = [];
													takeSyllables s (x:xs) = (s!!x) : (takeSyllables s xs);
													rhymes = Prelude.map (takeSyllables syls) ms
												return (and (Prelude.map haveRhyme rhymes))
										_ -> do tell (["Sobran "++show(mod length(p) n)++" versos o faltan "++show(n- (mod length(p) n))++" versos"]) 
										        return False


{-                                                let syls = Prelude.map (giveTonica . syllabifier . giveLastWord) p;
													takeSyllables _ [] = [];
													takeSyllables s (x:xs) = (s!!x) : (takeSyllables s xs);
													rhymes = Prelude.map (takeSyllables syls) ms
												in return (and (Prelude.map haveRhyme rhymes)) -}

















