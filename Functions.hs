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

syllabifier :: String -> [String]
syllabifier s = syllabifier' s [] ""

{- s es el string a separar en sílabas
   n es la lista de sílabas
   t es la sílaba hasta el momento-}
syllabifier' :: String -> [String] -> String -> [String]
syllabifier' s0:[] n _ = n
syllabifier' s0:s1:s2:xs n t = case (hiato s0 s1) of
								True -> syllabifier' (s1:xs) (n++(t++[s0])) ""
								False -> case ((member s0 c) && (member s1 v)) of
											True -> case ((member s0 lr) && (

		#consonante + vocal	
		if ((S[i] in C) and (S[i+1] in V)):
			print("Segundo if")
			if (( S[i] in LR) and (S[i-1] in C)):
				print("Tercer if")
				if i>1:
					print("Cuarto if")
					N = N + T + "-"
					T = S[i-1] + S[i]
				else:
					print("Cuarto else")
					T = T + S[i]
					
			else:
				print("Tercer else")
				N = N + T + "-"
				T = S[i]
		else:
			print("Segundo else")
			T = T + S[i]
		
		print("T: ",T)
		print(" ")
	print(N+T+S[i+1])

{- Determina si dos letras forman un hiato -}
hiato :: Char -> Char -> Bool
hiato l0 l1 = (((member l0 vs) || (member l0 vwa)) && (member l1 vs))) || ((member l0 v) && (member l1 vwa))








{-Métrica
abba -> [[0,3],[1,2]]
-} 

{- Esta función quizás no sea necesaria
giveMetric :: Poem -> WithError Metric
giveMetric [] = SomethingIsEmpty "Poema vacío"
giveMetric p = (map giveSyllable p)
-}
isVowel :: Char -> Bool
isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel _ = False

isClosed :: Char -> Bool
isClosed 'i' = True
isClosed 'u' = True
isClosed _ = False

isOpened :: Char -> Bool
isOpened 'a' = True
isOpened 'e' = True
isOpened 'o' = True
isOpened _ = False

vowel :: Parser Char
vowel = sat isVowel

closed :: Parser Char
closed = sat isClosed

opened :: Parser Char
opened = sat (isOpened)

isNotVowel :: Parser Char
isNotVowel = sat (\x -> not (isVowel x))

--diptongo al derecho
diphthong :: Parser String
diphthong = do c <- closed
               (do a <- opened
                   return ([c,a])
                   <|> do c1 <- closed
                          return ([c,c1]))

--diptongo al revés
{-diphthong :: Parser String
diphthong = do a <- opened
               c <- closed
               return ([a,c])
               <|> do c1 <- closed
                      c2 <- closed
                      return ([c1,c2])
-}
haveRhyme :: [WithError Syllable] -> Bool
haveRhyme [] = True
haveRhyme (s:syls) = and (Prelude.map (== s) syls)

-- Consume la última sílaba de un verso al revés
-- Hasta la última vocal de la sílaba
lastSyllable :: Parser Syllable
lastSyllable = do b <- isNotVowel
                  (do d <- diphthong
                      return (b:d)
                      <|> do a <- vowel
                             return ([b,a]))
                  <|> do d <- diphthong
                         return (d)
                  <|> do a <- vowel
                         return ([a])
                  
{-
lastSillable' :: String -> String -> (String,String)
lastSyllable' "" s0 = (s0,s1)
lastSyllable' (x:xs) s0 =  if (isVowel x) then (s0,x:xs) else (lastSyllable' xs (s0++[x]))
-}

-- devuelve una tupla (consonantes hasta la primera vocal, el resto)
--lastSyllable :: String -> (String,String)
--lastSyllable s =  lastSyllable' s ""

-- Parsear sílabas
isC1L :: Char -> Bool
isC1L 'g' = True
isC1L 'k' = True
isC1L 't' = True
isC1L 'b' = True
isC1L 'p' = True
isC1L 'f' = True
isC1L _ = False

isC1R :: Char -> Bool
isC1R 'g' = True
isC1R 'k' = True
isC1R 't' = True
isC1R 'b' = True
isC1R 'p' = True
isC1R 'f' = True
isC1R 'd' = True
isC1R _ = False


giveVowels :: WithError Syllable -> WithError Syllable
giveVowels (Left e) = Left e 
giveVowels (Right xs) = Right (Prelude.filter isVowel xs)

-- retorna la última sílaba de un verso
giveSyllable :: Verse -> WithError Syllable
giveSyllable "" = Left (SomethingIsEmpty "Verso vacío")
giveSyllable s = let s' = reverse s;
					 [(cons,rest)] = parse lastSyllable s'					 
				 in Right (reverse cons)


satisfyMetric :: Poem -> Metric -> Bool
satisfyMetric p ms = let syls = Prelude.map (giveVowels . giveSyllable) p;
						  takeSyllables _ [] = [];
						  takeSyllables s (n:ns) = (s!!n) : (takeSyllables s ns);
						  rhymes = Prelude.map (takeSyllables syls) ms
					  in and (Prelude.map haveRhyme rhymes)
							  
