module Functions where

import Common
import Parsing
import Data.Ratio
import Control.Monad.Writer


showAnswer :: Writer [String] a -> Input ()
showAnswer a = let (b,xs) = runWriter a in f_out_list xs

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
haveRhyme (s:syls) = and (map (== s) syls)

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

ataqueL' :: Parser Char
ataqueL' = sat isC1L

ataqueR' :: Parser Char
ataqueR' = sat isC1R

ataqueL :: Parser String
ataqueL = do x <- ataqueL'
             char 'l'
             return [x,'l']
             
ataqueR :: Parser String
ataqueR = do x <- ataqueR'
             char 'l'
             return [x,'l']             

ataque3 :: Parser String
ataque3 = do c <- isNotVowel
             char 's'
             return ([c,'s'])
             <|> do c <- isNotVowel
                    return [c]

nucleo :: Parser String
nucleo = do v <- vowel
            (do a <- ataque3
                return (v:a)
                <|> return [v])
            <|> do d <- diphthong
                   (do a <- ataque3
                       return (d++a)
                       <|> return d)
                       
lastSyllable1 :: Parser String
lastSyllable1 = do l <- ataqueL
                   n <- nucleo
                   return (l++n)
                   <|>  do r <- ataqueR
                           n <- nucleo
                           return (r++n)
                           <|> do c <- isNotVowel
                                  n <- nucleo
                                  return (c:n)
                                              
syllable :: Parser [Syllable]
syllable = do l <- ataqueL
              n <- nucleo
              s <- syllable
              return ((l++n):s)
              <|>  do r <- ataqueR
                      n <- nucleo
                      s <- syllable
                      return ((r++n):s)
                      <|>  do c <- isNotVowel
                              n <- nucleo
                              s <- syllable
                              return ((c:n):s)
                              <|> do ls <- lastSyllable1
                                     return [ls]

giveVowels :: WithError Syllable -> WithError Syllable
giveVowels (Left e) = Left e 
giveVowels (Right xs) = Right (filter isVowel xs)

-- retorna la última sílaba de un verso
giveSyllable :: Verse -> WithError Syllable
giveSyllable "" = Left (SomethingIsEmpty "Verso vacío")
giveSyllable s = let s' = reverse s;
					 [(cons,rest)] = parse lastSyllable s'					 
				 in Right (reverse cons)


satisfyMetric' :: Poem -> Metric -> Bool
satisfyMetric' p ms = let syls = map (giveVowels . giveSyllable) p;
						  takeSyllables _ [] = [];
						  takeSyllables s (n:ns) = (s!!n) : (takeSyllables s ns);
						  rhymes = map (takeSyllables syls) ms
					  in and (map haveRhyme rhymes)
							  

satisfyMetric :: Poem -> Metric -> WithError Bool
stisfyMetric [] _ = Left (SomethingIsEmpty "Poema vacío")
satisfyMetric _ [] = Left (SomethingIsEmpty "Métrica vacía")
satisfyMetric p m = Right (satisfyMetric' p m)
