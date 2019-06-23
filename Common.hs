module Common where

import Control.Monad
import System.IO (hFlush, stdout)
import Parsing

type Verse = String
type Poem = [Verse]
type Syllable = String
type Metric = [[Int]]
data Error = IsNotSyllable String | SomethingIsEmpty String deriving Eq
type WithError a = Either Error a

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

vowel :: Parser Char
vowel = sat isVowel

closed :: Parser Char
closed = sat isClosed

opened :: Parser Char
opened = sat (\x -> and [(isVowel x),(not (isClosed x))])

isNotVowel :: Parser Char
isNotVowel = sat (\x -> not (isVowel x))

diphthong :: Parser String
diphthong = do c <- closed
               (do a <- opened
                   return ([c,a])
                   <|> do c1 <- closed
                          return ([c,c1]))

giveVowels :: Syllable -> Syllable
giveVowels xs = filter isVowel xs

haveRhyme :: [WithError Syllable] -> Bool
haveRhyme [] = True
haveRhyme (s:syls) = and (map (== s) syls)

-- Consume la última sílaba de un verso al revés
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

-- retorna la última sílaba de un verso
giveSyllable :: Verse -> WithError Syllable
giveSyllable "" = Left (SomethingIsEmpty "Verso vacío")
giveSyllable s = let s' = reverse s;
					 [(cons,rest)] = parse lastSyllable s'					 
				 in Right (reverse cons)


satisfyMetric' :: Poem -> Metric -> Bool
satisfyMetric' p ms = let syls = map giveSyllable p;
						  takeSyllables _ [] = [];
						  takeSyllables s (n:ns) = (s!!n) : (takeSyllables s ns);
						  rhymes = map (takeSyllables syls) ms
					  in and (map haveRhyme rhymes)
							  

satisfyMetric :: Poem -> Metric -> WithError Bool
stisfyMetric [] _ = Left (SomethingIsEmpty "Poema vacío")
satisfyMetric _ [] = Left (SomethingIsEmpty "Métrica vacía")
satisfyMetric p m = Right (satisfyMetric' p m)
