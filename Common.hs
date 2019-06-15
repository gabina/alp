module Common where

import Control.Monad
import System.IO (hFlush, stdout)

type Verse = String
type Poem = [Verse]
type Syllable = String
type Metric = [[Int]]
data Error = IsNotSyllable String | SomethingIsEmpty String 
type WithError a = Either Error a

-- retorna la última sílaba de un verso
giveSyllable :: Verse -> WithError Syllable
giveSyllable "" = Left SomethingIsEmpty "Verso vacío"
giveSyllable _ = Right "ron"

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

giveVowels :: Syllable -> Syllable -> Syllable
giveVowels "" = ""
giveVowels x::xs = if (isVowel x) then x::(giveVowels xs) else (giveVowels xs)

haveRhyme :: Syllable -> Syllable -> Bool
haveRhyme s0 "" = False
haveRhyme "" s1 = False
haveRhyme s0 s1 = (giveVowels s0) == (giveVowels s1)

satisfyMetric' :: Poem -> Metric -> WithError Bool
satisfyMetric' p m:ms = 

-- 
satisfyMetric :: Poem -> Metric -> WithError Bool
stisfyMetric [] _ = Left SomethingIsEmpty "Poema vacío"
satisfyMetric _ [] = Left SomethingIsEmpty "Métrica vacía"
satisfyMetric p m = 
