{-# LANGUAGE InstanceSigs #-}

module Common where

import Control.Monad
import System.IO (hFlush, stdout)

import Parsing
import Data.Set
import qualified Data.Set as Set

type Verse = String
type Poem = [Verse]
type Syllable = String
data Metric = Asonante Int (Set (Set Int)) | Consonante Int (Set (Set Int))

type Command = (String,Input ())

data Input a = IN { runInput :: IO (Either String a) }
instance Functor Input where 

  fmap = liftM

instance Applicative Input where

  pure = return
  (<*>) = ap

instance Monad Input where

		return :: a -> Input a
		return = IN . return . Right
		
		(>>=) :: Input a -> (a -> Input b) -> Input b
		x >>= f = IN ( do either_value <- runInput x
		                  case either_value of 
							Right a -> runInput (f a)
							Left e -> return (Left e))

{-Funciones útiles para trabajar con Input -}

{-f_out imprime algo en pantalla -}							
f_out :: String -> Input ()
f_out s = lift (putStrLn s)

{-f_in lee de la pantalla -}		
f_in :: Input String
f_in = lift getLine

{-f_out_list imprime una lista de strings -}
f_out_list :: [String] -> Input ()		
f_out_list xs = mapM_ (\s -> f_out "" >> f_out s) xs

{-exit se utiliza para salir de Méri, ya sea por un error interno o por decisión del usuario -}
exit :: String -> Input a
exit s = IN (return (Left s))

lift :: IO a -> Input a
lift m = IN { runInput = m >>= return . Right }


{- Para (ayudar a) evitar la multiplicidad de representaciones, se creó la función checkMetric, que se encarga de chequear que todos los conjuntos sean disjuntos uno a uno y que ningún elemento del conjunto sea mayor o igual a la cantidad de versos.-} 
metricIsOk :: Metric -> Bool
metricIsOk (Asonante n s) = let ss = Set.foldl union Set.empty s;
								f n0 = n0 < n
							in (size ss == (sum (Prelude.map size (toList s)))) && and (Set.map f ss) 
metricIsOk (Consonante n s) = let ss = Set.foldl union Set.empty s;
								  f n0 = n0 < n
							  in (size ss == (sum (Prelude.map size (toList s)))) && and (Set.map f ss) 							  

checkMetric :: Metric -> Input Metric
checkMetric m = if (metricIsOk m) then (return m) else exit "Error interno. Métrica mal formada."


