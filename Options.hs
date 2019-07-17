module Options where

import Common
import Functions
import Parsers
import Control.Monad.Writer
import qualified Data.Text.IO as Txt

{-Cuarteto: estrofa de cuatro versos, normalmente endecası́labos, con patrón de rima consonante abba -}  
cuarteto :: Input ()
cuarteto = do f_out "Cuarteto"
              p <- get
              showAnswer (cuarteto' p)
              
cuarteto' :: Poem -> Writer [String] Bool
cuarteto' p = satisfyMetric p (Consonante 4 [[0,3],[1,2]]) 

{-Redondilla: estrofa de cuatro versos, normalmente osctasílabos, con patrón de rima consonante abba-}
redondilla :: Input ()
redondilla = do f_out "Redondilla"
                p <- get
                showAnswer (redondilla' p)
              
redondilla' :: Poem -> Writer [String] Bool
redondilla' p = satisfyMetric p (Consonante 4 [[0,3],[1,2]]) 

{-Seguidilla: estrofa de arte menor formada por cuatro versos. Los impares, heptası́labos y libres, y los pares, pentası́labos con rima asonante. El patrón de rima es abcb -}
seguidilla :: Input ()
seguidilla = do f_out "Seguidilla"
                p <- get
                showAnswer (seguidilla' p)
              
seguidilla' :: Poem -> Writer [String] Bool
seguidilla' p = satisfyMetric p (Asonante 4 [[1,3]]) 

{-Romance:  estrofa de arte menor formada por cuatro versos octosílabos, con el primero y el tercero libres, y el segundo y cuarto con rima asonante.-}
romance :: Input ()
romance = do f_out "Romance"
             p <- get
             showAnswer (romance' p)
              
romance' :: Poem -> Writer [String] Bool
romance' p = satisfyMetric p (Asonante 4 [[1,3]]) 

{-Soneto: es una composición poética compuesta por catorce versos de arte mayor, endecası́labos en su forma clásica. Admite distintos patrones de rima, según la época y el autor. En este caso se considerará uno de los más utilizados: abbacddceffegg-}
soneto :: Input ()
soneto = do f_out "Serventesio"
            p <- get
            showAnswer (decima' p)

soneto' :: Poem -> Writer [String] Bool
soneto' p = satisfyMetric p (Consonante 14 [[0,3],[1,2],[4,7],[5,6],[8,11],[9,10],[12,13]]) 

{-Décima: estrofa constituida por diez versos octosı́labos. La estructura de rimas es fija en en abbaaccddc-}
decima :: Input ()
decima = do f_out "Décima"
            p <- get
            showAnswer (decima' p)
 
decima' :: Poem -> Writer [String] Bool
decima' p = satisfyMetric p (Consonante 10 [[0,3,4],[1,2],[5,6,9],[7,8]]) 

{-Patrones no utilizados -} 
lira :: Input ()
lira = do f_out "Lira"
          p <- get
          showAnswer (decima' p)

lira' :: Poem -> Writer [String] Bool
lira' p = satisfyMetric p (Consonante 5 [[0,2],[1,3,4]]) 

octava :: Input ()
octava = do f_out "Octava Real"
            p <- get
            showAnswer (decima' p)

octava' :: Poem -> Writer [String] Bool
octava' p = satisfyMetric p (Consonante 8 [[0,2,4],[1,3,5],[6,7]])  

  
 
serventesio :: Input ()
serventesio = do f_out "Serventesio"
                 p <- get
                 showAnswer (decima' p)

serventesio' :: Poem -> Writer [String] Bool
serventesio' p = satisfyMetric p (Consonante 4 [[0,2],[1,3]]) 



help :: Input ()
help = do f_out_list (["Los poemas deben ser ingresados según el siguiente formato:",
					   "verso1 / verso2 /... / versoN /*"])
