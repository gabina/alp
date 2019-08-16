module Options where

import Common
import Functions
import Parsers
import Control.Monad.Writer
import Data.Set
import qualified Data.Set as Set

{-Cuarteto: estrofa de cuatro versos, normalmente endecası́labos, con patrón de rima consonante abba -}  
cuarteto :: Poem -> Input ()
cuarteto p = do f_out "Cuarteto"
                m <- checkMetric (Consonante 4 (fromList (Prelude.map fromList [[0,3],[1,2]])))
                showAnswer (satisfyMetric p m)

{-Redondilla: estrofa de cuatro versos, normalmente osctasílabos, con patrón de rima consonante abba-}
redondilla :: Poem -> Input ()
redondilla p = do f_out "Redondilla"
                  m <- checkMetric (Consonante 4 (fromList (Prelude.map fromList[[0,3],[1,2]])))
                  showAnswer (satisfyMetric p m)
                   
{-Seguidilla: estrofa de arte menor formada por cuatro versos. Los impares, heptası́labos y libres, y los pares, pentası́labos con rima asonante. El patrón de rima es abcb -}
seguidilla :: Poem -> Input ()
seguidilla p = do f_out "Seguidilla"
                  m <- checkMetric  (Asonante 4 (fromList (Prelude.map fromList [[1,3]])))
                  showAnswer (satisfyMetric p m)

{-Romance:  estrofa de arte menor formada por cuatro versos octosílabos, con el primero y el tercero libres, y el segundo y cuarto con rima asonante.-}
romance :: Poem -> Input ()
romance p = do f_out "Romance"
               m <- checkMetric  (Asonante 4 (fromList (Prelude.map fromList [[1,3]])))
               showAnswer (satisfyMetric p m)

{-Soneto: es una composición poética compuesta por catorce versos de arte mayor, endecası́labos en su forma clásica. Admite distintos patrones de rima, según la época y el autor. En este caso se considerará uno de los más utilizados: abbacddceffegg-}
soneto :: Poem -> Input ()
soneto p = do f_out "Soneto"
              m <- checkMetric  (Consonante 14 (fromList (Prelude.map fromList [[0,3],[1,2],[4,7],[5,6],[8,11],[9,10],[12,13]])))
              showAnswer (satisfyMetric p m)

{-Décima: estrofa constituida por diez versos octosı́labos. La estructura de rimas es fija en en abbaaccddc-}
decima :: Poem -> Input ()
decima p = do f_out "Décima"
              m <- checkMetric  (Asonante 10 (fromList (Prelude.map fromList [[0,3,4],[1,2],[5,6,9],[7,8]])))
              showAnswer (satisfyMetric p m)

help :: Poem -> Input ()
help _ = do f_out("Los poemas deben ser ingresados según el siguiente formato:\n verso1 / verso2 /... / versoN /*")

bye :: Poem -> Input ()
bye _ = exit "Programa finalizado"
