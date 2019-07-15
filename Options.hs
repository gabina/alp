module Options where

import Common
import Functions
import Parsers
import Control.Monad.Writer
import qualified Data.Text.IO as Txt


{- rima consonante entre los versos 1-4 y 2-3. Si la rima es entre los versos 1-3 y 2-4 no es cuarteto, sino serventesio. Los cuartetos pueden usarse como base para hacer poemas más largos.-}

{-
cuarteto :: Input ()
cuarteto = do p <- readPoem
              return ()              
-}
 
cuarteto :: Input ()
cuarteto = do f_out "Cuarteto"
              p <- get
              showAnswer (cuarteto' p)
              
cuarteto' :: Poem -> Writer [String] Bool
cuarteto' p = satisfyMetric p (Consonante 4 [[0,3],[1,2]]) 
 
decima :: Input ()
decima = undefined
 
lira :: Input ()
lira = undefined

octava :: Input ()
octava = undefined
 
redondilla :: Input ()
redondilla = undefined
 
serventesio :: Input ()
serventesio = undefined

soneto :: Input ()
soneto = undefined
