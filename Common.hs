module Common where

import Control.Monad
import System.IO (hFlush, stdout)
import Parsing

type Verse = String
type Poem = [Verse]
type Syllable = String
data Metric = Asonante Int [[Int]] | Consonante Int [[Int]]
data Error = IsNotSyllable String | SomethingIsEmpty String deriving (Eq, Show)
type WithError a = Either Error a


type Command = (String,Input ())
-- Necesario para algunas versiones de GHC (Por esto tambien se importa Control.Monad)

instance Functor Input where 

  fmap = liftM

instance Applicative Input where

  pure = return
  (<*>) = ap

---------------------------------------------------------------------------------------

data Input a = IN { runInput :: IO (Either String a) }


instance Monad Input where

        return = IN . return . Right
        x >>= f = IN ( do either_value <- runInput x
                          case either_value of 
                                Right a -> runInput (f a)
                                Left e -> return (Left e) )

class Monad m => IOMonad m where

        f_in :: m String
        f_out :: String -> m ()
        f_out_sl :: String -> m ()
        f_out_list :: [String] -> m ()

instance IOMonad Input where

        f_in = lift (do hFlush stdout
                        getLine)
        f_out s = lift (putStrLn s)
        f_out_sl s = lift (putStr s)
        f_out_list xs = mapM_ (\s -> f_out "" >> f_out s) xs

class Monad m => ExeptionMonad m where

        throw :: String -> m ()
        exit :: String -> m a

instance ExeptionMonad Input where

        throw s = do f_out s
                     return ()
        exit s = IN (return (Left s))


lift :: IO a -> Input a
lift m = IN { runInput = m >>= \x -> return (Right x) }
