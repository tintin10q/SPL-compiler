{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module SPL.State where

import SPL.Codegen.SSM ( Instr )
import Data.Map (Map)
import Control.Monad (ap, liftM)
import Data.Map (Map)
import qualified Data.Map as Map


data State s a = ST (s -> (a,s))

instance Functor (State s) where
    fmap f (ST st) = ST (\s -> let (a , s') = st s in (f a, s'))

instance Applicative (State s) where
    ST getfun <*> ST st = ST (\s -> let (a , s') = st s
                                        (fun, s'') = getfun s'
                                      in (fun a, s''))
    pure a = ST (\s -> (a,s))

instance Monad (State s) where
    return = pure
    (ST st) >>= f = ST (\s -> let (a, s') = st s
                                  ST st' = f a
                              in st' s'
                        )
class Monad m => StateMonad m s where
    update :: (s -> s) -> m s

instance StateMonad (State s) s where
    update f = ST (\s -> (s, f s))

incr :: StateMonad m Int => m Int
incr = update (+5)

get :: State s a -> s -> (a, s)
get (ST st) = st


-- How about a thing that we make both a reader and writer monad. 
-- Writing increases the code and reading gives the enviroment!

data Generation a = Generation { 
                    codex :: [Instr],
                    variables :: Map String Int -- Maybe instead of int a data with offset and address 
                  } deriving (Show)

-- (#) :: Instr -> Generation -> Generation
-- (#) instr (ST st) = _

-- These 2 do nothing because which one to even apply it to? We don't need a in the Generation record, unless like we keep a log or something
instance Functor Generation where
    fmap _ (Generation code vars) = Generation code vars -- we can do this because a is not part of the record!

instance Applicative Generation where
    pure x = Generation [] Map.empty
    _ <*> (Generation code vars) = Generation code vars

instance Monad Generation where 
    return a = Generation [] Map.empty -- maybe if we say that a has to be ([Instr], Map)
    m >>= _ = let code = codex m
                  vars = variables m
              in Generation code vars

-- hoe preventen we env doorgeven
    
