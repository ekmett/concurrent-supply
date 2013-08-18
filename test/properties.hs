module Main where

import Control.Monad
import Control.Concurrent.Supply
import Data.Set
import System.Exit

reps = 2049

main = do
  supply <- newSupply
  let ids = loop supply reps
  let n = size (fromList ids)
  when (n == reps) exitSuccess
  putStrLn $ "Only " ++ show n ++ " out of " ++ show reps ++ " supplied identifiers are distinct"
  print ids
  exitFailure
  where
    loop s 0 = []
    loop s n = let (fId,s')  = freshId s
               in fId : loop s' (n-1)
