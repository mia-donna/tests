module Main where

import Control.Concurrent  ( threadDelay, forkIO , takeMVar , putMVar , newEmptyMVar , MVar , newMVar , readMVar )
import Text.Printf ( printf )
import Control.Monad ( forever )
import System.IO
import Prelude hiding (lookup)
import Data.Map (Map, (!) )
import Data.Set as Set
import qualified Data.Map as Map
import Control.Concurrent.STM
import Data.Maybe
import System.Random

-- TYPE DEFINITIONS

data Customer = Customer {
  name :: Name,
  accountBalance :: AccountBalance,
  accountNumber :: AccountNumber
} deriving (Eq, Show)

data AccountNumber = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten  deriving (Show, Eq)
type AccountBalance =  Int
type Name = String

-- PROCESS that spawns a customer thread for each customer
customers :: Customer -> MVar Customer -> IO ()
customers customer_thread box = do 
   putMVar box customer_thread
   putStrLn $ "The customer in the box is " ++ (show customer_thread)
   customers customer_thread box


main :: IO ()
main = do
  --CREATE 10 VALUES OF TYPE CUSTOMER
  putStrLn $ "Creating 10 customers.."
  let c1 = Customer {name = "C1", accountBalance = 2000, accountNumber = One}
  let c2 = Customer {name = "C2", accountBalance = 1100, accountNumber = Two} 
  let c3 = Customer {name = "C3", accountBalance = 9000, accountNumber = Three}
  let c4 = Customer {name = "C4", accountBalance = 2500, accountNumber = Four}
  let c5 = Customer {name = "C5", accountBalance = 2020, accountNumber = Five}
  let c6 = Customer {name = "C6", accountBalance = 2090, accountNumber = Six}
  let c7 = Customer {name = "C3", accountBalance = 7050, accountNumber = Seven}
  let c8 = Customer {name = "C8", accountBalance = 5000, accountNumber = Eight}
  let c9 = Customer {name = "C9", accountBalance = 6050, accountNumber = Nine}
  let c10 = Customer {name = "C10", accountBalance = 8500, accountNumber = Ten}
  putStrLn $ "10 customers created."

  --SPAWN 10 CUSTOMER THREADS
  putStrLn $ "Creating 10 threads.."
  box <- newEmptyMVar
  mapM_ forkIO [customers c1 box, customers c2 box,customers c3 box, customers c4 box, customers c5 box, customers c6 box, customers c7 box, customers c8 box, customers c9 box, customers c10 box]
  putStrLn $ "10 threads created."
  --customer_a <- takeMVar box
  --putStrLn $ "The customer in the box is " ++ (show customer_a)
  putStrLn $ "end of main"

-- we can see which thread is running  
-- next we need to select two of them