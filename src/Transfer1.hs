module Transfer1 (main1) where

import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    check,
    newTVar,
    newTVarIO,
    readTVar,
    readTVarIO,
    writeTVar,
  )

type Account = TVar Int

withdraw :: Account -> Int -> STM ()
withdraw accFrom amount = do
  bal <- readTVar accFrom
  check (amount >= 0 && amount <= bal)
  writeTVar accFrom (bal - amount)

deposit :: Account -> Int -> STM ()
deposit accTo amount = do
  bal <- readTVar accTo
  check (amount >= 0)
  writeTVar accTo (bal + amount)

transfer :: Account -> Account -> Int -> IO ()
transfer accFrom accTo amount =
  atomically
    ( do
        withdraw accFrom amount
        deposit accTo amount
    )

showAccount :: Account -> IO Int
showAccount = readTVarIO

showBalance :: Account -> Account -> IO ()
showBalance accFrom accTo = do
  x <- showAccount accFrom
  y <- showAccount accTo
  putStrLn $ "FROM balance: $" <> show x
  putStrLn $ "TO balance: $" <> show y

main1 :: IO ()
main1 = do
  accFrom <- newTVarIO 200
  accTo <- newTVarIO 200
  showBalance accFrom accTo
  putStrLn "Transfering $50 from 'FROM' to 'TO'"
  transfer accFrom accTo 50
  putStrLn "Done!"
  showBalance accFrom accTo
