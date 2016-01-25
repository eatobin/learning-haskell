import System.IO
import Control.Concurrent.STM

type Account = TVar Int

withdraw :: Account -> Int -> STM ()
withdraw acc amount = do
    bal <- readTVar acc
    writeTVar acc (bal - amount)

deposit :: Account -> Int -> STM ()
deposit acc amount = withdraw acc (- amount)

transfer :: Account -> Account -> Int -> IO ()
-- Transfer ’amount’ from account ’from’ to account ’to’
transfer from to amount
    = atomically (do deposit to amount
                     withdraw from amount)

showAccount :: Account -> IO Int
showAccount acc = readTVarIO acc

main = do
    from <- atomically (newTVar 200)
    to   <- atomically (newTVar 108)
    transfer from to 50
    v1 <- showAccount from
    v2 <- showAccount to
    putStrLn $ (show v1) ++ ", " ++ (show v2)


-- type Box = TVar String
--
-- writeBox :: Box -> String -> IO ()
-- writeBox box string = writeTVar box string
--
-- --showBox :: Box -> IO String
-- showBox box = readTVarIO box
--
-- start = do
--   box <- atomically (newTVar "Eric")
--   vv <- showBox box
--   putStrLn (show vv)
--   writeBox box "Karen"
--   ww <- showBox box
--   putStrLn (show ww)
