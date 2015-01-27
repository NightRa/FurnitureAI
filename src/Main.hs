module Main where
import Model
import Data.Array hiding (index)
import System.Console.ANSI
import Data.IORef
import Data.Maybe

main :: IO ()
main = do
        setTitle "Furniture AI"
        showState 0 states
        frame <- newIORef (0 :: Nat)
        loop frame size
        return ()
       where
        result = fromJust $ bestPath swapGoal swapInitial
        size = fromIntegral $ length result
        states :: Array Nat State
        states = listArray (0,size - 1) result
        loop :: IORef Nat -> Nat -> IO ()
        loop ref size = do
                 c <- getChar
                 action size ref c
                 index <- readIORef ref
                 showState index states
                 loop ref size
        -- putStrLn (format $ bestPath swapGoal swapInitial)

showState :: Nat -> Array Nat State -> IO ()
showState frame arr = do
                       clearScreen
                       setCursorPosition 0 0
                       putStrLn $ "Result frame " ++ show frame
                       print $ arr ! frame

back :: Nat -> Nat
back 0 = 0
back n = n - 1

next :: Nat -> Nat -> Nat
next size n | n == size - 1 = n
next _    n                 = n + 1

action :: Nat -> IORef Nat -> Char -> IO ()
action _    ref 'l' = modifyIORef' ref back
action size ref 'r' = modifyIORef' ref (next size)
action _    _   _   = return ()
