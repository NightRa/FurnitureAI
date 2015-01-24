module Main where
import Model

main :: IO ()
main = do
        putStrLn "Starting search.."
        putStrLn (format $ bestPath swapGoal swapInitial)

