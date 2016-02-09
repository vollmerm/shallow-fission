module Main where
import           System.Exit
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit

main = do
    putStrLn "This test always passes!"
    exitSuccess
