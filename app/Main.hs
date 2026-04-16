module Main where

import AppData (renderCliReport)
import Gui (runSoccerSimGui)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] ->
            launchPreferredMode
        ["--gui"] ->
            runSoccerSimGui
        ["--cli"] ->
            putStrLn =<< renderCliReport
        _ ->
            die "Usage: SoccerSim [--gui | --cli]"

launchPreferredMode :: IO ()
launchPreferredMode =
    runSoccerSimGui
