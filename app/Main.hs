module Main where

import DataLoader (loadSampleTeams)
import FixtureGenerator (generateRoundRobinFixtures)
import Simulation (applyResults)
import Standings (createSeason)
import TableDisplay (renderSeason)
import Types (Result (..), Season)

main :: IO ()
main = putStrLn (renderSeason sampleSeason)

sampleSeason :: Season
sampleSeason = createSeason 2026 teams completedFixtures
  where
    teams = loadSampleTeams
    scheduledFixtures = generateRoundRobinFixtures teams
    completedFixtures =
        applyResults
            [ (1, Result 2 1)
            , (2, Result 0 0)
            , (3, Result 3 2)
            ]
            scheduledFixtures
