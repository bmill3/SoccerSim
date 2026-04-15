module Main where

import DataLoader (loadSampleTeams)
import FixtureGenerator (generateRoundRobinFixtures)
import PlayerData (loadSamplePlayerData)
import PremierLeagueData (loadLatestPremierLeagueTeams, loadPremierLeagueHistory)
import SeasonSimulator (simulateSeason)
import Simulation (applyResults)
import Standings (createSeason)
import TableDisplay (renderSeason, renderSeasonSimulation)
import Types (Result (..), Season)

main :: IO ()
main = do
    putStrLn (renderSeason sampleSeason)
    putStrLn "Premier League model trained on 2022-23, 2023-24, and 2024-25 results."
    (_, historicalFixtures) <- loadPremierLeagueHistory
    seasonTeams <- loadLatestPremierLeagueTeams
    (playerStats, availability) <- loadSamplePlayerData seasonTeams
    let simulation = simulateSeason seasonTeams historicalFixtures playerStats availability
    putStrLn ""
    putStrLn "Full 38-match-week simulation with player availability, in-season performance, and streak form:"
    putStrLn (renderSeasonSimulation simulation)

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
