module Main where

import DataLoader (loadSampleTeams)
import FixtureGenerator (generateRoundRobinFixtures)
import Predictor (predictFixtures, predictStandings)
import PremierLeagueData (loadPremierLeagueHistory)
import Simulation (applyResults)
import Standings (createSeason)
import TableDisplay (renderFixturePredictions, renderPredictedStandings, renderSeason)
import Types (Match (..), MatchStatus (..), Result (..), Season)

main :: IO ()
main = do
    putStrLn (renderSeason sampleSeason)
    putStrLn "Premier League model trained on 2022-23, 2023-24, and 2024-25 results."
    (teams, historicalFixtures) <- loadPremierLeagueHistory
    let previewFixtures = map clearResult (take 6 historicalFixtures)
        predictions = predictFixtures historicalFixtures previewFixtures
        projectedTable = take 10 (predictStandings teams historicalFixtures previewFixtures)
    putStrLn ""
    putStrLn "Sample fixture probabilities:"
    putStrLn (renderFixturePredictions predictions)
    putStrLn "Projected standings preview:"
    putStrLn (renderPredictedStandings projectedTable)

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

clearResult :: Match -> Match
clearResult match =
    match
        { matchStatus = Scheduled
        , matchResult = Nothing
        }
