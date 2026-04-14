module TableDisplay
    ( renderSeason
    , renderFixturePredictions
    , renderPredictedStandings
    , renderStandings
    ) where

import Data.List (intercalate)
import Text.Printf (printf)
import Types

renderSeason :: Season -> String
renderSeason season =
    unlines
        [ "Season " ++ show (seasonYear season)
        , "Teams: " ++ show (length (seasonTeams season))
        , "Fixtures: " ++ show (length (seasonFixtures season))
        , ""
        , renderStandings (seasonStandings season)
        ]

renderStandings :: [Standing] -> String
renderStandings standings =
    unlines (header : separator : map renderStandingRow (zip [1 :: Int ..] standings))
  where
    header = intercalate " | " ["Pos", "Team", "P", "W", "D", "L", "GF", "GA", "GD", "Pts"]
    separator = replicate (length header) '-'

renderStandingRow :: (Int, Standing) -> String
renderStandingRow (position, standing) =
    intercalate
        " | "
        [ show position
        , teamShortName (standingTeam standing)
        , show (gamesPlayed standing)
        , show (wins standing)
        , show (draws standing)
        , show (losses standing)
        , show (goalsFor standing)
        , show (goalsAgainst standing)
        , show (goalDifference standing)
        , show (points standing)
        ]

renderFixturePredictions :: [FixturePrediction] -> String
renderFixturePredictions predictions =
    unlines (header : separator : map renderPredictionRow predictions)
  where
    header = intercalate " | " ["Fixture", "Home", "Draw", "Away", "xG"]
    separator = replicate (length header) '-'

renderPredictionRow :: FixturePrediction -> String
renderPredictionRow prediction =
    intercalate
        " | "
        [ teamShortName (homeTeam match) ++ " vs " ++ teamShortName (awayTeam match)
        , formatPercent (homeWinProbability probabilities)
        , formatPercent (drawProbability probabilities)
        , formatPercent (awayWinProbability probabilities)
        , formatDouble (expectedHomeGoals prediction) ++ "-" ++ formatDouble (expectedAwayGoals prediction)
        ]
  where
    match = predictedMatch prediction
    probabilities = outcomeProbabilities prediction

renderPredictedStandings :: [PredictedStanding] -> String
renderPredictedStandings standings =
    unlines (header : separator : map renderPredictedStandingRow (zip [1 :: Int ..] standings))
  where
    header = intercalate " | " ["Pos", "Team", "P", "xPts", "xGD"]
    separator = replicate (length header) '-'

renderPredictedStandingRow :: (Int, PredictedStanding) -> String
renderPredictedStandingRow (position, standing) =
    intercalate
        " | "
        [ show position
        , teamShortName (predictedTeam standing)
        , show (predictedGamesPlayed standing)
        , formatDouble (predictedPoints standing)
        , formatDouble (predictedGoalDifference standing)
        ]

formatPercent :: Double -> String
formatPercent value =
    printf "%.1f%%" (value * 100)

formatDouble :: Double -> String
formatDouble value =
    printf "%.1f" value
