module TableDisplay
    ( renderSeason
    , renderStandings
    ) where

import Data.List (intercalate)
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
