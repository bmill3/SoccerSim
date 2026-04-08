module Standings
    ( completedMatches
    , createSeason
    , initialStanding
    , initialStandings
    , sortStandings
    , updateStandings
    ) where

import Data.List (sortBy)
import Types

initialStanding :: Team -> Standing
initialStanding team =
    Standing
        { standingTeam = team
        , gamesPlayed = 0
        , wins = 0
        , draws = 0
        , losses = 0
        , goalsFor = 0
        , goalsAgainst = 0
        , goalDifference = 0
        , points = 0
        }

initialStandings :: [Team] -> [Standing]
initialStandings = map initialStanding

completedMatches :: FixtureList -> [Match]
completedMatches = filter isCompleted
  where
    isCompleted match =
        matchStatus match == Finished && hasResult match

    hasResult match =
        case matchResult match of
            Just _ -> True
            Nothing -> False

updateStandings :: [Team] -> FixtureList -> [Standing]
updateStandings teams fixtures =
    foldl applyMatch (initialStandings teams) (completedMatches fixtures)

createSeason :: Int -> [Team] -> FixtureList -> Season
createSeason year teams fixtures =
    Season
        { seasonYear = year
        , seasonTeams = teams
        , seasonFixtures = fixtures
        , seasonStandings = sortStandings (updateStandings teams fixtures)
        }

sortStandings :: [Standing] -> [Standing]
sortStandings =
    sortBy compareStanding
  where
    compareStanding left right =
        compare (points right, goalDifference right, goalsFor right, teamName (standingTeam right))
                (points left, goalDifference left, goalsFor left, teamName (standingTeam left))

applyMatch :: [Standing] -> Match -> [Standing]
applyMatch standings match =
    case matchResult match of
        Nothing -> standings
        Just result ->
            map (applyTeamResult match result) standings

applyTeamResult :: Match -> Result -> Standing -> Standing
applyTeamResult match result standing
    | standingTeam standing == homeTeam match =
        recordGame
            (resultHomeGoals result)
            (resultAwayGoals result)
            standing
    | standingTeam standing == awayTeam match =
        recordGame
            (resultAwayGoals result)
            (resultHomeGoals result)
            standing
    | otherwise = standing

recordGame :: Int -> Int -> Standing -> Standing
recordGame goalsScored goalsConceded standing =
    Standing
        { standingTeam = standingTeam standing
        , gamesPlayed = gamesPlayed standing + 1
        , wins = wins standing + winIncrement
        , draws = draws standing + drawIncrement
        , losses = losses standing + lossIncrement
        , goalsFor = goalsFor standing + goalsScored
        , goalsAgainst = goalsAgainst standing + goalsConceded
        , goalDifference = newGoalDifference
        , points = points standing + pointsEarned
        }
  where
    winIncrement
        | goalsScored > goalsConceded = 1
        | otherwise = 0

    drawIncrement
        | goalsScored == goalsConceded = 1
        | otherwise = 0

    lossIncrement
        | goalsScored < goalsConceded = 1
        | otherwise = 0

    pointsEarned
        | goalsScored > goalsConceded = 3
        | goalsScored == goalsConceded = 1
        | otherwise = 0

    newGoalDifference =
        (goalsFor standing + goalsScored) - (goalsAgainst standing + goalsConceded)
