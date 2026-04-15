module FixtureGenerator
    ( generateDoubleRoundRobinFixtures
    , generateRoundRobinFixtures
    ) where

import Types

generateRoundRobinFixtures :: [Team] -> FixtureList
generateRoundRobinFixtures teams =
    zipWith buildMatch [1 ..] pairings
  where
    pairings = createPairings teams

    buildMatch matchNumber (dayNumber, home, away) =
        Match
            { matchId = matchNumber
            , matchDay = dayNumber
            , homeTeam = home
            , awayTeam = away
            , matchStatus = Scheduled
            , matchResult = Nothing
            }

createPairings :: [Team] -> [(Int, Team, Team)]
createPairings teams =
    concatMap roundForHomeTeam indexedTeams
  where
    indexedTeams = zip [1 ..] teams

    roundForHomeTeam (homeIndex, home) =
        [ (homeIndex, home, away)
        | (awayIndex, away) <- indexedTeams
        , awayIndex > homeIndex
        ]

generateDoubleRoundRobinFixtures :: [Team] -> FixtureList
generateDoubleRoundRobinFixtures teams =
    zipWith buildMatch [1 ..] (firstHalfPairings ++ secondHalfPairings)
  where
    rounds = createBalancedRounds teams
    firstHalfPairings =
        concatMap
            (\(matchWeek, pairings) -> [(matchWeek, home, away) | (home, away) <- pairings])
            rounds
    secondHalfPairings =
        [ (matchWeek + length rounds, away, home)
        | (matchWeek, home, away) <- firstHalfPairings
        ]

    buildMatch matchNumber (dayNumber, home, away) =
        Match
            { matchId = matchNumber
            , matchDay = dayNumber
            , homeTeam = home
            , awayTeam = away
            , matchStatus = Scheduled
            , matchResult = Nothing
            }

createBalancedRounds :: [Team] -> [(Int, [(Team, Team)])]
createBalancedRounds [] = []
createBalancedRounds teams =
    zip [1 ..] (map orientRound indexedRounds)
  where
    indexedRounds =
        zip [0 ..] (take (length teams - 1) (iterate rotateTeams teams))

    orientRound (roundIndex, roundTeams) =
        [ orientPair roundIndex pairIndex pair
        | (pairIndex, pair) <- zip [0 ..] (roundPairs roundTeams)
        ]

roundPairs :: [Team] -> [(Team, Team)]
roundPairs teams =
    zip frontHalf (reverse backHalf)
  where
    halfLength = length teams `div` 2
    frontHalf = take halfLength teams
    backHalf = drop halfLength teams

orientPair :: Int -> Int -> (Team, Team) -> (Team, Team)
orientPair roundIndex pairIndex (left, right)
    | even (roundIndex + pairIndex) = (left, right)
    | otherwise = (right, left)

rotateTeams :: [Team] -> [Team]
rotateTeams [] = []
rotateTeams [team] = [team]
rotateTeams (fixedTeam : rotatingTeams) =
    fixedTeam : last rotatingTeams : init rotatingTeams
