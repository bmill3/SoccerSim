module FixtureGenerator
    ( generateRoundRobinFixtures
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
