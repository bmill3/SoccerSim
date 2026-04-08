module SoccerSim.Example
    ( exampleFixtures
    , exampleSeason
    , exampleTeams
    ) where

import SoccerSim.Season (createSeason)
import SoccerSim.Types

exampleTeams :: [Team]
exampleTeams =
    [ Team 1 "Albuquerque Atoms" "AAT"
    , Team 2 "Santa Fe Strikers" "SFS"
    , Team 3 "Taos Thunder" "TTH"
    , Team 4 "Roswell Rockets" "RRO"
    ]

exampleFixtures :: FixtureList
exampleFixtures =
    [ Match 1 1 (exampleTeams !! 0) (exampleTeams !! 1) Finished (Just (Result 2 1))
    , Match 2 1 (exampleTeams !! 2) (exampleTeams !! 3) Finished (Just (Result 0 0))
    , Match 3 2 (exampleTeams !! 0) (exampleTeams !! 2) Scheduled Nothing
    , Match 4 2 (exampleTeams !! 1) (exampleTeams !! 3) Scheduled Nothing
    ]

exampleSeason :: Season
exampleSeason = createSeason 2026 exampleTeams exampleFixtures
