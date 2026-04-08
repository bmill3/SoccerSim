module Simulation
    ( applyResults
    ) where

import Types

applyResults :: [(Int, Result)] -> FixtureList -> FixtureList
applyResults results =
    map updateMatch
  where
    updateMatch match =
        case lookup (matchId match) results of
            Just result ->
                match
                    { matchStatus = Finished
                    , matchResult = Just result
                    }
            Nothing -> match
