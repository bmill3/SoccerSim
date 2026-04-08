module Main where

import FixtureGenerator (generateRoundRobinFixtures)
import Simulation (applyResults)
import Standings (createSeason, updateStandings)
import Types

main :: IO ()
main = do
    runTest "fixture generation creates each pairing once" testFixtureGeneration
    runTest "simulation applies only matching results" testApplyResults
    runTest "standings update wins draws losses and points" testUpdateStandings
    runTest "season creation sorts standings by points and tiebreakers" testCreateSeason
    putStrLn "All tests passed."

runTest :: String -> IO () -> IO ()
runTest name testAction = do
    putStrLn ("[TEST] " ++ name)
    testAction

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual message expected actual
    | expected == actual = pure ()
    | otherwise =
        error
            (message ++ "\nExpected: " ++ show expected ++ "\nActual:   " ++ show actual)

testFixtureGeneration :: IO ()
testFixtureGeneration = do
    let teams =
            [ Team 1 "A" "A"
            , Team 2 "B" "B"
            , Team 3 "C" "C"
            , Team 4 "D" "D"
            ]
        fixtures = generateRoundRobinFixtures teams
        pairings =
            [ (teamShortName (homeTeam match), teamShortName (awayTeam match))
            | match <- fixtures
            ]
    assertEqual "Expected six round-robin fixtures for four teams." 6 (length fixtures)
    assertEqual
        "Expected each unique pairing exactly once."
        [("A", "B"), ("A", "C"), ("A", "D"), ("B", "C"), ("B", "D"), ("C", "D")]
        pairings

testApplyResults :: IO ()
testApplyResults = do
    let teams =
            [ Team 1 "A" "A"
            , Team 2 "B" "B"
            , Team 3 "C" "C"
            ]
        fixtures = generateRoundRobinFixtures teams
        updatedFixtures = applyResults [(1, Result 2 1)] fixtures
        firstMatch = head updatedFixtures
        secondMatch = updatedFixtures !! 1
    assertEqual "Expected the first match to be marked finished." Finished (matchStatus firstMatch)
    assertEqual
        "Expected the first match result to be stored."
        (Just (Result 2 1))
        (matchResult firstMatch)
    assertEqual "Expected untouched matches to remain scheduled." Scheduled (matchStatus secondMatch)
    assertEqual "Expected untouched matches to have no result." Nothing (matchResult secondMatch)

testUpdateStandings :: IO ()
testUpdateStandings = do
    let teams =
            [ Team 1 "A" "A"
            , Team 2 "B" "B"
            , Team 3 "C" "C"
            ]
        fixtures =
            applyResults
                [ (1, Result 3 1)
                , (2, Result 0 0)
                ]
                (generateRoundRobinFixtures teams)
        standings = updateStandings teams fixtures
        teamA = findStanding "A" standings
        teamB = findStanding "B" standings
        teamC = findStanding "C" standings
    assertEqual "Team A should have one win." 1 (wins teamA)
    assertEqual "Team A should have one draw." 1 (draws teamA)
    assertEqual "Team A should have four points." 4 (points teamA)
    assertEqual "Team B should have one loss." 1 (losses teamB)
    assertEqual "Team B should have one goal scored." 1 (goalsFor teamB)
    assertEqual "Team C should have one draw." 1 (draws teamC)
    assertEqual "Team C should have one point." 1 (points teamC)

testCreateSeason :: IO ()
testCreateSeason = do
    let teams =
            [ Team 1 "Alpha" "ALP"
            , Team 2 "Bravo" "BRV"
            , Team 3 "Charlie" "CHR"
            ]
        fixtures =
            applyResults
                [ (1, Result 2 0)
                , (2, Result 1 0)
                ]
                (generateRoundRobinFixtures teams)
        season = createSeason 2026 teams fixtures
        orderedTeams = map (teamShortName . standingTeam) (seasonStandings season)
    assertEqual
        "Standings should be sorted by points, then goal difference, then goals for."
        ["ALP", "CHR", "BRV"]
        orderedTeams

findStanding :: String -> [Standing] -> Standing
findStanding shortName standings =
    case filter matchesTeam standings of
        [standing] -> standing
        _ -> error ("Could not find standing for team " ++ shortName)
  where
    matchesTeam standing = teamShortName (standingTeam standing) == shortName
