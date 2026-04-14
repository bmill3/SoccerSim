module Main where

import FixtureGenerator (generateRoundRobinFixtures)
import PlayerData (loadSamplePlayerData)
import Predictor (predictFixture, predictFixtureWithPlayers, predictStandings)
import PremierLeagueData (loadPremierLeagueHistory)
import Simulation (applyResults)
import Standings (createSeason, updateStandings)
import Types

main :: IO ()
main = do
    runTest "fixture generation creates each pairing once" testFixtureGeneration
    runTest "simulation applies only matching results" testApplyResults
    runTest "standings update wins draws losses and points" testUpdateStandings
    runTest "season creation sorts standings by points and tiebreakers" testCreateSeason
    runTest "premier league history loads from csv files" testPremierLeagueHistoryLoads
    runTest "predictor creates valid fixture probabilities" testPredictFixture
    runTest "player availability adjusts fixture probabilities" testPlayerAvailabilityAdjustsPrediction
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

assertBool :: String -> Bool -> IO ()
assertBool message condition
    | condition = pure ()
    | otherwise = error message

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

testPremierLeagueHistoryLoads :: IO ()
testPremierLeagueHistoryLoads = do
    (teams, fixtures) <- loadPremierLeagueHistory
    assertBool "Expected more than one Premier League season worth of teams." (length teams > 20)
    assertEqual "Expected three complete 380-match seasons." 1140 (length fixtures)
    assertBool "Expected every loaded match to be finished." (all ((== Finished) . matchStatus) fixtures)

testPredictFixture :: IO ()
testPredictFixture = do
    (teams, historicalFixtures) <- loadPremierLeagueHistory
    let fixture =
            (head historicalFixtures)
                { matchStatus = Scheduled
                , matchResult = Nothing
                }
        prediction = predictFixture historicalFixtures fixture
        probabilities = outcomeProbabilities prediction
        probabilityTotal =
            homeWinProbability probabilities
                + drawProbability probabilities
                + awayWinProbability probabilities
        projectedStandings = predictStandings teams historicalFixtures [fixture]
    assertBool "Expected home win probability to be positive." (homeWinProbability probabilities > 0)
    assertBool "Expected draw probability to be positive." (drawProbability probabilities > 0)
    assertBool "Expected away win probability to be positive." (awayWinProbability probabilities > 0)
    assertBool "Expected probabilities to sum close to 1." (probabilityTotal > 0.99 && probabilityTotal <= 1.001)
    assertEqual "Expected one projected standing per team." (length teams) (length projectedStandings)

testPlayerAvailabilityAdjustsPrediction :: IO ()
testPlayerAvailabilityAdjustsPrediction = do
    (teams, historicalFixtures) <- loadPremierLeagueHistory
    (playerStats, availability) <- loadSamplePlayerData teams
    let fixture =
            (head historicalFixtures)
                { matchStatus = Scheduled
                , matchResult = Nothing
                }
        basePrediction = predictFixture historicalFixtures fixture
        playerAwarePrediction = predictFixtureWithPlayers historicalFixtures playerStats availability fixture
        baseProbabilities = outcomeProbabilities basePrediction
        playerProbabilities = outcomeProbabilities playerAwarePrediction
    assertBool
        "Expected player availability to adjust home win probability."
        (homeWinProbability baseProbabilities /= homeWinProbability playerProbabilities)
    assertBool
        "Expected player availability to adjust expected goals."
        ( expectedHomeGoals basePrediction /= expectedHomeGoals playerAwarePrediction
            || expectedAwayGoals basePrediction /= expectedAwayGoals playerAwarePrediction
        )

findStanding :: String -> [Standing] -> Standing
findStanding shortName standings =
    case filter matchesTeam standings of
        [standing] -> standing
        _ -> error ("Could not find standing for team " ++ shortName)
  where
    matchesTeam standing = teamShortName (standingTeam standing) == shortName
