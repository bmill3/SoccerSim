module Main where

import AppData (GuiData (..), GuiWeek (..), loadGuiData)
import Data.List (nub, sort)
import FeatureEngineering (buildTrainingExamples, featureValues, featuresForFixture, trainingFeatures)
import FixtureGenerator (generateDoubleRoundRobinFixtures, generateRoundRobinFixtures)
import MLModel (predictOutcomeProbabilities, trainLogisticModel)
import PlayerData (loadKeyPlayerData)
import Predictor (predictFixture, predictFixtureWithPlayers, predictStandings)
import PremierLeagueData (loadLatestPremierLeagueTeams, loadPremierLeagueHistory)
import SeasonSimulator (simulateSeason)
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
    runTest "ml training creates valid outcome probabilities" testMachineLearningProbabilities
    runTest "predictor creates valid fixture probabilities" testPredictFixture
    runTest "player availability adjusts fixture probabilities" testPlayerAvailabilityAdjustsPrediction
    runTest "double round robin creates 38 balanced match weeks" testDoubleRoundRobinSeasonSchedule
    runTest "season simulator produces week by week standings" testSeasonSimulation
    runTest "gui data exposes structured weeks and final standings" testGuiData
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

testMachineLearningProbabilities :: IO ()
testMachineLearningProbabilities = do
    (_, historicalFixtures) <- loadPremierLeagueHistory
    let examples = buildTrainingExamples historicalFixtures
        model = trainLogisticModel examples
        fixture =
            (head historicalFixtures)
                { matchStatus = Scheduled
                , matchResult = Nothing
                }
        featureRow = featuresForFixture historicalFixtures fixture
        probabilities = predictOutcomeProbabilities model (featureValues featureRow)
        probabilityTotal =
            homeWinProbability probabilities
                + drawProbability probabilities
                + awayWinProbability probabilities
    assertEqual "Expected one ML training row per historical fixture." 1140 (length examples)
    assertBool "Expected ML features to include multiple pre-match values." (length (featureValues (trainingFeatures (head examples))) > 10)
    assertBool "Expected trained ML probabilities to sum to 1." (probabilityTotal > 0.999 && probabilityTotal < 1.001)

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
    (playerStats, availability) <- loadKeyPlayerData teams
    assertEqual "Expected one key player per Premier League team." 20 (length playerStats)
    assertEqual "Expected one availability row per key player." 20 (length availability)
    let fixture =
            (head historicalFixtures)
                { matchStatus = Scheduled
                , matchResult = Nothing
                }
        basePrediction = predictFixture historicalFixtures fixture
        scenarioAvailability =
            case availability of
                firstAvailability : rest ->
                    firstAvailability {availabilityStatus = Injured, expectedMinutes = 0} : rest
                [] -> []
        playerAwarePrediction = predictFixtureWithPlayers historicalFixtures playerStats scenarioAvailability fixture
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

testDoubleRoundRobinSeasonSchedule :: IO ()
testDoubleRoundRobinSeasonSchedule = do
    teams <- loadLatestPremierLeagueTeams
    let fixtures = generateDoubleRoundRobinFixtures teams
        matchWeeks = sort (nub (map matchDay fixtures))
    assertEqual "Expected 20 teams from the latest Premier League season file." 20 (length teams)
    assertEqual "Expected 380 fixtures in a 20-team double round robin." 380 (length fixtures)
    assertEqual "Expected 38 match weeks." [1 .. 38] matchWeeks
    assertBool "Expected every match week to have 10 fixtures." (all ((== 10) . fixturesInWeek fixtures) [1 .. 38])
    assertBool "Expected each team to play 19 home fixtures." (all ((== 19) . homeFixturesFor fixtures) teams)
    assertBool "Expected each team to play 19 away fixtures." (all ((== 19) . awayFixturesFor fixtures) teams)

testSeasonSimulation :: IO ()
testSeasonSimulation = do
    teams <- loadLatestPremierLeagueTeams
    (_, historicalFixtures) <- loadPremierLeagueHistory
    (playerStats, availability) <- loadKeyPlayerData teams
    let simulation = simulateSeason teams historicalFixtures playerStats availability
        matchWeeks = simulatedMatchWeeks simulation
        finalWeek = last matchWeeks
    assertEqual "Expected 38 simulated match weeks." 38 (length matchWeeks)
    assertBool "Expected every simulated match week to have 10 fixtures." (all ((== 10) . length . simulatedFixtures) matchWeeks)
    assertEqual "Expected final standings to include all 20 teams." 20 (length (standingsAfterMatchWeek finalWeek))
    assertBool
        "Expected every final standing to show a full 38-game season."
        (all ((== 38) . gamesPlayed) (standingsAfterMatchWeek finalWeek))

testGuiData :: IO ()
testGuiData = do
    guiData <- loadGuiData
    assertEqual "Expected 38 GUI match weeks." 38 (length (guiMatchWeeks guiData))
    assertEqual "Expected 20 final GUI standings rows." 20 (length (guiFinalStandings guiData))
    let firstWeek = head (guiMatchWeeks guiData)
    assertEqual "Expected 10 GUI fixtures in the first week." 10 (length (guiWeekFixtures firstWeek))
    assertEqual "Expected 20 GUI standings rows in the first week." 20 (length (guiWeekStandings firstWeek))

findStanding :: String -> [Standing] -> Standing
findStanding shortName standings =
    case filter matchesTeam standings of
        [standing] -> standing
        _ -> error ("Could not find standing for team " ++ shortName)
  where
    matchesTeam standing = teamShortName (standingTeam standing) == shortName

fixturesInWeek :: FixtureList -> Int -> Int
fixturesInWeek fixtures weekNumber =
    length (filter (\match -> matchDay match == weekNumber) fixtures)

homeFixturesFor :: FixtureList -> Team -> Int
homeFixturesFor fixtures team =
    length (filter (\match -> homeTeam match == team) fixtures)

awayFixturesFor :: FixtureList -> Team -> Int
awayFixturesFor fixtures team =
    length (filter (\match -> awayTeam match == team) fixtures)
