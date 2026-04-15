module SeasonSimulator
    ( simulateSeason
    , simulateSeasonWithSchedule
    ) where

import FixtureGenerator (generateDoubleRoundRobinFixtures)
import Predictor (predictFixturesWithSeasonContext)
import Standings (sortStandings, updateStandings)
import Types

simulateSeason :: [Team] -> FixtureList -> [PlayerStats] -> [PlayerAvailability] -> SeasonSimulation
simulateSeason teams historicalFixtures playerStats availability =
    simulateSeasonWithSchedule
        teams
        historicalFixtures
        playerStats
        availability
        (generateDoubleRoundRobinFixtures teams)

simulateSeasonWithSchedule :: [Team] -> FixtureList -> [PlayerStats] -> [PlayerAvailability] -> FixtureList -> SeasonSimulation
simulateSeasonWithSchedule teams historicalFixtures playerStats availability schedule =
    SeasonSimulation
        { simulatedTeams = teams
        , simulatedSchedule = schedule
        , simulatedMatchWeeks = reverse matchWeeks
        }
  where
    (_, matchWeeks) =
        foldl simulateMatchWeek ([], []) [1 .. maximumMatchDay schedule]

    simulateMatchWeek (completedFixtures, completedWeeks) matchWeekNumber =
        (completedFixtures ++ completedWeekFixtures, weekSimulation : completedWeeks)
      where
        weekFixtures =
            filter (\match -> matchDay match == matchWeekNumber) schedule
        predictions =
            predictFixturesWithSeasonContext
                historicalFixtures
                completedFixtures
                playerStats
                availability
                weekFixtures
        simulatedWeekFixtures =
            map simulateFixture predictions
        completedWeekFixtures =
            map simulatedFixture simulatedWeekFixtures
        weekSimulation =
            MatchWeekSimulation
                { simulatedMatchWeek = matchWeekNumber
                , simulatedFixtures = simulatedWeekFixtures
                , standingsAfterMatchWeek =
                    sortStandings (updateStandings teams (completedFixtures ++ completedWeekFixtures))
                }

simulateFixture :: FixturePrediction -> SimulatedFixture
simulateFixture prediction =
    SimulatedFixture
        { fixturePrediction = prediction
        , simulatedFixture =
            match
                { matchStatus = Finished
                , matchResult = Just (simulateResult prediction)
                }
        }
  where
    match = predictedMatch prediction

simulateResult :: FixturePrediction -> Result
simulateResult prediction =
    case selectedOutcome prediction of
        HomeOutcome ->
            winningHomeResult roundedHomeGoals roundedAwayGoals
        DrawOutcome ->
            drawResult roundedHomeGoals roundedAwayGoals
        AwayOutcome ->
            winningAwayResult roundedHomeGoals roundedAwayGoals
  where
    roundedHomeGoals = round (expectedHomeGoals prediction)
    roundedAwayGoals = round (expectedAwayGoals prediction)

data Outcome
    = HomeOutcome
    | DrawOutcome
    | AwayOutcome
    deriving (Eq, Show)

selectedOutcome :: FixturePrediction -> Outcome
selectedOutcome prediction
    | roll < homeWinProbability probabilities =
        HomeOutcome
    | roll < homeWinProbability probabilities + drawProbability probabilities =
        DrawOutcome
    | otherwise =
        AwayOutcome
  where
    probabilities = outcomeProbabilities prediction
    roll = deterministicRoll (predictedMatch prediction)

deterministicRoll :: Match -> Double
deterministicRoll match =
    fromIntegral seed / 10000
  where
    seed =
        abs
            ( (matchId match * 1103)
                + (teamId (homeTeam match) * 917)
                + (teamId (awayTeam match) * 613)
            )
            `mod` 10000

winningHomeResult :: Int -> Int -> Result
winningHomeResult homeGoals awayGoals
    | homeGoals > awayGoals = Result homeGoals awayGoals
    | otherwise = Result (awayGoals + 1) awayGoals

winningAwayResult :: Int -> Int -> Result
winningAwayResult homeGoals awayGoals
    | awayGoals > homeGoals = Result homeGoals awayGoals
    | otherwise = Result homeGoals (homeGoals + 1)

drawResult :: Int -> Int -> Result
drawResult homeGoals awayGoals =
    Result drawGoals drawGoals
  where
    drawGoals =
        max 0 (round ((fromIntegral homeGoals + fromIntegral awayGoals) / 2 :: Double))

maximumMatchDay :: FixtureList -> Int
maximumMatchDay [] = 0
maximumMatchDay fixtures =
    maximum (map matchDay fixtures)
