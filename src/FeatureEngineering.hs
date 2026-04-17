module FeatureEngineering
    ( FixtureFeatures (..)
    , MatchOutcome (..)
    , TrainingExample (..)
    , buildTrainingExamples
    , featuresForFixture
    ) where

import Types

data MatchOutcome
    = HomeWinClass
    | DrawClass
    | AwayWinClass
    deriving (Eq, Ord, Read, Show)

data FixtureFeatures = FixtureFeatures
    { featureValues :: [Double]
    }
    deriving (Eq, Show)

data TrainingExample = TrainingExample
    { trainingFeatures :: FixtureFeatures
    , trainingOutcome :: MatchOutcome
    }
    deriving (Eq, Show)

data TeamFeatureState = TeamFeatureState
    { stateTeam :: Team
    , statePlayed :: Int
    , stateGoalsFor :: Int
    , stateGoalsAgainst :: Int
    , statePoints :: Int
    , stateRecentPoints :: [Int]
    , stateRecentGoalDifferences :: [Int]
    , stateStreakResult :: Maybe MatchOutcome
    , stateStreakLength :: Int
    }
    deriving (Eq, Show)

buildTrainingExamples :: FixtureList -> [TrainingExample]
buildTrainingExamples fixtures =
    reverse examples
  where
    (_, examples) =
        foldl addFixtureExample ([], []) (finishedFixtures fixtures)

    addFixtureExample (states, builtExamples) match =
        case matchResult match of
            Nothing -> (states, builtExamples)
            Just result ->
                ( updateStates match result states
                , TrainingExample
                    { trainingFeatures = featuresFromStates states match
                    , trainingOutcome = outcomeFor result
                    }
                    : builtExamples
                )

featuresForFixture :: FixtureList -> Match -> FixtureFeatures
featuresForFixture fixtures match =
    featuresFromStates states match
  where
    states =
        foldl addFinishedMatch [] (finishedFixtures fixtures)

    addFinishedMatch currentStates finishedMatch =
        case matchResult finishedMatch of
            Nothing -> currentStates
            Just result -> updateStates finishedMatch result currentStates

featuresFromStates :: [TeamFeatureState] -> Match -> FixtureFeatures
featuresFromStates states match =
    FixtureFeatures
        { featureValues =
            [ 1.0
            , ppg homeSummary / 3
            , ppg awaySummary / 3
            , goalsForPerGame homeSummary / 3
            , goalsForPerGame awaySummary / 3
            , goalsAgainstPerGame homeSummary / 3
            , goalsAgainstPerGame awaySummary / 3
            , recentPpg homeSummary / 3
            , recentPpg awaySummary / 3
            , recentGoalDifferencePerGame homeSummary / 3
            , recentGoalDifferencePerGame awaySummary / 3
            , winStreak homeSummary / 5
            , winStreak awaySummary / 5
            , lossStreak homeSummary / 5
            , lossStreak awaySummary / 5
            , 1.0
            ]
        }
  where
    homeSummary = summaryForTeam states (homeTeam match)
    awaySummary = summaryForTeam states (awayTeam match)

data TeamSummary = TeamSummary
    { ppg :: Double
    , goalsForPerGame :: Double
    , goalsAgainstPerGame :: Double
    , recentPpg :: Double
    , recentGoalDifferencePerGame :: Double
    , winStreak :: Double
    , lossStreak :: Double
    }

summaryForTeam :: [TeamFeatureState] -> Team -> TeamSummary
summaryForTeam states team =
    case filter ((== teamName team) . teamName . stateTeam) states of
        state : _ ->
            TeamSummary
                { ppg = safeRatio (fromIntegral (statePoints state)) played
                , goalsForPerGame = safeRatio (fromIntegral (stateGoalsFor state)) played
                , goalsAgainstPerGame = safeRatio (fromIntegral (stateGoalsAgainst state)) played
                , recentPpg = safeRatio (fromIntegral (sum (stateRecentPoints state))) (fromIntegral (length (stateRecentPoints state)))
                , recentGoalDifferencePerGame =
                    safeRatio
                        (fromIntegral (sum (stateRecentGoalDifferences state)))
                        (fromIntegral (length (stateRecentGoalDifferences state)))
                , winStreak = streakFor HomeWinClass state
                , lossStreak = streakFor AwayWinClass state
                }
          where
            played = fromIntegral (statePlayed state)
        [] ->
            TeamSummary
                { ppg = 1.25
                , goalsForPerGame = 1.35
                , goalsAgainstPerGame = 1.35
                , recentPpg = 1.25
                , recentGoalDifferencePerGame = 0
                , winStreak = 0
                , lossStreak = 0
                }

streakFor :: MatchOutcome -> TeamFeatureState -> Double
streakFor outcome state =
    case stateStreakResult state of
        Just currentOutcome
            | currentOutcome == outcome -> fromIntegral (stateStreakLength state)
        _ -> 0

updateStates :: Match -> Result -> [TeamFeatureState] -> [TeamFeatureState]
updateStates match result states =
    updateTeam awayTeamResult (awayTeam match) awayPoints awayGoalsFor awayGoalsAgainst $
        updateTeam homeTeamResult (homeTeam match) homePoints homeGoalsFor homeGoalsAgainst states
  where
    homeGoalsFor = resultHomeGoals result
    homeGoalsAgainst = resultAwayGoals result
    awayGoalsFor = resultAwayGoals result
    awayGoalsAgainst = resultHomeGoals result
    homePoints = pointsFor homeGoalsFor homeGoalsAgainst
    awayPoints = pointsFor awayGoalsFor awayGoalsAgainst
    homeTeamResult = teamOutcome homeGoalsFor homeGoalsAgainst
    awayTeamResult = teamOutcome awayGoalsFor awayGoalsAgainst

updateTeam :: MatchOutcome -> Team -> Int -> Int -> Int -> [TeamFeatureState] -> [TeamFeatureState]
updateTeam outcome team earnedPoints scored conceded states =
    replaceOrAdd updatedState states
  where
    previousState = stateForTeam team states
    updatedState =
        previousState
            { statePlayed = statePlayed previousState + 1
            , stateGoalsFor = stateGoalsFor previousState + scored
            , stateGoalsAgainst = stateGoalsAgainst previousState + conceded
            , statePoints = statePoints previousState + earnedPoints
            , stateRecentPoints = take 5 (earnedPoints : stateRecentPoints previousState)
            , stateRecentGoalDifferences = take 5 ((scored - conceded) : stateRecentGoalDifferences previousState)
            , stateStreakResult = Just outcome
            , stateStreakLength =
                case stateStreakResult previousState of
                    Just previousOutcome
                        | previousOutcome == outcome -> stateStreakLength previousState + 1
                    _ -> 1
            }

replaceOrAdd :: TeamFeatureState -> [TeamFeatureState] -> [TeamFeatureState]
replaceOrAdd state [] = [state]
replaceOrAdd state (candidate : rest)
    | teamName (stateTeam candidate) == teamName (stateTeam state) = state : rest
    | otherwise = candidate : replaceOrAdd state rest

stateForTeam :: Team -> [TeamFeatureState] -> TeamFeatureState
stateForTeam team states =
    case filter ((== teamName team) . teamName . stateTeam) states of
        state : _ -> state
        [] ->
            TeamFeatureState
                { stateTeam = team
                , statePlayed = 0
                , stateGoalsFor = 0
                , stateGoalsAgainst = 0
                , statePoints = 0
                , stateRecentPoints = []
                , stateRecentGoalDifferences = []
                , stateStreakResult = Nothing
                , stateStreakLength = 0
                }

outcomeFor :: Result -> MatchOutcome
outcomeFor result =
    teamOutcome (resultHomeGoals result) (resultAwayGoals result)

teamOutcome :: Int -> Int -> MatchOutcome
teamOutcome scored conceded
    | scored > conceded = HomeWinClass
    | scored == conceded = DrawClass
    | otherwise = AwayWinClass

pointsFor :: Int -> Int -> Int
pointsFor scored conceded
    | scored > conceded = 3
    | scored == conceded = 1
    | otherwise = 0

finishedFixtures :: FixtureList -> FixtureList
finishedFixtures fixtures =
    [ match
    | match <- fixtures
    , matchStatus match == Finished
    , matchResult match /= Nothing
    ]

safeRatio :: Double -> Double -> Double
safeRatio _ 0 = 0
safeRatio numerator denominator = numerator / denominator
