module Predictor
    ( predictFixture
    , predictFixtureWithPlayers
    , predictFixtures
    , predictFixturesWithPlayers
    , predictStandings
    , predictStandingsWithPlayers
    ) where

import Data.List (sortBy)
import Types

data TeamStats = TeamStats
    { statsTeam :: Team
    , statsPlayed :: Int
    , statsGoalsFor :: Int
    , statsGoalsAgainst :: Int
    }
    deriving (Eq, Show)

predictFixtures :: FixtureList -> FixtureList -> [FixturePrediction]
predictFixtures historicalFixtures fixtures =
    predictFixturesWithPlayers historicalFixtures [] [] fixtures

predictFixture :: FixtureList -> Match -> FixturePrediction
predictFixture historicalFixtures match =
    predictFixtureWithPlayers historicalFixtures [] [] match

predictFixturesWithPlayers :: FixtureList -> [PlayerStats] -> [PlayerAvailability] -> FixtureList -> [FixturePrediction]
predictFixturesWithPlayers historicalFixtures playerStats availability fixtures =
    map (predictFixtureWithPlayers historicalFixtures playerStats availability) fixtures

predictFixtureWithPlayers :: FixtureList -> [PlayerStats] -> [PlayerAvailability] -> Match -> FixturePrediction
predictFixtureWithPlayers historicalFixtures playerStats availability match =
    FixturePrediction
        { predictedMatch = match
        , outcomeProbabilities = probabilities
        , expectedHomeGoals = homeExpectedGoals
        , expectedAwayGoals = awayExpectedGoals
        }
  where
    model = buildModel historicalFixtures
    homeStats = findStats (homeTeam match) model
    awayStats = findStats (awayTeam match) model
    homeExpectedGoals =
        clampExpectedGoals
            ( baseHomeExpectedGoals
                * teamAttackAvailabilityFactor (homeTeam match) playerStats availability
                * teamDefenseAvailabilityFactor (awayTeam match) playerStats availability
            )
    awayExpectedGoals =
        clampExpectedGoals
            ( baseAwayExpectedGoals
                * teamAttackAvailabilityFactor (awayTeam match) playerStats availability
                * teamDefenseAvailabilityFactor (homeTeam match) playerStats availability
            )
    baseHomeExpectedGoals =
        leagueHomeGoalsPerMatch historicalFixtures
            * attackStrength homeStats model
            * defenseWeakness awayStats model
    baseAwayExpectedGoals =
        leagueAwayGoalsPerMatch historicalFixtures
            * attackStrength awayStats model
            * defenseWeakness homeStats model
    probabilities = scoreProbabilities homeExpectedGoals awayExpectedGoals

predictStandings :: [Team] -> FixtureList -> FixtureList -> [PredictedStanding]
predictStandings teams historicalFixtures fixtures =
    predictStandingsWithPlayers teams historicalFixtures [] [] fixtures

predictStandingsWithPlayers :: [Team] -> FixtureList -> [PlayerStats] -> [PlayerAvailability] -> FixtureList -> [PredictedStanding]
predictStandingsWithPlayers teams historicalFixtures playerStats availability fixtures =
    sortPredictedStandings (map projectTeam teams)
  where
    fixturePredictions = predictFixturesWithPlayers historicalFixtures playerStats availability fixtures

    projectTeam team =
        PredictedStanding
            { predictedTeam = team
            , predictedGamesPlayed = actualGames + projectedGames
            , predictedPoints = actualPoints + projectedPoints
            , predictedGoalDifference = actualGoalDifference + projectedGoalDifference
            }
      where
        actualMatches =
            filter (isFinishedMatchFor team) fixtures
        actualGames =
            length actualMatches
        actualPoints =
            fromIntegral (sum (map (actualPointsFor team) actualMatches))
        actualGoalDifference =
            fromIntegral (sum (map (actualGoalDifferenceFor team) actualMatches))
        futurePredictions =
            filter (isPredictionFor team) fixturePredictions
        projectedGames =
            length futurePredictions
        projectedPoints =
            sum (map (expectedPointsFor team) futurePredictions)
        projectedGoalDifference =
            sum (map (expectedGoalDifferenceFor team) futurePredictions)

buildModel :: FixtureList -> [TeamStats]
buildModel fixtures =
    foldl addMatch initialStats finishedFixtures
  where
    finishedFixtures =
        [ match
        | match <- fixtures
        , matchStatus match == Finished
        , matchResult match /= Nothing
        ]
    teams =
        uniqueTeams (concatMap (\match -> [homeTeam match, awayTeam match]) finishedFixtures)
    initialStats =
        map emptyStats teams

emptyStats :: Team -> TeamStats
emptyStats team =
    TeamStats
        { statsTeam = team
        , statsPlayed = 0
        , statsGoalsFor = 0
        , statsGoalsAgainst = 0
        }

addMatch :: [TeamStats] -> Match -> [TeamStats]
addMatch stats match =
    case matchResult match of
        Nothing -> stats
        Just result ->
            map (addTeamMatch match result) stats

addTeamMatch :: Match -> Result -> TeamStats -> TeamStats
addTeamMatch match result stats
    | statsTeam stats == homeTeam match =
        addGame (resultHomeGoals result) (resultAwayGoals result) stats
    | statsTeam stats == awayTeam match =
        addGame (resultAwayGoals result) (resultHomeGoals result) stats
    | otherwise = stats

addGame :: Int -> Int -> TeamStats -> TeamStats
addGame scored conceded stats =
    stats
        { statsPlayed = statsPlayed stats + 1
        , statsGoalsFor = statsGoalsFor stats + scored
        , statsGoalsAgainst = statsGoalsAgainst stats + conceded
        }

findStats :: Team -> [TeamStats] -> TeamStats
findStats team model =
    case filter (\stats -> statsTeam stats == team) model of
        stats : _ -> stats
        [] -> averageStats team model

averageStats :: Team -> [TeamStats] -> TeamStats
averageStats team model =
    TeamStats
        { statsTeam = team
        , statsPlayed = 1
        , statsGoalsFor = roundedAverage statsGoalsFor
        , statsGoalsAgainst = roundedAverage statsGoalsAgainst
        }
  where
    roundedAverage field =
        round (safeAverage (map (fromIntegral . field) model))

attackStrength :: TeamStats -> [TeamStats] -> Double
attackStrength stats model =
    safeRatio (goalsForPerMatch stats) (safeAverage (map goalsForPerMatch model))

defenseWeakness :: TeamStats -> [TeamStats] -> Double
defenseWeakness stats model =
    safeRatio (goalsAgainstPerMatch stats) (safeAverage (map goalsAgainstPerMatch model))

goalsForPerMatch :: TeamStats -> Double
goalsForPerMatch stats =
    safeRatio (fromIntegral (statsGoalsFor stats)) (fromIntegral (statsPlayed stats))

goalsAgainstPerMatch :: TeamStats -> Double
goalsAgainstPerMatch stats =
    safeRatio (fromIntegral (statsGoalsAgainst stats)) (fromIntegral (statsPlayed stats))

leagueHomeGoalsPerMatch :: FixtureList -> Double
leagueHomeGoalsPerMatch fixtures =
    safeAverage
        [ fromIntegral (resultHomeGoals result)
        | match <- fixtures
        , Just result <- [matchResult match]
        ]

leagueAwayGoalsPerMatch :: FixtureList -> Double
leagueAwayGoalsPerMatch fixtures =
    safeAverage
        [ fromIntegral (resultAwayGoals result)
        | match <- fixtures
        , Just result <- [matchResult match]
        ]

teamAttackAvailabilityFactor :: Team -> [PlayerStats] -> [PlayerAvailability] -> Double
teamAttackAvailabilityFactor team playerStats availability =
    availabilityFactor attackingImpact team playerStats availability

teamDefenseAvailabilityFactor :: Team -> [PlayerStats] -> [PlayerAvailability] -> Double
teamDefenseAvailabilityFactor team playerStats availability =
    safeRatio 1 (availabilityFactor defensiveImpact team playerStats availability)

availabilityFactor :: (PlayerStats -> Double) -> Team -> [PlayerStats] -> [PlayerAvailability] -> Double
availabilityFactor impactFor team playerStats availability
    | baselineImpact <= 0 = 1
    | otherwise = clampAvailabilityFactor (availableImpact / baselineImpact)
  where
    teamPlayerStats =
        filter (\stats -> playerTeam (playerStatsPlayer stats) == team) playerStats
    baselineImpact =
        sum (map impactFor teamPlayerStats)
    availableImpact =
        sum
            [ impactFor stats * availabilityWeight stats availability
            | stats <- teamPlayerStats
            ]

attackingImpact :: PlayerStats -> Double
attackingImpact stats =
    roleWeight * minutesShare stats * ratingWeight stats
        + 0.18 * fromIntegral (playerGoals stats)
        + 0.12 * fromIntegral (playerAssists stats)
        + 0.03 * fromIntegral (playerShotsOnTarget stats)
  where
    roleWeight =
        case playerPosition (playerStatsPlayer stats) of
            Forward -> 1.25
            Midfielder -> 0.85
            Defender -> 0.35
            Goalkeeper -> 0.05

defensiveImpact :: PlayerStats -> Double
defensiveImpact stats =
    roleWeight * minutesShare stats * ratingWeight stats
        + 0.04 * fromIntegral (playerTackles stats)
        + 0.05 * fromIntegral (playerInterceptions stats)
        + 0.03 * fromIntegral (playerSaves stats)
  where
    roleWeight =
        case playerPosition (playerStatsPlayer stats) of
            Goalkeeper -> 1.3
            Defender -> 1.1
            Midfielder -> 0.65
            Forward -> 0.2

availabilityWeight :: PlayerStats -> [PlayerAvailability] -> Double
availabilityWeight stats availability =
    case filter matchesPlayer availability of
        currentAvailability : _ ->
            statusWeight (availabilityStatus currentAvailability)
                * minutesWeight (expectedMinutes currentAvailability)
        [] ->
            statusWeight Available
  where
    player = playerStatsPlayer stats
    matchesPlayer currentAvailability =
        availabilityPlayer currentAvailability == player

statusWeight :: AvailabilityStatus -> Double
statusWeight status =
    case status of
        Starting -> 1.05
        Available -> 0.85
        Benched -> 0.35
        Injured -> 0
        Suspended -> 0
        NotCalledUp -> 0

minutesWeight :: Int -> Double
minutesWeight minutesValue =
    max 0 (min 1.1 (fromIntegral minutesValue / 90))

minutesShare :: PlayerStats -> Double
minutesShare stats =
    safeRatio (fromIntegral (playerMinutes stats)) (38 * 90)

ratingWeight :: PlayerStats -> Double
ratingWeight stats =
    case playerRating stats of
        Just rating -> max 0.8 (min 1.25 (rating / 6.8))
        Nothing -> 1

scoreProbabilities :: Double -> Double -> OutcomeProbabilities
scoreProbabilities homeExpected awayExpected =
    OutcomeProbabilities
        { homeWinProbability = homeWin
        , drawProbability = draw
        , awayWinProbability = awayWin
        }
  where
    scoreRange = [0 .. 10]
    scoreProbabilitiesByScore =
        [ (homeGoals, awayGoals, poisson homeExpected homeGoals * poisson awayExpected awayGoals)
        | homeGoals <- scoreRange
        , awayGoals <- scoreRange
        ]
    homeWin =
        sum [probability | (homeGoals, awayGoals, probability) <- scoreProbabilitiesByScore, homeGoals > awayGoals]
    draw =
        sum [probability | (homeGoals, awayGoals, probability) <- scoreProbabilitiesByScore, homeGoals == awayGoals]
    awayWin =
        sum [probability | (homeGoals, awayGoals, probability) <- scoreProbabilitiesByScore, homeGoals < awayGoals]

poisson :: Double -> Int -> Double
poisson lambda goals =
    exp (-lambda) * (lambda ^ goals) / fromIntegral (factorial goals)

factorial :: Int -> Int
factorial number =
    product [1 .. max 1 number]

expectedPointsFor :: Team -> FixturePrediction -> Double
expectedPointsFor team prediction
    | team == homeTeam match =
        3 * homeWinProbability probabilities + drawProbability probabilities
    | team == awayTeam match =
        3 * awayWinProbability probabilities + drawProbability probabilities
    | otherwise = 0
  where
    match = predictedMatch prediction
    probabilities = outcomeProbabilities prediction

expectedGoalDifferenceFor :: Team -> FixturePrediction -> Double
expectedGoalDifferenceFor team prediction
    | team == homeTeam match = expectedHomeGoals prediction - expectedAwayGoals prediction
    | team == awayTeam match = expectedAwayGoals prediction - expectedHomeGoals prediction
    | otherwise = 0
  where
    match = predictedMatch prediction

actualPointsFor :: Team -> Match -> Int
actualPointsFor team match =
    case matchResult match of
        Nothing -> 0
        Just result
            | team == homeTeam match && resultHomeGoals result > resultAwayGoals result -> 3
            | team == awayTeam match && resultAwayGoals result > resultHomeGoals result -> 3
            | resultHomeGoals result == resultAwayGoals result -> 1
            | otherwise -> 0

actualGoalDifferenceFor :: Team -> Match -> Int
actualGoalDifferenceFor team match =
    case matchResult match of
        Nothing -> 0
        Just result
            | team == homeTeam match -> resultHomeGoals result - resultAwayGoals result
            | team == awayTeam match -> resultAwayGoals result - resultHomeGoals result
            | otherwise -> 0

isFinishedMatchFor :: Team -> Match -> Bool
isFinishedMatchFor team match =
    matchStatus match == Finished && isMatchFor team match

isPredictionFor :: Team -> FixturePrediction -> Bool
isPredictionFor team prediction =
    matchStatus match == Scheduled && isMatchFor team match
  where
    match = predictedMatch prediction

isMatchFor :: Team -> Match -> Bool
isMatchFor team match =
    team == homeTeam match || team == awayTeam match

sortPredictedStandings :: [PredictedStanding] -> [PredictedStanding]
sortPredictedStandings =
    sortBy compareStanding
  where
    compareStanding left right =
        compare
            ( predictedPoints right
            , predictedGoalDifference right
            , teamName (predictedTeam right)
            )
            ( predictedPoints left
            , predictedGoalDifference left
            , teamName (predictedTeam left)
            )

uniqueTeams :: [Team] -> [Team]
uniqueTeams =
    foldl addUnique []
  where
    addUnique seen team
        | team `elem` seen = seen
        | otherwise = seen ++ [team]

safeAverage :: [Double] -> Double
safeAverage [] = 1
safeAverage values = sum values / fromIntegral (length values)

safeRatio :: Double -> Double -> Double
safeRatio _ 0 = 1
safeRatio numerator denominator = numerator / denominator

clampExpectedGoals :: Double -> Double
clampExpectedGoals =
    max 0.2 . min 5.0

clampAvailabilityFactor :: Double -> Double
clampAvailabilityFactor =
    max 0.65 . min 1.2
