module Types
    ( FixtureList
    , Match (..)
    , MatchStatus (..)
    , PredictedStanding (..)
    , FixturePrediction (..)
    , OutcomeProbabilities (..)
    , AvailabilityStatus (..)
    , Player (..)
    , PlayerAvailability (..)
    , PlayerStats (..)
    , Position (..)
    , Result (..)
    , Season (..)
    , SeasonSimulation (..)
    , MatchWeekSimulation (..)
    , SimulatedFixture (..)
    , Standing (..)
    , Team (..)
    ) where

data Team = Team
    { teamId :: Int
    , teamName :: String
    , teamShortName :: String
    }
    deriving (Eq, Ord, Read, Show)

data Position
    = Goalkeeper
    | Defender
    | Midfielder
    | Forward
    deriving (Eq, Ord, Read, Show)

data Player = Player
    { playerId :: Int
    , playerName :: String
    , playerTeam :: Team
    , playerPosition :: Position
    }
    deriving (Eq, Ord, Read, Show)

data PlayerStats = PlayerStats
    { playerStatsPlayer :: Player
    , playerAppearances :: Int
    , playerStarts :: Int
    , playerMinutes :: Int
    , playerGoals :: Int
    , playerAssists :: Int
    , playerShotsOnTarget :: Int
    , playerTackles :: Int
    , playerInterceptions :: Int
    , playerSaves :: Int
    , playerRating :: Maybe Double
    }
    deriving (Eq, Ord, Read, Show)

data AvailabilityStatus
    = Available
    | Starting
    | Benched
    | Injured
    | Suspended
    | NotCalledUp
    deriving (Eq, Ord, Read, Show)

data PlayerAvailability = PlayerAvailability
    { availabilityPlayer :: Player
    , availabilityStatus :: AvailabilityStatus
    , expectedMinutes :: Int
    }
    deriving (Eq, Ord, Read, Show)

data Result = Result
    { resultHomeGoals :: Int
    , resultAwayGoals :: Int
    }
    deriving (Eq, Ord, Read, Show)

data MatchStatus
    = Scheduled
    | InProgress
    | Finished
    deriving (Eq, Ord, Read, Show)

data Match = Match
    { matchId :: Int
    , matchDay :: Int
    , homeTeam :: Team
    , awayTeam :: Team
    , matchStatus :: MatchStatus
    , matchResult :: Maybe Result
    }
    deriving (Eq, Ord, Read, Show)

type FixtureList = [Match]

data Standing = Standing
    { standingTeam :: Team
    , gamesPlayed :: Int
    , wins :: Int
    , draws :: Int
    , losses :: Int
    , goalsFor :: Int
    , goalsAgainst :: Int
    , goalDifference :: Int
    , points :: Int
    }
    deriving (Eq, Ord, Read, Show)

data Season = Season
    { seasonYear :: Int
    , seasonTeams :: [Team]
    , seasonFixtures :: FixtureList
    , seasonStandings :: [Standing]
    }
    deriving (Eq, Ord, Read, Show)

data OutcomeProbabilities = OutcomeProbabilities
    { homeWinProbability :: Double
    , drawProbability :: Double
    , awayWinProbability :: Double
    }
    deriving (Eq, Ord, Read, Show)

data FixturePrediction = FixturePrediction
    { predictedMatch :: Match
    , outcomeProbabilities :: OutcomeProbabilities
    , expectedHomeGoals :: Double
    , expectedAwayGoals :: Double
    }
    deriving (Eq, Ord, Read, Show)

data PredictedStanding = PredictedStanding
    { predictedTeam :: Team
    , predictedGamesPlayed :: Int
    , predictedPoints :: Double
    , predictedGoalDifference :: Double
    }
    deriving (Eq, Ord, Read, Show)

data SimulatedFixture = SimulatedFixture
    { fixturePrediction :: FixturePrediction
    , simulatedFixture :: Match
    }
    deriving (Eq, Ord, Read, Show)

data MatchWeekSimulation = MatchWeekSimulation
    { simulatedMatchWeek :: Int
    , simulatedFixtures :: [SimulatedFixture]
    , standingsAfterMatchWeek :: [Standing]
    }
    deriving (Eq, Ord, Read, Show)

data SeasonSimulation = SeasonSimulation
    { simulatedTeams :: [Team]
    , simulatedSchedule :: FixtureList
    , simulatedMatchWeeks :: [MatchWeekSimulation]
    }
    deriving (Eq, Ord, Read, Show)
