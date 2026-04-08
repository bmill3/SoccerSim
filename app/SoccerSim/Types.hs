module SoccerSim.Types
    ( Team (..)
    , Result (..)
    , MatchStatus (..)
    , Match (..)
    , Standing (..)
    , FixtureList
    , Season (..)
    ) where

data Team = Team
    { teamId :: Int
    , teamName :: String
    , teamShortName :: String
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

type FixtureList = [Match]

data Season = Season
    { seasonYear :: Int
    , seasonTeams :: [Team]
    , seasonFixtures :: FixtureList
    , seasonStandings :: [Standing]
    }
    deriving (Eq, Ord, Read, Show)
