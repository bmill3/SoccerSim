module AppData
    ( GuiData (..)
    , GuiFixture (..)
    , GuiStanding (..)
    , GuiWeek (..)
    , buildSampleSeason
    , loadGuiData
    , loadPremierLeagueSimulation
    , renderCliReport
    ) where

import Data.Maybe (listToMaybe)
import DataLoader (loadSampleTeams)
import FixtureGenerator (generateRoundRobinFixtures)
import PlayerData (loadKeyPlayerData)
import PremierLeagueData (loadLatestPremierLeagueTeams, loadPremierLeagueHistory)
import SeasonSimulator (simulateSeason)
import Simulation (applyResults)
import Standings (createSeason)
import TableDisplay (renderMatchWeek, renderSeason, renderSeasonSimulation, renderStandings)
import Types

data GuiStanding = GuiStanding
    { guiStandingPosition :: Int
    , guiStandingTeamName :: String
    , guiStandingTeamShortName :: String
    , guiStandingPlayed :: Int
    , guiStandingWins :: Int
    , guiStandingDraws :: Int
    , guiStandingLosses :: Int
    , guiStandingGoalsFor :: Int
    , guiStandingGoalsAgainst :: Int
    , guiStandingGoalDifference :: Int
    , guiStandingPoints :: Int
    }
    deriving (Eq, Show)

data GuiFixture = GuiFixture
    { guiFixtureMatchup :: String
    , guiFixtureHomeTeamName :: String
    , guiFixtureHomeTeamShortName :: String
    , guiFixtureAwayTeamName :: String
    , guiFixtureAwayTeamShortName :: String
    , guiFixtureHomeWinProbability :: Double
    , guiFixtureDrawProbability :: Double
    , guiFixtureAwayWinProbability :: Double
    , guiFixtureExpectedHomeGoals :: Double
    , guiFixtureExpectedAwayGoals :: Double
    , guiFixtureResult :: String
    , guiFixtureSimulatedHomeGoals :: Int
    , guiFixtureSimulatedAwayGoals :: Int
    }
    deriving (Eq, Show)

data GuiWeek = GuiWeek
    { guiWeekNumber :: Int
    , guiWeekReportText :: String
    , guiWeekFixtures :: [GuiFixture]
    , guiWeekStandings :: [GuiStanding]
    }
    deriving (Eq, Show)

data GuiData = GuiData
    { guiOverviewText :: String
    , guiSampleSeasonText :: String
    , guiFinalStandings :: [GuiStanding]
    , guiMatchWeeks :: [GuiWeek]
    }
    deriving (Eq, Show)

renderCliReport :: IO String
renderCliReport = do
    simulation <- loadPremierLeagueSimulation
    pure $
        unlines
            [ renderSeason buildSampleSeason
            , seasonSimulationIntro
            , ""
            , "Full 38-match-week simulation with player availability, in-season performance, and streak form:"
            , renderSeasonSimulation simulation
            ]

loadGuiData :: IO GuiData
loadGuiData = do
    simulation <- loadPremierLeagueSimulation
    let guiWeeks = map buildGuiWeek (simulatedMatchWeeks simulation)
    pure
        GuiData
            { guiOverviewText = renderSimulationOverview simulation
            , guiSampleSeasonText = renderSeason buildSampleSeason
            , guiFinalStandings = maybe [] guiWeekStandings (lastMaybe guiWeeks)
            , guiMatchWeeks = guiWeeks
            }

loadPremierLeagueSimulation :: IO SeasonSimulation
loadPremierLeagueSimulation = do
    (_, historicalFixtures) <- loadPremierLeagueHistory
    leagueTeams <- loadLatestPremierLeagueTeams
    (playerStats, availability) <- loadKeyPlayerData leagueTeams
    pure (simulateSeason leagueTeams historicalFixtures playerStats availability)

buildSampleSeason :: Season
buildSampleSeason = createSeason 2026 teams completedFixtures
  where
    teams = loadSampleTeams
    scheduledFixtures = generateRoundRobinFixtures teams
    completedFixtures =
        applyResults
            [ (1, Result 2 1)
            , (2, Result 0 0)
            , (3, Result 3 2)
            ]
            scheduledFixtures

buildGuiWeek :: MatchWeekSimulation -> GuiWeek
buildGuiWeek matchWeek =
    GuiWeek
        { guiWeekNumber = simulatedMatchWeek matchWeek
        , guiWeekReportText = renderMatchWeek matchWeek
        , guiWeekFixtures = map buildGuiFixture (simulatedFixtures matchWeek)
        , guiWeekStandings = buildGuiStandings (standingsAfterMatchWeek matchWeek)
        }

buildGuiStandings :: [Standing] -> [GuiStanding]
buildGuiStandings standings =
    zipWith buildGuiStanding [1 :: Int ..] standings

buildGuiStanding :: Int -> Standing -> GuiStanding
buildGuiStanding position standing =
    GuiStanding
        { guiStandingPosition = position
        , guiStandingTeamName = teamName team
        , guiStandingTeamShortName = teamShortName team
        , guiStandingPlayed = gamesPlayed standing
        , guiStandingWins = wins standing
        , guiStandingDraws = draws standing
        , guiStandingLosses = losses standing
        , guiStandingGoalsFor = goalsFor standing
        , guiStandingGoalsAgainst = goalsAgainst standing
        , guiStandingGoalDifference = goalDifference standing
        , guiStandingPoints = points standing
        }
  where
    team = standingTeam standing

buildGuiFixture :: SimulatedFixture -> GuiFixture
buildGuiFixture simulated =
    GuiFixture
        { guiFixtureMatchup = teamShortName home ++ " vs " ++ teamShortName away
        , guiFixtureHomeTeamName = teamName home
        , guiFixtureHomeTeamShortName = teamShortName home
        , guiFixtureAwayTeamName = teamName away
        , guiFixtureAwayTeamShortName = teamShortName away
        , guiFixtureHomeWinProbability = homeWinProbability probabilities
        , guiFixtureDrawProbability = drawProbability probabilities
        , guiFixtureAwayWinProbability = awayWinProbability probabilities
        , guiFixtureExpectedHomeGoals = expectedHomeGoals prediction
        , guiFixtureExpectedAwayGoals = expectedAwayGoals prediction
        , guiFixtureResult = renderResultSummary maybeResult
        , guiFixtureSimulatedHomeGoals = maybe 0 resultHomeGoals maybeResult
        , guiFixtureSimulatedAwayGoals = maybe 0 resultAwayGoals maybeResult
        }
  where
    prediction = fixturePrediction simulated
    probabilities = outcomeProbabilities prediction
    match = simulatedFixture simulated
    home = homeTeam match
    away = awayTeam match
    maybeResult = matchResult match

renderResultSummary :: Maybe Result -> String
renderResultSummary Nothing = "-"
renderResultSummary (Just result) =
    show (resultHomeGoals result) ++ "-" ++ show (resultAwayGoals result)

renderSimulationOverview :: SeasonSimulation -> String
renderSimulationOverview simulation =
    unlines
        [ seasonSimulationIntro
        , "Simulated teams: " ++ show (length (simulatedTeams simulation))
        , "Match weeks: " ++ show (length (simulatedMatchWeeks simulation))
        , ""
        , "Final standings:"
        , renderStandings (finalStandings simulation)
        ]

finalStandings :: SeasonSimulation -> [Standing]
finalStandings =
    maybe [] standingsAfterMatchWeek . lastMaybe . simulatedMatchWeeks

lastMaybe :: [a] -> Maybe a
lastMaybe =
    listToMaybe . reverse

seasonSimulationIntro :: String
seasonSimulationIntro =
    "Premier League model trained on 2022-23, 2023-24, and 2024-25 results."
