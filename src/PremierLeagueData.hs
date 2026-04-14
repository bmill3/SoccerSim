module PremierLeagueData
    ( loadPremierLeagueHistory
    , loadPremierLeagueSeasonFiles
    , parsePremierLeagueCsv
    , premierLeagueDataFiles
    ) where

import Data.Char (isAlphaNum, isSpace, toUpper)
import Data.List (find, nub, sort)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Types

data RawMatch = RawMatch
    { rawHomeTeam :: String
    , rawAwayTeam :: String
    , rawHomeGoals :: Int
    , rawAwayGoals :: Int
    }
    deriving (Eq, Show)

premierLeagueDataFiles :: [FilePath]
premierLeagueDataFiles =
    [ "data/epl/epl-2022-2023.csv"
    , "data/epl/epl-2023-2024.csv"
    , "data/epl/epl-2024-2025.csv"
    ]

loadPremierLeagueHistory :: IO ([Team], FixtureList)
loadPremierLeagueHistory =
    loadPremierLeagueSeasonFiles premierLeagueDataFiles

loadPremierLeagueSeasonFiles :: [FilePath] -> IO ([Team], FixtureList)
loadPremierLeagueSeasonFiles filePaths = do
    contents <- mapM readFile filePaths
    let parsedSeasons = map parsePremierLeagueCsv contents
    case sequence parsedSeasons of
        Left message -> fail message
        Right rawMatches -> do
            let allMatches = concat rawMatches
                teams = buildTeams allMatches
                fixtures = buildFixtures teams allMatches
            pure (teams, fixtures)

parsePremierLeagueCsv :: String -> Either String [RawMatch]
parsePremierLeagueCsv contents =
    case lines contents of
        [] -> Right []
        headerLine : dataLines ->
            traverse (parseRow header) numberedRows
          where
            header = map stripBom (parseCsvLine headerLine)
            numberedRows = zip [2 :: Int ..] (filter (not . null) dataLines)

parseRow :: [String] -> (Int, String) -> Either String RawMatch
parseRow header (lineNumber, rowText) =
    case requiredFields of
        Just (homeName, awayName, homeGoalsText, awayGoalsText) ->
            case (readMaybe homeGoalsText, readMaybe awayGoalsText) of
                (Just homeGoals, Just awayGoals) ->
                    Right
                        RawMatch
                            { rawHomeTeam = trim homeName
                            , rawAwayTeam = trim awayName
                            , rawHomeGoals = homeGoals
                            , rawAwayGoals = awayGoals
                            }
                _ -> Left ("Missing or invalid score on CSV line " ++ show lineNumber)
        Nothing -> Left ("Missing required Premier League columns on CSV line " ++ show lineNumber)
  where
    row = parseCsvLine rowText
    value columnName = do
        index <- columnIndex columnName header
        safeIndex index row
    requiredFields =
        (,,,)
            <$> value "HomeTeam"
            <*> value "AwayTeam"
            <*> value "FTHG"
            <*> value "FTAG"

columnIndex :: String -> [String] -> Maybe Int
columnIndex columnName header =
    lookup columnName (zip header [0 ..])

safeIndex :: Int -> [a] -> Maybe a
safeIndex index values
    | index < length values = Just (values !! index)
    | otherwise = Nothing

buildTeams :: [RawMatch] -> [Team]
buildTeams matches =
    zipWith buildTeam [1 ..] teamNames
  where
    teamNames =
        sort (nub (concatMap (\match -> [rawHomeTeam match, rawAwayTeam match]) matches))
    buildTeam teamNumber name =
        Team
            { teamId = teamNumber
            , teamName = name
            , teamShortName = makeShortName name
            }

buildFixtures :: [Team] -> [RawMatch] -> FixtureList
buildFixtures teams matches =
    mapMaybe buildMatch (zip [1 ..] matches)
  where
    buildMatch (matchNumber, rawMatch) = do
        home <- findTeam (rawHomeTeam rawMatch)
        away <- findTeam (rawAwayTeam rawMatch)
        Just
            Match
                { matchId = matchNumber
                , matchDay = matchNumber
                , homeTeam = home
                , awayTeam = away
                , matchStatus = Finished
                , matchResult = Just (Result (rawHomeGoals rawMatch) (rawAwayGoals rawMatch))
                }

    findTeam name =
        find (\team -> teamName team == name) teams

parseCsvLine :: String -> [String]
parseCsvLine =
    reverse . finish . foldl step ([], "", False)
  where
    step (fields, current, inQuotes) character
        | character == '"' = (fields, current, not inQuotes)
        | character == ',' && not inQuotes = (reverse current : fields, "", inQuotes)
        | otherwise = (fields, character : current, inQuotes)

    finish (fields, current, _) =
        reverse current : fields

stripBom :: String -> String
stripBom ('\65279' : rest) = rest
stripBom value = value

trim :: String -> String
trim =
    dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd predicate =
    reverse . dropWhile predicate . reverse

makeShortName :: String -> String
makeShortName name =
    take 3 initialsOrLetters
  where
    cleanWords =
        filter (not . null) (map (filter isAlphaNum) (words name))
    initials =
        map (toUpper . head) cleanWords
    compactName =
        map toUpper (filter isAlphaNum name)
    initialsOrLetters
        | length cleanWords > 1 = initials
        | otherwise = compactName
