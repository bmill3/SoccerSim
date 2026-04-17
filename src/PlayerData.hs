module PlayerData
    ( keyPlayerAvailabilityDataFile
    , keyPlayerStatsDataFile
    , loadKeyPlayerData
    , loadSamplePlayerData
    , loadPlayerData
    , playerAvailabilityDataFile
    , playerStatsDataFile
    ) where

import Data.Char (isSpace, toLower)
import Data.List (find)
import Text.Read (readMaybe)
import Types

data RawPlayerStats = RawPlayerStats
    { rawPlayerId :: Int
    , rawPlayerName :: String
    , rawPlayerTeam :: String
    , rawPlayerPosition :: Position
    , rawAppearances :: Int
    , rawStarts :: Int
    , rawMinutes :: Int
    , rawGoals :: Int
    , rawAssists :: Int
    , rawShotsOnTarget :: Int
    , rawTackles :: Int
    , rawInterceptions :: Int
    , rawSaves :: Int
    , rawRating :: Maybe Double
    }
    deriving (Eq, Show)

data RawAvailability = RawAvailability
    { rawAvailabilityPlayerId :: Int
    , rawAvailabilityStatus :: AvailabilityStatus
    , rawExpectedMinutes :: Int
    }
    deriving (Eq, Show)

playerStatsDataFile :: FilePath
playerStatsDataFile =
    keyPlayerStatsDataFile

playerAvailabilityDataFile :: FilePath
playerAvailabilityDataFile =
    keyPlayerAvailabilityDataFile

keyPlayerStatsDataFile :: FilePath
keyPlayerStatsDataFile =
    "data/epl/key-player-stats-2024-2025.csv"

keyPlayerAvailabilityDataFile :: FilePath
keyPlayerAvailabilityDataFile =
    "data/epl/key-player-availability-2024-2025.csv"

loadSamplePlayerData :: [Team] -> IO ([PlayerStats], [PlayerAvailability])
loadSamplePlayerData =
    loadKeyPlayerData

loadKeyPlayerData :: [Team] -> IO ([PlayerStats], [PlayerAvailability])
loadKeyPlayerData =
    loadPlayerData playerStatsDataFile playerAvailabilityDataFile

loadPlayerData :: FilePath -> FilePath -> [Team] -> IO ([PlayerStats], [PlayerAvailability])
loadPlayerData statsPath availabilityPath teams = do
    statsContents <- readFile statsPath
    availabilityContents <- readFile availabilityPath
    case (parsePlayerStatsCsv statsContents, parseAvailabilityCsv availabilityContents) of
        (Left message, _) -> fail message
        (_, Left message) -> fail message
        (Right rawStats, Right rawAvailability) -> do
            let stats = map (buildPlayerStats teams) rawStats
            case sequence stats of
                Nothing -> fail "Player stats reference a team that was not found in the loaded fixture data."
                Just playerStats -> do
                    let availability = map (buildAvailability playerStats) rawAvailability
                    case sequence availability of
                        Nothing -> fail "Player availability references a player that was not found in player stats."
                        Just playerAvailability -> pure (playerStats, playerAvailability)

parsePlayerStatsCsv :: String -> Either String [RawPlayerStats]
parsePlayerStatsCsv contents =
    parseRows contents parseStatsRow

parseAvailabilityCsv :: String -> Either String [RawAvailability]
parseAvailabilityCsv contents =
    parseRows contents parseAvailabilityRow

parseRows :: String -> ([String] -> (Int, String) -> Either String a) -> Either String [a]
parseRows contents rowParser =
    case lines contents of
        [] -> Right []
        headerLine : dataLines ->
            traverse (rowParser header) numberedRows
          where
            header = map stripBom (parseCsvLine headerLine)
            numberedRows = zip [2 :: Int ..] (filter (not . null) dataLines)

parseStatsRow :: [String] -> (Int, String) -> Either String RawPlayerStats
parseStatsRow header (lineNumber, rowText) =
    case requiredFields of
        Just values ->
            buildRawStats lineNumber values
        Nothing ->
            Left ("Missing required player stats columns on CSV line " ++ show lineNumber)
  where
    row = parseCsvLine rowText
    value columnName = do
        index <- columnIndex columnName header
        safeIndex index row
    requiredFields =
        (,,,,,,,,,,,,,)
            <$> value "PlayerId"
            <*> value "PlayerName"
            <*> value "Team"
            <*> value "Position"
            <*> value "Appearances"
            <*> value "Starts"
            <*> value "Minutes"
            <*> value "Goals"
            <*> value "Assists"
            <*> value "ShotsOnTarget"
            <*> value "Tackles"
            <*> value "Interceptions"
            <*> value "Saves"
            <*> value "Rating"

buildRawStats ::
    Int ->
    (String, String, String, String, String, String, String, String, String, String, String, String, String, String) ->
    Either String RawPlayerStats
buildRawStats lineNumber (playerIdText, name, team, positionText, appearancesText, startsText, minutesText, goalsText, assistsText, shotsText, tacklesText, interceptionsText, savesText, ratingText) =
    case parsedFields of
        Just values ->
            Right values
        Nothing ->
            Left ("Missing or invalid player stats value on CSV line " ++ show lineNumber)
  where
    parsedFields =
        RawPlayerStats
            <$> readMaybe playerIdText
            <*> Just (trim name)
            <*> Just (trim team)
            <*> parsePosition positionText
            <*> readMaybe appearancesText
            <*> readMaybe startsText
            <*> readMaybe minutesText
            <*> readMaybe goalsText
            <*> readMaybe assistsText
            <*> readMaybe shotsText
            <*> readMaybe tacklesText
            <*> readMaybe interceptionsText
            <*> readMaybe savesText
            <*> parseOptionalDouble ratingText

parseAvailabilityRow :: [String] -> (Int, String) -> Either String RawAvailability
parseAvailabilityRow header (lineNumber, rowText) =
    case requiredFields of
        Just (playerIdText, statusText, minutesText) ->
            case (readMaybe playerIdText, parseAvailabilityStatus statusText, readMaybe minutesText) of
                (Just playerIdValue, Just statusValue, Just minutesValue) ->
                    Right (RawAvailability playerIdValue statusValue minutesValue)
                _ ->
                    Left ("Missing or invalid player availability value on CSV line " ++ show lineNumber)
        Nothing ->
            Left ("Missing required player availability columns on CSV line " ++ show lineNumber)
  where
    row = parseCsvLine rowText
    value columnName = do
        index <- columnIndex columnName header
        safeIndex index row
    requiredFields =
        (,,)
            <$> value "PlayerId"
            <*> value "Status"
            <*> value "ExpectedMinutes"

buildPlayerStats :: [Team] -> RawPlayerStats -> Maybe PlayerStats
buildPlayerStats teams rawStats = do
    team <- find (\candidate -> teamName candidate == rawPlayerTeam rawStats) teams
    let player =
            Player
                { playerId = rawPlayerId rawStats
                , playerName = rawPlayerName rawStats
                , playerTeam = team
                , playerPosition = rawPlayerPosition rawStats
                }
    Just
        PlayerStats
            { playerStatsPlayer = player
            , playerAppearances = rawAppearances rawStats
            , playerStarts = rawStarts rawStats
            , playerMinutes = rawMinutes rawStats
            , playerGoals = rawGoals rawStats
            , playerAssists = rawAssists rawStats
            , playerShotsOnTarget = rawShotsOnTarget rawStats
            , playerTackles = rawTackles rawStats
            , playerInterceptions = rawInterceptions rawStats
            , playerSaves = rawSaves rawStats
            , playerRating = rawRating rawStats
            }

buildAvailability :: [PlayerStats] -> RawAvailability -> Maybe PlayerAvailability
buildAvailability stats rawAvailability = do
    player <- findPlayer (rawAvailabilityPlayerId rawAvailability)
    Just
        PlayerAvailability
            { availabilityPlayer = player
            , availabilityStatus = rawAvailabilityStatus rawAvailability
            , expectedMinutes = rawExpectedMinutes rawAvailability
            }
  where
    findPlayer playerIdValue =
        playerStatsPlayer
            <$> find
                (\playerStats -> playerId (playerStatsPlayer playerStats) == playerIdValue)
                stats

parsePosition :: String -> Maybe Position
parsePosition value =
    case normalize value of
        "goalkeeper" -> Just Goalkeeper
        "defender" -> Just Defender
        "midfielder" -> Just Midfielder
        "forward" -> Just Forward
        _ -> Nothing

parseAvailabilityStatus :: String -> Maybe AvailabilityStatus
parseAvailabilityStatus value =
    case normalize value of
        "available" -> Just Available
        "starting" -> Just Starting
        "benched" -> Just Benched
        "injured" -> Just Injured
        "suspended" -> Just Suspended
        "notcalledup" -> Just NotCalledUp
        "notcalled" -> Just NotCalledUp
        _ -> Nothing

parseOptionalDouble :: String -> Maybe (Maybe Double)
parseOptionalDouble value
    | null (trim value) = Just Nothing
    | otherwise = Just <$> readMaybe value

columnIndex :: String -> [String] -> Maybe Int
columnIndex columnName header =
    lookup columnName (zip header [0 ..])

safeIndex :: Int -> [a] -> Maybe a
safeIndex index values
    | index < length values = Just (values !! index)
    | otherwise = Nothing

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

normalize :: String -> String
normalize =
    filter (/= ' ') . map toLower . trim
