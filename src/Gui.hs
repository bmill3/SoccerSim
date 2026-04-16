module Gui
    ( runSoccerSimGui
    ) where

import AppData
    ( GuiData (..)
    , GuiFixture (..)
    , GuiStanding (..)
    , GuiWeek (..)
    , loadGuiData
    )
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (findExecutable, getTemporaryDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Info (os)
import System.Process (callProcess)

runSoccerSimGui :: IO ()
runSoccerSimGui = do
    guiData <- loadGuiData
    htmlPath <- createHtmlPath
    writeFile htmlPath (renderGuiHtml guiData)
    skipOpen <- shouldSkipOpen
    if skipOpen
        then
            putStrLn ("GUI written to " ++ htmlPath)
        else do
            opened <- openHtmlFile htmlPath
            if opened
                then putStrLn ("Opened GUI in your browser: " ++ htmlPath)
                else putStrLn ("GUI written to " ++ htmlPath)

createHtmlPath :: IO FilePath
createHtmlPath = do
    temporaryDirectory <- getTemporaryDirectory
    timestamp <- ((round . (* 1000)) <$> getPOSIXTime) :: IO Integer
    pure (temporaryDirectory </> ("soccer-sim-gui-" ++ show timestamp ++ ".html"))

shouldSkipOpen :: IO Bool
shouldSkipOpen = do
    maybeValue <- lookupEnv "SOCCERSIM_GUI_SKIP_OPEN"
    pure
        ( case fmap (map toLower) maybeValue of
            Just "1" -> True
            Just "true" -> True
            Just "yes" -> True
            _ -> False
        )

openHtmlFile :: FilePath -> IO Bool
openHtmlFile htmlPath =
    case preferredOpenCommand of
        Nothing ->
            pure False
        Just commandName -> do
            executable <- findExecutable commandName
            case executable of
                Just _ -> do
                    callProcess commandName [htmlPath]
                    pure True
                Nothing ->
                    pure False

preferredOpenCommand :: Maybe String
preferredOpenCommand =
    case os of
        "darwin" -> Just "open"
        "linux" -> Just "xdg-open"
        _ -> Nothing

renderGuiHtml :: GuiData -> String
renderGuiHtml guiData =
    unlines
        [ "<!DOCTYPE html>"
        , "<html lang=\"en\">"
        , "<head>"
        , "  <meta charset=\"utf-8\">"
        , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
        , "  <title>SoccerSim Dashboard</title>"
        , "  <style>"
        , renderStyles
        , "  </style>"
        , "</head>"
        , "<body>"
        , "  <main>"
        , "    <h1>SoccerSim Dashboard</h1>"
        , "    <p class=\"subtitle\">Premier League season simulation with charts, filters, exports, and a week-by-week explorer.</p>"
        , "    <div class=\"tab-list\" role=\"tablist\" aria-label=\"SoccerSim views\">"
        , "      <button class=\"tab-button active\" data-tab=\"overview\" type=\"button\">Overview</button>"
        , "      <button class=\"tab-button\" data-tab=\"matchweeks\" type=\"button\">Match Weeks</button>"
        , "      <button class=\"tab-button\" data-tab=\"sample\" type=\"button\">Sample Season</button>"
        , "    </div>"
        , "    <section class=\"panel active\" id=\"overview\">"
        , "      <div class=\"toolbar toolbar-wrap\">"
        , "        <label for=\"overview-search\">Search team</label>"
        , "        <input id=\"overview-search\" type=\"search\" placeholder=\"Liverpool or LIV\">"
        , "        <button class=\"secondary-button\" id=\"overview-clear\" type=\"button\">Clear</button>"
        , "        <div class=\"toolbar-spacer\"></div>"
        , "        <button id=\"export-standings-csv\" type=\"button\">Export standings CSV</button>"
        , "        <button id=\"export-overview-txt\" type=\"button\">Export overview TXT</button>"
        , "      </div>"
        , "      <div class=\"summary-line\" id=\"overview-summary\"></div>"
        , "      <div class=\"chart-grid\">"
        , "        <section class=\"chart-card\">"
        , "          <h2>Points chart</h2>"
        , "          <div id=\"points-chart\"></div>"
        , "        </section>"
        , "        <section class=\"chart-card\">"
        , "          <h2>Goal difference chart</h2>"
        , "          <div id=\"goal-difference-chart\"></div>"
        , "        </section>"
        , "      </div>"
        , "      <section class=\"table-card\">"
        , "        <h2>Final standings</h2>"
        , "        <div class=\"table-wrapper\">"
        , "          <table>"
        , "            <thead>"
        , "              <tr><th>Pos</th><th>Team</th><th>P</th><th>W</th><th>D</th><th>L</th><th>GF</th><th>GA</th><th>GD</th><th>Pts</th></tr>"
        , "            </thead>"
        , "            <tbody id=\"overview-standings-body\"></tbody>"
        , "          </table>"
        , "        </div>"
        , "      </section>"
        , "    </section>"
        , "    <section class=\"panel\" id=\"matchweeks\">"
        , "      <div class=\"toolbar toolbar-wrap\">"
        , "        <label for=\"match-week-select\">Match week</label>"
        , "        <select id=\"match-week-select\"></select>"
        , "        <label for=\"match-week-filter\">Team filter</label>"
        , "        <input id=\"match-week-filter\" type=\"search\" placeholder=\"Chelsea or CHE\">"
        , "        <button class=\"secondary-button\" id=\"match-week-clear\" type=\"button\">Clear</button>"
        , "        <div class=\"toolbar-spacer\"></div>"
        , "        <button id=\"export-week-fixtures-csv\" type=\"button\">Export fixtures CSV</button>"
        , "        <button id=\"export-week-standings-csv\" type=\"button\">Export standings CSV</button>"
        , "        <button id=\"export-week-txt\" type=\"button\">Export week TXT</button>"
        , "      </div>"
        , "      <div class=\"summary-line\" id=\"match-week-summary\"></div>"
        , "      <div class=\"table-grid\">"
        , "        <section class=\"table-card\">"
        , "          <h2>Fixtures</h2>"
        , "          <div class=\"table-wrapper\">"
        , "            <table>"
        , "              <thead>"
        , "                <tr><th>Fixture</th><th>Home</th><th>Draw</th><th>Away</th><th>xG</th><th>Sim</th></tr>"
        , "              </thead>"
        , "              <tbody id=\"match-week-fixtures-body\"></tbody>"
        , "            </table>"
        , "          </div>"
        , "        </section>"
        , "        <section class=\"table-card\">"
        , "          <h2>Standings after week</h2>"
        , "          <div class=\"table-wrapper\">"
        , "            <table>"
        , "              <thead>"
        , "                <tr><th>Pos</th><th>Team</th><th>P</th><th>W</th><th>D</th><th>L</th><th>GF</th><th>GA</th><th>GD</th><th>Pts</th></tr>"
        , "              </thead>"
        , "              <tbody id=\"match-week-standings-body\"></tbody>"
        , "            </table>"
        , "          </div>"
        , "        </section>"
        , "      </div>"
        , "      <details class=\"details-panel\">"
        , "        <summary>Raw report</summary>"
        , "        <pre id=\"match-week-report\"></pre>"
        , "      </details>"
        , "    </section>"
        , "    <section class=\"panel\" id=\"sample\">"
        , "      <div class=\"toolbar toolbar-wrap\">"
        , "        <div class=\"toolbar-spacer\"></div>"
        , "        <button id=\"export-sample-txt\" type=\"button\">Export sample TXT</button>"
        , "      </div>"
        , "      <pre id=\"sample-season-report\"></pre>"
        , "    </section>"
        , "    <p class=\"footer\">The dashboard is generated locally from the Haskell simulator. Re-run the app to regenerate it from the latest project data.</p>"
        , "  </main>"
        , "  <script>"
        , renderClientScript guiData
        , "  </script>"
        , "</body>"
        , "</html>"
        ]

renderStyles :: String
renderStyles =
    unlines
        [ "    :root {"
        , "      color-scheme: dark;"
        , "      --bg: #0f172a;"
        , "      --panel: #111827;"
        , "      --panel-alt: #1f2937;"
        , "      --accent: #38bdf8;"
        , "      --accent-soft: #164e63;"
        , "      --text: #e5e7eb;"
        , "      --muted: #94a3b8;"
        , "      --border: #334155;"
        , "      --success: #22c55e;"
        , "      --danger: #f97316;"
        , "    }"
        , "    * { box-sizing: border-box; }"
        , "    body {"
        , "      margin: 0;"
        , "      font-family: Inter, system-ui, -apple-system, BlinkMacSystemFont, sans-serif;"
        , "      background: linear-gradient(160deg, #020617, var(--bg));"
        , "      color: var(--text);"
        , "      min-height: 100vh;"
        , "    }"
        , "    main {"
        , "      max-width: 1320px;"
        , "      margin: 0 auto;"
        , "      padding: 32px 24px 40px;"
        , "    }"
        , "    h1 { margin: 0 0 8px; font-size: 2rem; }"
        , "    h2 { margin: 0 0 12px; font-size: 1.05rem; }"
        , "    .subtitle { margin: 0 0 24px; color: var(--muted); }"
        , "    .tab-list { display: flex; flex-wrap: wrap; gap: 12px; margin-bottom: 20px; }"
        , "    .tab-button, button {"
        , "      border: 1px solid var(--border);"
        , "      background: var(--panel);"
        , "      color: var(--text);"
        , "      padding: 10px 16px;"
        , "      border-radius: 999px;"
        , "      cursor: pointer;"
        , "      font: inherit;"
        , "    }"
        , "    button.secondary-button { background: transparent; }"
        , "    .tab-button.active {"
        , "      background: var(--accent-soft);"
        , "      border-color: var(--accent);"
        , "      color: #e0f2fe;"
        , "    }"
        , "    .panel {"
        , "      display: none;"
        , "      background: rgba(17, 24, 39, 0.92);"
        , "      border: 1px solid var(--border);"
        , "      border-radius: 18px;"
        , "      padding: 18px;"
        , "      box-shadow: 0 18px 48px rgba(2, 6, 23, 0.35);"
        , "    }"
        , "    .panel.active { display: block; }"
        , "    .toolbar { display: flex; align-items: center; gap: 12px; margin-bottom: 16px; }"
        , "    .toolbar-wrap { flex-wrap: wrap; }"
        , "    .toolbar-spacer { flex: 1 1 auto; }"
        , "    label { color: var(--muted); }"
        , "    input, select {"
        , "      background: var(--panel-alt);"
        , "      color: var(--text);"
        , "      border: 1px solid var(--border);"
        , "      border-radius: 10px;"
        , "      padding: 8px 12px;"
        , "      font: inherit;"
        , "    }"
        , "    input { min-width: 220px; }"
        , "    pre {"
        , "      margin: 0;"
        , "      padding: 18px;"
        , "      background: rgba(15, 23, 42, 0.9);"
        , "      border: 1px solid var(--border);"
        , "      border-radius: 14px;"
        , "      color: var(--text);"
        , "      font: 13px/1.45 SFMono-Regular, Menlo, Consolas, Monaco, monospace;"
        , "      overflow: auto;"
        , "      white-space: pre;"
        , "      min-height: 420px;"
        , "    }"
        , "    .summary-line { color: var(--muted); margin-bottom: 16px; }"
        , "    .chart-grid, .table-grid {"
        , "      display: grid;"
        , "      grid-template-columns: repeat(auto-fit, minmax(360px, 1fr));"
        , "      gap: 16px;"
        , "      margin-bottom: 16px;"
        , "    }"
        , "    .chart-card, .table-card {"
        , "      background: rgba(15, 23, 42, 0.72);"
        , "      border: 1px solid var(--border);"
        , "      border-radius: 16px;"
        , "      padding: 16px;"
        , "    }"
        , "    .table-wrapper { overflow: auto; }"
        , "    table { width: 100%; border-collapse: collapse; }"
        , "    th, td { padding: 10px 12px; border-bottom: 1px solid rgba(51, 65, 85, 0.9); text-align: left; white-space: nowrap; }"
        , "    th { color: var(--muted); font-weight: 600; }"
        , "    tbody tr:hover { background: rgba(30, 41, 59, 0.55); }"
        , "    .team-name { font-weight: 600; }"
        , "    .team-code { color: var(--muted); font-size: 0.92em; }"
        , "    .chart-list { display: flex; flex-direction: column; gap: 10px; }"
        , "    .chart-row { display: grid; grid-template-columns: 58px minmax(0, 1fr) 62px; gap: 12px; align-items: center; }"
        , "    .chart-label { color: var(--text); font-weight: 600; }"
        , "    .chart-value { color: var(--muted); text-align: right; }"
        , "    .chart-track { position: relative; height: 16px; border-radius: 999px; background: rgba(51, 65, 85, 0.85); overflow: hidden; }"
        , "    .chart-track.diverging::before { content: ''; position: absolute; left: 50%; top: 0; bottom: 0; width: 1px; background: rgba(148, 163, 184, 0.45); }"
        , "    .chart-fill { position: absolute; top: 0; bottom: 0; left: 0; border-radius: 999px; background: linear-gradient(90deg, var(--accent), #60a5fa); }"
        , "    .chart-fill.positive { background: linear-gradient(90deg, var(--accent), var(--success)); }"
        , "    .chart-fill.negative { background: linear-gradient(90deg, #fb7185, var(--danger)); }"
        , "    .empty-state { color: var(--muted); padding: 16px; text-align: center; }"
        , "    .details-panel { margin-top: 16px; }"
        , "    .details-panel summary { cursor: pointer; color: var(--muted); margin-bottom: 12px; }"
        , "    .details-panel pre { min-height: 240px; }"
        , "    .footer { margin-top: 16px; color: var(--muted); font-size: 0.95rem; }"
        , "    @media (max-width: 780px) {"
        , "      main { padding: 24px 16px 32px; }"
        , "      input { min-width: 0; width: 100%; }"
        , "      .chart-row { grid-template-columns: 48px minmax(0, 1fr) 52px; gap: 8px; }"
        , "    }"
        ]

renderClientScript :: GuiData -> String
renderClientScript guiData =
    unlines
        [ renderGuiDataScript guiData
        , "const elements = {"
        , "  sampleSeasonReport: document.getElementById('sample-season-report'),"
        , "  overviewSearch: document.getElementById('overview-search'),"
        , "  overviewClear: document.getElementById('overview-clear'),"
        , "  overviewSummary: document.getElementById('overview-summary'),"
        , "  overviewStandingsBody: document.getElementById('overview-standings-body'),"
        , "  pointsChart: document.getElementById('points-chart'),"
        , "  goalDifferenceChart: document.getElementById('goal-difference-chart'),"
        , "  matchWeekSelect: document.getElementById('match-week-select'),"
        , "  matchWeekFilter: document.getElementById('match-week-filter'),"
        , "  matchWeekClear: document.getElementById('match-week-clear'),"
        , "  matchWeekSummary: document.getElementById('match-week-summary'),"
        , "  matchWeekFixturesBody: document.getElementById('match-week-fixtures-body'),"
        , "  matchWeekStandingsBody: document.getElementById('match-week-standings-body'),"
        , "  matchWeekReport: document.getElementById('match-week-report'),"
        , "  exportStandingsCsv: document.getElementById('export-standings-csv'),"
        , "  exportOverviewTxt: document.getElementById('export-overview-txt'),"
        , "  exportWeekFixturesCsv: document.getElementById('export-week-fixtures-csv'),"
        , "  exportWeekStandingsCsv: document.getElementById('export-week-standings-csv'),"
        , "  exportWeekTxt: document.getElementById('export-week-txt'),"
        , "  exportSampleTxt: document.getElementById('export-sample-txt')"
        , "};"
        , ""
        , "elements.sampleSeasonReport.textContent = sampleSeasonText;"
        , ""
        , "const normalize = (value) => value.trim().toLowerCase();"
        , "const escapeHtml = (value) => value"
        , "  .replace(/&/g, '&amp;')"
        , "  .replace(/</g, '&lt;')"
        , "  .replace(/>/g, '&gt;')"
        , "  .replace(/\"/g, '&quot;')"
        , "  .replace(/'/g, '&#39;');"
        , "const slugify = (value) => value.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '') || 'all';"
        , "const formatPercent = (value) => `${(value * 100).toFixed(1)}%`;"
        , "const formatExpectedGoals = (fixture) => `${fixture.expectedHomeGoals.toFixed(1)}-${fixture.expectedAwayGoals.toFixed(1)}`;"
        , "const formatTeamCell = (row) => `<span class=\"team-name\">${escapeHtml(row.teamName)}</span> <span class=\"team-code\">(${escapeHtml(row.teamShortName)})</span>`;"
        , "const teamMatches = (query, teamName, teamShortName) => !query || teamName.toLowerCase().includes(query) || teamShortName.toLowerCase().includes(query);"
        , "const fixtureMatches = (query, fixture) => !query || [fixture.homeTeamName, fixture.homeTeamShortName, fixture.awayTeamName, fixture.awayTeamShortName, fixture.matchup, fixture.result].some((value) => value.toLowerCase().includes(query));"
        , "const filterStandings = (rows, query) => rows.filter((row) => teamMatches(query, row.teamName, row.teamShortName));"
        , "const filterFixtures = (fixtures, query) => fixtures.filter((fixture) => fixtureMatches(query, fixture));"
        , ""
        , "function downloadFile(filename, content, mimeType) {"
        , "  const blob = new Blob([content], { type: mimeType });"
        , "  const url = URL.createObjectURL(blob);"
        , "  const link = document.createElement('a');"
        , "  link.href = url;"
        , "  link.download = filename;"
        , "  document.body.appendChild(link);"
        , "  link.click();"
        , "  link.remove();"
        , "  URL.revokeObjectURL(url);"
        , "}"
        , ""
        , "function csvEscape(value) {"
        , "  const text = String(value);"
        , "  return /[\",\\n]/.test(text) ? `\"${text.replace(/\"/g, '\"\"')}\"` : text;"
        , "}"
        , ""
        , "function buildStandingsCsv(rows) {"
        , "  const header = ['Position', 'Team', 'Code', 'Played', 'Wins', 'Draws', 'Losses', 'Goals For', 'Goals Against', 'Goal Difference', 'Points'];"
        , "  const lines = rows.map((row) => ["
        , "    row.position, row.teamName, row.teamShortName, row.played, row.wins, row.draws, row.losses, row.goalsFor, row.goalsAgainst, row.goalDifference, row.points"
        , "  ].map(csvEscape).join(','));"
        , "  return [header.join(','), ...lines].join('\\n');"
        , "}"
        , ""
        , "function buildFixturesCsv(weekNumber, fixtures) {"
        , "  const header = ['Match Week', 'Home Team', 'Home Code', 'Away Team', 'Away Code', 'Home Win %', 'Draw %', 'Away Win %', 'Expected Home Goals', 'Expected Away Goals', 'Simulated Home Goals', 'Simulated Away Goals', 'Result'];"
        , "  const lines = fixtures.map((fixture) => ["
        , "    weekNumber,"
        , "    fixture.homeTeamName,"
        , "    fixture.homeTeamShortName,"
        , "    fixture.awayTeamName,"
        , "    fixture.awayTeamShortName,"
        , "    formatPercent(fixture.homeWinProbability),"
        , "    formatPercent(fixture.drawProbability),"
        , "    formatPercent(fixture.awayWinProbability),"
        , "    fixture.expectedHomeGoals.toFixed(1),"
        , "    fixture.expectedAwayGoals.toFixed(1),"
        , "    fixture.simulatedHomeGoals,"
        , "    fixture.simulatedAwayGoals,"
        , "    fixture.result"
        , "  ].map(csvEscape).join(','));"
        , "  return [header.join(','), ...lines].join('\\n');"
        , "}"
        , ""
        , "function renderEmptyState(colspan, message) {"
        , "  return `<tr><td colspan=\"${colspan}\" class=\"empty-state\">${escapeHtml(message)}</td></tr>`;"
        , "}"
        , ""
        , "function renderStandingsRows(rows) {"
        , "  if (!rows.length) {"
        , "    return renderEmptyState(10, 'No teams match the current filter.');"
        , "  }"
        , "  return rows.map((row) => `"
        , "    <tr>"
        , "      <td>${row.position}</td>"
        , "      <td>${formatTeamCell(row)}</td>"
        , "      <td>${row.played}</td>"
        , "      <td>${row.wins}</td>"
        , "      <td>${row.draws}</td>"
        , "      <td>${row.losses}</td>"
        , "      <td>${row.goalsFor}</td>"
        , "      <td>${row.goalsAgainst}</td>"
        , "      <td>${row.goalDifference}</td>"
        , "      <td>${row.points}</td>"
        , "    </tr>`).join('');"
        , "}"
        , ""
        , "function renderFixtureRows(fixtures) {"
        , "  if (!fixtures.length) {"
        , "    return renderEmptyState(6, 'No fixtures match the current filter.');"
        , "  }"
        , "  return fixtures.map((fixture) => `"
        , "    <tr>"
        , "      <td>${escapeHtml(fixture.matchup)}</td>"
        , "      <td>${formatPercent(fixture.homeWinProbability)}</td>"
        , "      <td>${formatPercent(fixture.drawProbability)}</td>"
        , "      <td>${formatPercent(fixture.awayWinProbability)}</td>"
        , "      <td>${formatExpectedGoals(fixture)}</td>"
        , "      <td>${escapeHtml(fixture.result)}</td>"
        , "    </tr>`).join('');"
        , "}"
        , ""
        , "function renderPositiveChart(container, rows, key, suffix) {"
        , "  if (!rows.length) {"
        , "    container.innerHTML = '<div class=\"empty-state\">No teams match the current search.</div>';"
        , "    return;"
        , "  }"
        , "  const maxValue = Math.max(...rows.map((row) => row[key]), 1);"
        , "  container.innerHTML = `<div class=\"chart-list\">${rows.map((row) => {"
        , "    const width = (row[key] / maxValue) * 100;"
        , "    return `"
        , "      <div class=\"chart-row\">"
        , "        <div class=\"chart-label\">${escapeHtml(row.teamShortName)}</div>"
        , "        <div class=\"chart-track\"><div class=\"chart-fill\" style=\"width: ${width}%;\"></div></div>"
        , "        <div class=\"chart-value\">${row[key]} ${suffix}</div>"
        , "      </div>`;"
        , "  }).join('')}</div>`;"
        , "}"
        , ""
        , "function renderDivergingChart(container, rows, key) {"
        , "  if (!rows.length) {"
        , "    container.innerHTML = '<div class=\"empty-state\">No teams match the current search.</div>';"
        , "    return;"
        , "  }"
        , "  const maxAbs = Math.max(...rows.map((row) => Math.abs(row[key])), 1);"
        , "  container.innerHTML = `<div class=\"chart-list\">${rows.map((row) => {"
        , "    const value = row[key];"
        , "    const width = (Math.abs(value) / maxAbs) * 50;"
        , "    const left = value >= 0 ? 50 : 50 - width;"
        , "    const directionClass = value >= 0 ? 'positive' : 'negative';"
        , "    return `"
        , "      <div class=\"chart-row\">"
        , "        <div class=\"chart-label\">${escapeHtml(row.teamShortName)}</div>"
        , "        <div class=\"chart-track diverging\"><div class=\"chart-fill ${directionClass}\" style=\"left: ${left}%; width: ${width}%;\"></div></div>"
        , "        <div class=\"chart-value\">${value > 0 ? '+' : ''}${value}</div>"
        , "      </div>`;"
        , "  }).join('')}</div>`;"
        , "}"
        , ""
        , "function renderOverview() {"
        , "  const query = normalize(elements.overviewSearch.value);"
        , "  const filteredStandings = filterStandings(finalStandings, query);"
        , "  elements.overviewStandingsBody.innerHTML = renderStandingsRows(filteredStandings);"
        , "  renderPositiveChart(elements.pointsChart, filteredStandings, 'points', 'pts');"
        , "  renderDivergingChart(elements.goalDifferenceChart, filteredStandings, 'goalDifference');"
        , "  elements.overviewSummary.textContent = query"
        , "    ? `Showing ${filteredStandings.length} of ${finalStandings.length} teams matching \"${elements.overviewSearch.value.trim()}\".`"
        , "    : `Showing all ${finalStandings.length} teams.`;"
        , "}"
        , ""
        , "function getSelectedWeek() {"
        , "  return matchWeeks.find((week) => String(week.number) === elements.matchWeekSelect.value) || matchWeeks[0] || null;"
        , "}"
        , ""
        , "function buildStandingTextRow(row) {"
        , "  return [row.position, row.teamShortName, row.played, row.wins, row.draws, row.losses, row.goalsFor, row.goalsAgainst, row.goalDifference, row.points].join(' | ');"
        , "}"
        , ""
        , "function buildFixtureTextRow(fixture) {"
        , "  return [fixture.matchup, formatPercent(fixture.homeWinProbability), formatPercent(fixture.drawProbability), formatPercent(fixture.awayWinProbability), formatExpectedGoals(fixture), fixture.result].join(' | ');"
        , "}"
        , ""
        , "function buildWeekReportText(week, query) {"
        , "  const fixtures = filterFixtures(week.fixtures, query);"
        , "  const standings = filterStandings(week.standings, query);"
        , "  const lines = ["
        , "    `Match Week ${week.number}`,"
        , "    '',"
        , "    'Fixture | Home | Draw | Away | xG | Sim',"
        , "    '---------------------------------------'"
        , "  ];"
        , "  if (fixtures.length) {"
        , "    fixtures.forEach((fixture) => lines.push(buildFixtureTextRow(fixture)));"
        , "  } else {"
        , "    lines.push('No fixtures match the current filter.');"
        , "  }"
        , "  lines.push('', `Standings after Match Week ${week.number}:`, 'Pos | Team | P | W | D | L | GF | GA | GD | Pts', '-----------------------------------------------');"
        , "  if (standings.length) {"
        , "    standings.forEach((row) => lines.push(buildStandingTextRow(row)));"
        , "  } else {"
        , "    lines.push('No standings rows match the current filter.');"
        , "  }"
        , "  return lines.join('\\n');"
        , "}"
        , ""
        , "function renderMatchWeek() {"
        , "  const week = getSelectedWeek();"
        , "  const query = normalize(elements.matchWeekFilter.value);"
        , "  if (!week) {"
        , "    elements.matchWeekFixturesBody.innerHTML = renderEmptyState(6, 'No match-week data is available.');"
        , "    elements.matchWeekStandingsBody.innerHTML = renderEmptyState(10, 'No standings data is available.');"
        , "    elements.matchWeekReport.textContent = '';"
        , "    elements.matchWeekSummary.textContent = 'No match-week data is available.';"
        , "    return;"
        , "  }"
        , "  const filteredFixtures = filterFixtures(week.fixtures, query);"
        , "  const filteredStandings = filterStandings(week.standings, query);"
        , "  elements.matchWeekFixturesBody.innerHTML = renderFixtureRows(filteredFixtures);"
        , "  elements.matchWeekStandingsBody.innerHTML = renderStandingsRows(filteredStandings);"
        , "  elements.matchWeekReport.textContent = buildWeekReportText(week, query);"
        , "  elements.matchWeekSummary.textContent = query"
        , "    ? `Showing ${filteredFixtures.length} of ${week.fixtures.length} fixtures and ${filteredStandings.length} of ${week.standings.length} standings rows matching \"${elements.matchWeekFilter.value.trim()}\".`"
        , "    : `Showing all ${week.fixtures.length} fixtures and ${week.standings.length} standings rows for match week ${week.number}.`;"
        , "}"
        , ""
        , "function activateTab(target) {"
        , "  document.querySelectorAll('.tab-button').forEach((button) => {"
        , "    button.classList.toggle('active', button.dataset.tab === target);"
        , "  });"
        , "  document.querySelectorAll('.panel').forEach((panel) => {"
        , "    panel.classList.toggle('active', panel.id === target);"
        , "  });"
        , "}"
        , ""
        , "matchWeeks.forEach((week) => {"
        , "  const option = document.createElement('option');"
        , "  option.value = String(week.number);"
        , "  option.textContent = `Match Week ${week.number}`;"
        , "  elements.matchWeekSelect.appendChild(option);"
        , "});"
        , "if (matchWeeks.length) {"
        , "  elements.matchWeekSelect.value = String(matchWeeks[0].number);"
        , "}"
        , ""
        , "document.querySelectorAll('.tab-button').forEach((button) => {"
        , "  button.addEventListener('click', () => activateTab(button.dataset.tab));"
        , "});"
        , "elements.overviewSearch.addEventListener('input', renderOverview);"
        , "elements.overviewClear.addEventListener('click', () => {"
        , "  elements.overviewSearch.value = '';"
        , "  renderOverview();"
        , "});"
        , "elements.matchWeekSelect.addEventListener('change', renderMatchWeek);"
        , "elements.matchWeekFilter.addEventListener('input', renderMatchWeek);"
        , "elements.matchWeekClear.addEventListener('click', () => {"
        , "  elements.matchWeekFilter.value = '';"
        , "  renderMatchWeek();"
        , "});"
        , ""
        , "elements.exportStandingsCsv.addEventListener('click', () => {"
        , "  const query = normalize(elements.overviewSearch.value);"
        , "  const rows = filterStandings(finalStandings, query);"
        , "  downloadFile(`final-standings-${slugify(query || 'all')}.csv`, buildStandingsCsv(rows), 'text/csv;charset=utf-8');"
        , "});"
        , "elements.exportOverviewTxt.addEventListener('click', () => {"
        , "  downloadFile('overview-report.txt', overviewText, 'text/plain;charset=utf-8');"
        , "});"
        , "elements.exportWeekFixturesCsv.addEventListener('click', () => {"
        , "  const week = getSelectedWeek();"
        , "  if (!week) return;"
        , "  const query = normalize(elements.matchWeekFilter.value);"
        , "  const fixtures = filterFixtures(week.fixtures, query);"
        , "  downloadFile(`match-week-${week.number}-fixtures-${slugify(query || 'all')}.csv`, buildFixturesCsv(week.number, fixtures), 'text/csv;charset=utf-8');"
        , "});"
        , "elements.exportWeekStandingsCsv.addEventListener('click', () => {"
        , "  const week = getSelectedWeek();"
        , "  if (!week) return;"
        , "  const query = normalize(elements.matchWeekFilter.value);"
        , "  const standings = filterStandings(week.standings, query);"
        , "  downloadFile(`match-week-${week.number}-standings-${slugify(query || 'all')}.csv`, buildStandingsCsv(standings), 'text/csv;charset=utf-8');"
        , "});"
        , "elements.exportWeekTxt.addEventListener('click', () => {"
        , "  const week = getSelectedWeek();"
        , "  if (!week) return;"
        , "  const query = normalize(elements.matchWeekFilter.value);"
        , "  downloadFile(`match-week-${week.number}-report-${slugify(query || 'all')}.txt`, buildWeekReportText(week, query), 'text/plain;charset=utf-8');"
        , "});"
        , "elements.exportSampleTxt.addEventListener('click', () => {"
        , "  downloadFile('sample-season.txt', sampleSeasonText, 'text/plain;charset=utf-8');"
        , "});"
        , ""
        , "renderOverview();"
        , "renderMatchWeek();"
        ]

renderGuiDataScript :: GuiData -> String
renderGuiDataScript guiData =
    unlines
        [ "const overviewText = " ++ jsString (guiOverviewText guiData) ++ ";"
        , "const sampleSeasonText = " ++ jsString (guiSampleSeasonText guiData) ++ ";"
        , "const finalStandings = " ++ renderStandingsArray (guiFinalStandings guiData) ++ ";"
        , "const matchWeeks = " ++ renderWeeksArray (guiMatchWeeks guiData) ++ ";"
        ]

renderStandingsArray :: [GuiStanding] -> String
renderStandingsArray standings =
    "[" ++ intercalate "," (map renderStandingObject standings) ++ "]"

renderStandingObject :: GuiStanding -> String
renderStandingObject standing =
    "{" ++ intercalate ","
        [ "position:" ++ show (guiStandingPosition standing)
        , "teamName:" ++ jsString (guiStandingTeamName standing)
        , "teamShortName:" ++ jsString (guiStandingTeamShortName standing)
        , "played:" ++ show (guiStandingPlayed standing)
        , "wins:" ++ show (guiStandingWins standing)
        , "draws:" ++ show (guiStandingDraws standing)
        , "losses:" ++ show (guiStandingLosses standing)
        , "goalsFor:" ++ show (guiStandingGoalsFor standing)
        , "goalsAgainst:" ++ show (guiStandingGoalsAgainst standing)
        , "goalDifference:" ++ show (guiStandingGoalDifference standing)
        , "points:" ++ show (guiStandingPoints standing)
        ] ++ "}"

renderWeeksArray :: [GuiWeek] -> String
renderWeeksArray weeks =
    "[" ++ intercalate "," (map renderWeekObject weeks) ++ "]"

renderWeekObject :: GuiWeek -> String
renderWeekObject week =
    "{" ++ intercalate ","
        [ "number:" ++ show (guiWeekNumber week)
        , "reportText:" ++ jsString (guiWeekReportText week)
        , "fixtures:" ++ renderFixturesArray (guiWeekFixtures week)
        , "standings:" ++ renderStandingsArray (guiWeekStandings week)
        ] ++ "}"

renderFixturesArray :: [GuiFixture] -> String
renderFixturesArray fixtures =
    "[" ++ intercalate "," (map renderFixtureObject fixtures) ++ "]"

renderFixtureObject :: GuiFixture -> String
renderFixtureObject fixture =
    "{" ++ intercalate ","
        [ "matchup:" ++ jsString (guiFixtureMatchup fixture)
        , "homeTeamName:" ++ jsString (guiFixtureHomeTeamName fixture)
        , "homeTeamShortName:" ++ jsString (guiFixtureHomeTeamShortName fixture)
        , "awayTeamName:" ++ jsString (guiFixtureAwayTeamName fixture)
        , "awayTeamShortName:" ++ jsString (guiFixtureAwayTeamShortName fixture)
        , "homeWinProbability:" ++ show (guiFixtureHomeWinProbability fixture)
        , "drawProbability:" ++ show (guiFixtureDrawProbability fixture)
        , "awayWinProbability:" ++ show (guiFixtureAwayWinProbability fixture)
        , "expectedHomeGoals:" ++ show (guiFixtureExpectedHomeGoals fixture)
        , "expectedAwayGoals:" ++ show (guiFixtureExpectedAwayGoals fixture)
        , "result:" ++ jsString (guiFixtureResult fixture)
        , "simulatedHomeGoals:" ++ show (guiFixtureSimulatedHomeGoals fixture)
        , "simulatedAwayGoals:" ++ show (guiFixtureSimulatedAwayGoals fixture)
        ] ++ "}"

jsString :: String -> String
jsString value =
    "\"" ++ concatMap escapeJsCharacter value ++ "\""

escapeJsCharacter :: Char -> String
escapeJsCharacter character =
    case character of
        '\\' -> "\\\\"
        '"' -> "\\\""
        '\n' -> "\\n"
        '\r' -> "\\r"
        '\t' -> "\\t"
        '<' -> "\\u003c"
        '>' -> "\\u003e"
        '&' -> "\\u0026"
        _ -> [character]
