# Premier League CSV Data

These CSV files come from Football-Data.co.uk's public English Premier League
results files:

- `epl-2022-2023.csv`: https://www.football-data.co.uk/mmz4281/2223/E0.csv
- `epl-2023-2024.csv`: https://www.football-data.co.uk/mmz4281/2324/E0.csv
- `epl-2024-2025.csv`: https://www.football-data.co.uk/mmz4281/2425/E0.csv

Each file contains 380 completed Premier League matches. The project currently
uses `HomeTeam`, `AwayTeam`, `FTHG`, and `FTAG` to build fixtures, results,
team attack strength, team defense strength, fixture probabilities, and
projected expected-points standings.

`player-stats-sample.csv` and `player-availability-sample.csv` are small sample
inputs for the player-aware prediction feature. They are intentionally compact
so the model can be tested without needing a paid live player/injury API. Replace
or extend them with fuller player stats, predicted lineups, injuries, and
suspensions when better data is available.
