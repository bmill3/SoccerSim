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

`key-player-stats-2024-2025.csv` contains one real 2024-25 key attacking player
for each Premier League club in the latest completed season file. The app loads
this file by default through `PlayerData`.

`key-player-availability-2024-2025.csv` is an editable scenario file for starts,
bench roles, injuries, suspensions, and not-called-up status. It is not a live
injury feed; by default it assumes all key players start and play 90 minutes.

See `PLAYER_DATA_SOURCES.md` for the player-data source notes.
