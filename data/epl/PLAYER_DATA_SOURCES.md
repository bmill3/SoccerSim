# Key Player Data Sources

`key-player-stats-2024-2025.csv` contains one real 2024-25 Premier League key
attacking player for each club in the 2024-25 season file.

Most rows are based on public StatMuse player stat summaries for the 2024-25
Premier League season. The fields used here are the ones our Haskell model can
consume directly: appearances, starts, minutes, goals, assists, shots on target,
and optional rating.

Cross-check / summary sources:

- FBref 2024-25 Premier League page:
  https://fbref.com/en/comps/9/2024-2025/2024-2025-Premier-League-Stats
- WorldFootball.net 2024-25 Premier League scorers:
  https://www.worldfootball.net/scorer/eng-premier-league-2024-2025/
- ESPN Southampton 2024-25 scoring stats:
  https://www.espn.com/soccer/team/stats/_/id/376/league/ENG.1/season/2024
- StatMuse player stat summaries for club key players, including Mohamed Salah,
  Alexander Isak, Erling Haaland, Bryan Mbeumo, Cole Palmer, Ollie Watkins,
  Bukayo Saka, Bruno Fernandes, Jean-Philippe Mateta, Antoine Semenyo, Jamie
  Vardy, Chris Wood, Paul Onuachu, Jarrod Bowen, and Matheus Cunha.

Notes:

- This is real historical player performance data, not live injury data.
- The model treats this file as a compact "key player" layer, not a full squad
  dataset.
- `key-player-availability-2024-2025.csv` is an editable scenario file. By
  default, all key players are marked `Starting` with 90 expected minutes. Change
  a row to `Injured`, `Suspended`, `Benched`, `Available`, or `NotCalledUp` to
  see that player's absence or reduced role change match probabilities.
- Defensive fields are currently `0` in this compact file because these public
  summaries focused on attacking contribution. The model can already consume
  tackles, interceptions, and saves when a fuller dataset is added.
