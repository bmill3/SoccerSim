# Key Player Data Sources

`key-player-stats-2024-2025.csv` contains one real key attacking player for
each club used by the simulator's current club set.

Most rows are based on public StatMuse player stat summaries for the 2024-25
Premier League season. Promoted 2025-26 clubs were updated from public 2024-25
Championship summary sources. The fields used here are the ones our Haskell
model can consume directly: appearances, starts, minutes, goals, assists, shots
on target, and optional rating.

Cross-check / summary sources:

- FBref 2024-25 Premier League page:
  https://fbref.com/en/comps/9/2024-2025/2024-2025-Premier-League-Stats
- FBref 2024-25 Championship page:
  https://fbref.com/en/comps/10/2024-2025/2024-2025-Championship-Stats
- WorldFootball.net 2024-25 Premier League scorers:
  https://www.worldfootball.net/scorer/eng-premier-league-2024-2025/
- WorldFootball.net 2024-25 Championship scorers:
  https://www.worldfootball.net/goalgetter/eng-championship-2024-2025/
- ESPN Southampton 2024-25 scoring stats:
  https://www.espn.com/soccer/team/stats/_/id/376/league/ENG.1/season/2024
- StatMuse player stat summaries for club key players, including Mohamed Salah,
  Alexander Isak, Erling Haaland, Bryan Mbeumo, Cole Palmer, Ollie Watkins,
  Bukayo Saka, Bruno Fernandes, Jean-Philippe Mateta, Antoine Semenyo, Jamie
  Chris Wood, Jarrod Bowen, Matheus Cunha, Joël Piroe, Josh Brownhill, and
  Wilson Isidor.

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
