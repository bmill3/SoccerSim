# SoccerSim

Premier League season simulator written in Haskell.

## Run

- `cabal build`
- `cabal run` generates and opens a local browser dashboard
- `cabal run -- --cli` prints the original text report in the terminal

## GUI

The GUI includes:

- an overview tab with final simulated standings
- points and goal-difference charts for the final table
- a match-week explorer for all 38 simulated weeks
- team search and match-week filtering
- CSV/TXT export buttons for standings and weekly reports
- the original sample season view

## Dev

- `cabal repl`
- `cabal test`
