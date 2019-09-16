## Synopsis


## Code Example

Coming Soon...

## Motivation

The goal of this project is to perform analysis of College Football Play by Play Data mainly geared towards Fantasy Football Analysis. 

## Installation

Provide code examples and explanations of how to get the project.

## API Reference

Coming Soon...

## Code Notes

#### web_scrape_game_ids_2002_2019.R 
	- this code scrapes all college football game ids from ESPN from 2002-2019. 
	- note: occasionally some links are down on ESPN's websites so this code will have 
		to be periodically rerun to compile a full list. 
	- the output is the ../Outputs/all_game_ids_2002_2019.rds and .csv

#### ../Outputs/game_ids_2018.csv is a sample of game ids from 2018 (and a few from 2019)
	- this file is generated from

#### ESPN_API_Stats_by_gameid.ipynb takes a vector of ESPN game id's and produces  the following data sets: 
	- df_teams: A list of teams in each game, including their team id and name
	- df_team_stats: teams stats by game 
	- df_athlete: database of athletes compiled from the box score of each game
	- df_venue: databse of venues frome each game
	- df_drive: drive level stats for each drive in each game
	- df_play:  play-by-play level stats for each play  
	- df_predictor: prediction of winner for each game
	- df_pick: gambling odds for each game
	- df_win_prob: win probability throughout the game
	- df_header: additional game infromation including conference game indicator, week, season, team reords, rank, etc. 


#### d1_ids.rds is a vector of Division Ids
d1_ids.r is the code used to identify these ids

## Source Data

For Pre-pulled college football data:
https://sportsdatastuff.com/cfb_pbpdata

For Self-pulled college football data:
ESPN API



## License

N/A