# Voobly aoe scraper
This is a simple scraper for pulling in match data from voobly

## Data

This repo also contains data obtained from running this scraper. Releases of data will be versioned to some extent. At the current time the following datasets are available. 

| Version | File | Content | Ladders | Size (unzipped)
| --- | --- | --- | --- | --- |
| 20180504 | [matchDump.zip](https://github.com/bowswung/voobly-scraper/raw/master/data/MatchData/20180504/matchDump.zip) | 648641 matches | RM 1v1, RM Team | 404 MB |

## Data structure
For the sake of simplicity the data are provided in csv format, with one row per player per match, and the following columns

### MatchId
The id of the match on Voobly
### MatchUrl
The url of the match on Voobly
### MatchDate
When the match was played
### MatchDuration
How long the match was, in seconds
### MatchLadder
Which ladder the match was played on
### MatchMap
Which map the match was played on
### MatchMods
Any match mods used, separated by commas
### MatchPlayerId
The id of the player on Voobly
### MatchPlayerName
The (most recently scraped) name of the player - may not be the same as the player name on the match url
### MatchPlayerTeam
Which team the player was on
### MatchPlayerCivId
The civ id of the match - a full list is available below
### MatchPlayerCivName
The civ name
### MatchPlayerWinner
Whether or not this player was a match winner (coded as 1 = Winner, 0 = Not winner)
### MatchPlayerPreRating
The rating of the player on the relevant ladder before the match
### MatchPlayerPostRating
The rating of the player on the relevant ladder after the match

## Civ ids
These are the same as the ids Voobly uses to display civ images. Voobly has some errors here, and uses civ ids for which it doesn't have a picture or any information. We label these civs as "VooblyCivError"

| Id | Name |
| --- | --- |
| 1 | Britons
| 2 | Franks
| 3 | Goths
| 4 | Teutons
| 5 | Japanese
| 6 | Chinese
| 7 | Byzantines
| 8 | Persians
| 9 | Saracens
| 10 | Turks
| 11 | Vikings
| 12 | Mongols
| 13 | Celts
| 14 | Spanish
| 15 | Aztecs
| 16 | Mayans
| 17 | Huns
| 18 | Koreans
| 19 | Italians
| 20 | Indians
| 21 | Incas
| 22 | Magyars
| 23 | Slavs
| 24 | Portuguese
| 25 | Ethiopian
| 26 | Malian
| 27 | Berbers
| 28 | Khmer
| 29 | Malay
| 30 | Burmese
| 31 | Vietnamese
| 32 | VooblyCivError
| 33 | VooblyCivError
| 34 | VooblyCivError
| 36 | VooblyCivError
| 35 | VooblyCivError
| 37 | VooblyCivError
| 38 | VooblyCivError
| 39 | VooblyCivError
| 40 | VooblyCivError
| 41 | VooblyCivError
| 46 | VooblyCivError
| 47 | VooblyCivError