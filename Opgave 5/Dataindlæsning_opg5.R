#### opgave 5 - dataindlæsning ####

#### Pakker ####
library(tidyverse)
library(mongolite)
library(jsonlite)
library(tidyr)


#### Matches Connection ####
conm <- mongo(
  url = "mongodb://localhost",
  db = "fsoccer",
  collection = "matches"
)

#### Female Events Connection ####
confe <- mongo(
  url = "mongodb://localhost",
  db = "fsoccer",
  collection = "events"
)

#### Male Events Connection ####
conme <- mongo(
  url = "mongodb://localhost",
  db = "fsoccer",
  collection = "maleevents"
)

#### Data Retrieval ####
## Matches
matches <- conm$find("{}","{}")
matches <- fromJSON(toJSON(matches), flatten = T)

f_matches <- matches %>% 
  filter(competition.competition_name == "Women's World Cup")

m_matches <- matches %>% 
  filter(competition.competition_name == "FIFA World Cup",
         season.season_name >= 2018)

f_id <- f_matches[1:100,2]
m_id <- m_matches[1:100,2]

f_events_raw <- confe$find(toJSON(list(match_id = list("$in" = f_id)), auto_unbox = TRUE))
f_events <- fromJSON(toJSON(f_events_raw), flatten = TRUE)

m_events_raw <- conme$find(toJSON(list(matchId = list("$in" = m_id)), auto_unbox = TRUE))
m_events <- fromJSON(toJSON(m_events_raw), flatten = TRUE)
