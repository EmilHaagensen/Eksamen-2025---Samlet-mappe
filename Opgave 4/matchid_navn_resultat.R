library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggsoccer)
library(RMariaDB)

con = dbConnect(MariaDB(),
                user = "root",
                dbname = "Fodbolddata",
                host = "localhost",
                port = 3306,
                password = "duddi100"
                
)

matchbase = dbReadTable(con ,"_matchdetail_base_sl")
teams = dbReadTable(con, "_teams_sl")
passes <- dbReadTable(con, "PASSES")
wyscout_players_sl = dbReadTable(con, "_players_sl")

dbDisconnect(con)
player_info <- wyscout_players_sl %>%
  distinct(PLAYER_WYID, .keep_all = TRUE) %>%
  select(PLAYER_WYID, FIRSTNAME, LASTNAME, HEIGHT, BIRTHDATE, ROLECODE2, FOOT, CURRENTTEAM_WYID)

# Merge dataframes, hvor vi bevarer alle rækker i passes
passes <- left_join(passes, player_info, by = "PLAYER_WYID")

#kun afleveringer
passes <- passes %>% filter(PRIMARYTYPE == "pass")

#samlet navn
passes <- passes %>%
  mutate(FULLNAME = paste(FIRSTNAME, LASTNAME, sep = " "))


teams_clean <- teams %>%
  select(TEAM_WYID, OFFICIALNAME) %>%
  distinct() %>%
  group_by(TEAM_WYID) %>%
  summarise(OFFICIALNAME = first(OFFICIALNAME), .groups = "drop")

match_with_names <- matchbase %>%
  left_join(teams_clean, by = "TEAM_WYID")


matchscore <- match_with_names %>%
  group_by(MATCH_WYID) %>%
  summarise(
    hjemmehold = OFFICIALNAME[SIDE == "home"],
    hjemmescore = SCORE[SIDE == "home"],
    udehold = OFFICIALNAME[SIDE == "away"],
    udescore = SCORE[SIDE == "away"],
    kampresultat = paste(hjemmescore, "-", udescore),
    kamphold = paste(hjemmehold, "-", udehold),
    samlet = paste(kamphold, "   ", kampresultat),
    .groups = "drop"
  )


secondary_combined <- apply(passes[, paste0("SECONDARYTYPE", 1:10)], 1, paste, collapse = " ")

