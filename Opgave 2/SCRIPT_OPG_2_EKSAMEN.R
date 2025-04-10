#### Pakker ####
library(tidyverse)
library(RMariaDB)
library(tidyr)
#### Indhentning af data fra SQL samt mindre filtrering ####

## Connection til SQL-DB

con = dbConnect(MariaDB(),
                user = "root",
                dbname = "Fodbolddata",
                host = "localhost",
                port = 3306,
                password = "Nuh-uh"
)

## data-retrieval (primær filtrering er lavet i SQL)

shots = dbReadTable(con, "SHOTS")
passes = dbReadTable(con, "PASSES")
matchdetails = dbReadTable(con, "_matchdetail_base_sl")
## ifelse over sæsonID

shots$season = as.factor(ifelse(shots$SEASON_WYID == 188945, "23/24", "24/25"))
passes$season = as.factor(ifelse(passes$SEASON_WYID == 188945, "23/24", "24/25"))

## Ændrer home-team / away_team -> hold / modstanderhold

colnames(shots)[24] = "hold"
colnames(shots)[26] = "modstanderhold"

colnames(passes)[22] = "hold"
colnames(passes)[24] = "modstanderhold"

## gemmer filerne som RDS til fremadrettet brug

saveRDS(shots, "shots")
saveRDS(passes, "passes")
saveRDS(matchdetails, "matchdetails")




## Stopper connection til SQL
dbDisconnect(con)

# Step 1: summér xG pr. kamp
xg_per_match <- shots %>%
  filter(season == "23/24") %>%
  select(SHOTPOSTSHOTXG, MATCH_WYID, TEAM_WYID, OPPONENTTEAM_WYID) %>%
  group_by(MATCH_WYID, TEAM_WYID, OPPONENTTEAM_WYID) %>%
  summarise(summeret_xG = sum(SHOTPOSTSHOTXG, na.rm = TRUE), .groups = "drop") %>%
  left_join(matchdetails %>% select(MATCH_WYID, TEAM_WYID, SIDE), by = c("MATCH_WYID", "TEAM_WYID"))

# Sammensmeltning af hjemme- og udeholdsteam
home_away_teams <- matchdetails %>%
  select(MATCH_WYID, TEAM_WYID, SIDE) %>%
  spread(SIDE, TEAM_WYID) %>%
  rename(hometeam_id = home, awayteam_id = away)

# Sammensmelt alle data og beregn summen af XG for hjemme- og udehold i én pipe
xg_per_match <- xg_per_match %>%
  left_join(home_away_teams, by = "MATCH_WYID") %>%
  group_by(MATCH_WYID) %>%
  mutate(
    home_xgsum = sum(ifelse(SIDE == "home", summeret_xG, 0)),
    away_xgsum = sum(ifelse(SIDE == "away", summeret_xG, 0))
  ) %>%
  ungroup() %>%
  filter(row_number() %% 2 == 1) %>%
  select(MATCH_WYID, awayteam_id, hometeam_id, home_xgsum, away_xgsum)


### xP-model ####
# Antal simulationer
n_simulations <- 10000

# Simuler mål for hjemme- og udehold
simulate_goals <- function(xg_home, xg_away, n_simulations) {
  home_goals <- rpois(n_simulations, lambda = xg_home)
  away_goals <- rpois(n_simulations, lambda = xg_away)
  return(data.frame(home_goals, away_goals))
}

# Simuler for hver kamp
simulation_results <- xg_per_match %>%
  rowwise() %>%
  mutate(simulations = list(simulate_goals(home_xgsum, away_xgsum, n_simulations)))

# Unnest resultaterne
simulation_results_unnested <- simulation_results %>%
  unnest(simulations)


# Beregn xP for hver simulation
simulation_results_unnested <- simulation_results_unnested %>%
  mutate(
    Home_xP = ifelse(home_goals > away_goals, 3, ifelse(home_goals == away_goals, 1, 0)),
    Away_xP = ifelse(away_goals > home_goals, 3, ifelse(away_goals == home_goals, 1, 0))
  )

# Beregn det gennemsnitlige xP for hjemme- og udehold
xP_results <- simulation_results_unnested %>%
  group_by(MATCH_WYID, hometeam_id, awayteam_id) %>%
  summarise(
    avg_home_xP = mean(Home_xP),
    avg_away_xP = mean(Away_xP)
  )

# Beregn samlede xP for hvert hold
total_xP <- xP_results %>%
  group_by(hometeam_id) %>%
  summarise(total_home_xP = sum(avg_home_xP)) %>%
  bind_rows(
    xP_results %>%
      group_by(awayteam_id) %>%
      summarise(total_away_xP = sum(avg_away_xP))
  ) 


total_xP[1:12, 3:4] = total_xP[13:24, 3:4]
total_xP <- total_xP[1:12,]

total_xP <- total_xP %>% 
  mutate(total_xP = total_home_xP + total_away_xP) %>% 
  select(-awayteam_id) 
colnames(total_xP)[1] <- "teamid"

#### Bootstrapping og Konfidensintervaller ####

# Bootstrapping af xG for hjemmehold og udehold (samme struktur som din oprindelige xP-model)

bootstrap_results <- xg_per_match %>%
  rowwise() %>%
  mutate(
    # Bootstrapping af hjemmeholdets summerede xG, kun baseret på kampe for samme hometeam_id
    boot_home_xg = list(sample(xg_per_match$home_xgsum[xg_per_match$hometeam_id == hometeam_id], 
                               size = n_simulations, replace = TRUE)),
    
    # Bootstrapping af udeholdets summerede xG, kun baseret på kampe for samme awayteam_id
    boot_away_xg = list(sample(xg_per_match$away_xgsum[xg_per_match$awayteam_id == awayteam_id], 
                               size = n_simulations, replace = TRUE))
  ) %>%
  ungroup()


# Simulering af mål for de bootstrappede xG-værdier
bootstrap_results_unnested <- bootstrap_results %>%
  rowwise() %>%
  mutate(
    # Simuler mål for hjemmehold og udehold baseret på de bootstrappede xG-værdier
    boot_simulations = list(simulate_goals(boot_home_xg[[1]], boot_away_xg[[1]], n_simulations))
  )

# Unnest de bootstrappede simulationsresultater
bootstrap_results_unnested <- bootstrap_results_unnested %>%
  unnest(cols = c(boot_simulations)) %>%
  mutate(
    # Beregn bootstrappede xP for hjemmehold og udehold
    boot_home_xP = ifelse(home_goals > away_goals, 3, 
                          ifelse(home_goals == away_goals, 1, 0)),
    boot_away_xP = ifelse(away_goals > home_goals, 3, 
                          ifelse(away_goals == home_goals, 1, 0))
  )

# Nu kan du beregne gennemsnit af xP og konfidensintervallerne for de bootstrappede værdier
bootstrap_xP_results <- bootstrap_results_unnested %>%
  group_by(MATCH_WYID, hometeam_id, awayteam_id) %>%
  summarise(
    boot_avg_home_xP = mean(boot_home_xP),
    boot_avg_away_xP = mean(boot_away_xP),
    boot_home_xP_lower = quantile(boot_home_xP, 0.025),
    boot_home_xP_upper = quantile(boot_home_xP, 0.975),
    boot_away_xP_lower = quantile(boot_away_xP, 0.025),
    boot_away_xP_upper = quantile(boot_away_xP, 0.975)
  )

bootstrap_total_xP <- bootstrap_xP_results %>% 
  group_by(hometeam_id) %>% 
  summarise(total_bootstrap_home_xP = sum(boot_avg_home_xP)) %>% 
  bind_rows(
    bootstrap_xP_results %>% 
      group_by(awayteam_id) %>% 
      summarise(total_bootstrap_away_xP = sum(boot_avg_away_xP))
  )


bootstrap_total_xP[1:12, 3:4] = bootstrap_total_xP[13:24, 3:4]
bootstrap_total_xP <- bootstrap_total_xP[1:12,]

bootstrap_total_xP <- bootstrap_total_xP %>% 
  mutate(total_bootstrap_xP = total_bootstrap_home_xP + total_bootstrap_away_xP) %>% 
  select(-awayteam_id)
colnames(bootstrap_total_xP)[1] <- "teamid"

#### Merger xP_model resultater med bootstrap resultater for at validere, sammenligne og tjekke resultater ####

# Merge bootstrap_total_xp og total_xp på teamid
Bootstrap_vs_xP_model <- merge(bootstrap_total_xP, total_xP, by = "teamid", all = TRUE)








