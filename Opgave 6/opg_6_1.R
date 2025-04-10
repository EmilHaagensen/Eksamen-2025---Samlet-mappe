library(tidyverse)

#indlæser data fra kvindeliga 2023
f_events <- readRDS("f_events_2023.rds") #kun liga 2023

#Filterer til handlinger under pres
under_pres <- f_events %>%
  filter(under_pressure == TRUE)

#### MOD SCORE ####
under_pres <- under_pres %>%
  mutate(mod_score = case_when(
    type.name == "Dribble" ~ 1,  
    type.name == "Dribble" & dribble.nutmeg == TRUE ~ 2, 
    type.name == "Shot" ~ 1,  
    type.name == "Pass" & (pass.through_ball == TRUE |
                             pass.cut_back == TRUE |
                             pass.switch == TRUE |
                             pass.shot_assist == TRUE) ~ 1, 
    TRUE ~ 0  
  ))

#Modigste spillere på baggrund af modscore 
modige_spillere <- under_pres %>%
  group_by(player.name, team.name) %>%
  summarise(
    mod_total = sum(mod_score, na.rm = TRUE),  # Samler modscore for hver spiller
    dribbles = sum(type.name == "Dribble", na.rm = TRUE), 
    nutmegs = sum(dribble.nutmeg == TRUE, na.rm = TRUE), 
    shots = sum(type.name == "Shot", na.rm = TRUE),
    passes = sum(type.name == "Pass" & 
                   (pass.through_ball == TRUE | pass.cut_back == TRUE | 
                      pass.switch == TRUE | pass.shot_assist == TRUE), na.rm = TRUE), 
    matches_played = n_distinct(match_id)  # Antal kampe spiller har spillet
  ) %>%
  arrange(desc(mod_total))

#Modscore per kamp
modige_spillere <- modige_spillere %>%
  mutate(mod_per_game = mod_total / matches_played) %>%
  arrange(desc(mod_per_game))

#Top 10 modigste spillere
head(modige_spillere, 10)

#Sakina Ouzraoui Diki er den modigste spiller i ud fra modscoren 



#### Modigste hold ####
# Saml modscore for holdet
modige_hold <- modige_spillere %>%
  group_by(team.name) %>%
  summarise(
    mod_total_team = sum(mod_per_game, na.rm = TRUE), 
    total_dribbles = sum(dribbles, na.rm = TRUE),
    total_nutmegs = sum(nutmegs, na.rm = TRUE),  
    total_shots = sum(shots, na.rm = TRUE),  
    total_passes = sum(passes, na.rm = TRUE),  
    matches_played_team = n_distinct(under_pres$match_id[under_pres$team.name == team.name])  # antal unikke kampe holdet har spillet
  ) %>%
  mutate(
    mod_per_game_team = mod_total_team / matches_played_team  # beregn modscore per kamp for holdet
  ) %>%
  arrange(desc(mod_per_game_team))  #Sortér efter højeste modscore per kamp

# Vis top 10 modigste hold pr. kamp
head(modige_hold, 10)


#### KORRELATIONSANALYSE ####

##### Spiller####

#samler antal skud, mål og modscore for hver spiller
spiller_skud_og_maal <- under_pres %>%
  filter(type.name == "Shot") %>%
  group_by(player.name) %>%
  summarise(
    shots = n(), 
    goals = sum(shot.outcome.name == "Goal", na.rm = TRUE)  # antal mål
  )

mod_vs_skud <- modige_spillere %>%
  left_join(spiller_skud_og_maal, by = "player.name")

#Her tages den samlede modscore i betragtning, da både antallet af skud og mål er baseret på totalerne for hver spiller.

# Spiller korrelation mellem mod og skud
cor(mod_vs_skud$mod_total, mod_vs_skud$shots.y, use = "complete.obs") #0.5640924

# Spiller korrelation mellem mod og mål
cor(mod_vs_skud$mod_total, mod_vs_skud$goals, use = "complete.obs") #0.2474009


##### HOLD ####

hold_skud_og_maal <- under_pres %>%
  filter(type.name == "Shot") %>%
  group_by(team.name) %>%
  summarise(
    shots = n(),  # antal skud
    goals = sum(shot.outcome.name == "Goal", na.rm = TRUE)  # antal mål
  )

mod_vs_skud_hold <- modige_hold %>%
  left_join(hold_skud_og_maal, by = "team.name")

# Korrelation mellem mod og skud - hold
cor(mod_vs_skud_hold$mod_total_team, mod_vs_skud_hold$shots, use = "complete.obs") #0.5348801

# Korrelation mellem mod og mål -. hold
cor(mod_vs_skud_hold$mod_total_team, mod_vs_skud_hold$goals, use = "complete.obs") #0.423614



#visualisering
mod_vs_skud_hold$team.name <- gsub(" Women's", "", mod_vs_skud_hold$team.name)

library(ggplot2)
library(RColorBrewer)

#sammenhængen mellem modscore, skud og mål for hold

ggplot(mod_vs_skud_hold) +
  geom_point(aes(x = mod_total_team, y = total_shots, color = "Skud"), size = 3, shape = 16) +
  geom_point(aes(x = mod_total_team, y = goals, color = "Mål"), size = 3, shape = 16) +
  geom_smooth(aes(x = mod_total_team, y = total_shots), method = "lm", se = FALSE, color = brewer.pal(3, "Set2")[2], linetype = "solid") +
  geom_smooth(aes(x = mod_total_team, y = goals), method = "lm", se = FALSE, color = brewer.pal(3, "Set2")[1], linetype = "solid") +

  geom_text(aes(x = mod_total_team, y = total_shots, label = team.name), vjust = -0.5, hjust = -0.1, color = "black", size = 3) +
  geom_text(aes(x = mod_total_team, y = goals, label = team.name), vjust = 1.5, hjust = -0.1, color = "black", size = 3) +

  scale_color_manual(values = brewer.pal(3, "Set2")) +
  
  ylim(0, 30) +

  labs(title = "Hold med mere modige spillere tager flere skud og scorer flere mål",
       x = "Total Modscore for Holdet",
       y = "Antal Skud og Mål",
       color = "Variabel") +
  theme_minimal() +
  theme(legend.position = "top", 
        legend.title = element_blank(),  
        legend.text = element_text(size = 10, face = "bold"),
        text = element_text(family = "Helvetica"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"))























