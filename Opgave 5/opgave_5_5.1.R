#### Opgave 5.1 ####

#### Pakker ####
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(tibble)
library(patchwork)


#### Dataindlæsning ####
setwd("Documents/Dataanlyse/2_semester/Eksamen/")
f_events <- readRDS("f_events.rds")
m_events <- readRDS("m_events.rds")

#### Subset ####
f_events$gender <- "Kvinder"
m_events$gender <- "Herrer"

#matchid samme kolonnenavn
m_events <- m_events %>%
  rename(match_id = matchId)

#sæt sammen to datasæt 
events <- bind_rows(f_events, m_events)

# Hent RDS --> events 
events <- readRDS("events.rds")


#### statistisk analyse ####

# 1. Lav kampoversigt
kampoversigt <- events %>%
  distinct(match_id, gender)

# 2. Tacklinger
tackles <- events %>%
  filter(type.name == "Duel", duel.type.name == "Tackle") %>%
  group_by(match_id, gender) %>%
  summarise(tackles = n(), .groups = "drop")

# 3. Kort
cards <- events %>%
  filter(!is.na(foul_committed.card.name)) %>%
  group_by(match_id, gender) %>%
  summarise(cards = n(), .groups = "drop") %>%
  right_join(kampoversigt, by = c("match_id", "gender")) %>%
  mutate(cards = replace_na(cards, 0))

# 4. Dueller
duels <- events %>%
  filter(type.name == "Duel") %>%
  group_by(match_id, gender) %>%
  summarise(duels = n(), .groups = "drop")

# 5. Frispark
fouls <- events %>%
  filter(type.name == "Foul Committed") %>%
  group_by(match_id, gender) %>%
  summarise(fouls = n(), .groups = "drop")

# 6. Afleveringer
passes <- events %>%
  filter(type.name == "Pass") %>%
  group_by(match_id, gender) %>%
  summarise(passes = n(), .groups = "drop")

# 7. Fejlafleveringer
bad_passes <- events %>%
  filter(type.name == "Pass", pass.outcome.name == "Incomplete") %>%
  group_by(match_id, gender) %>%
  summarise(bad_passes = n(), .groups = "drop")

# 8. Afleveringspræcision
pass_success <- passes %>%
  left_join(bad_passes, by = c("match_id", "gender")) %>%
  mutate(pass_success_rate = (passes - bad_passes) / passes)

# 9. Gennemsnitlig afleveringslængde
pass_length <- events %>%
  filter(type.name == "Pass", !is.na(pass.length)) %>%
  group_by(match_id, gender) %>%
  summarise(avg_pass_length = mean(pass.length), .groups = "drop")

# 10. Bolderobringer
recoveries <- events %>%
  filter(type.name == "Ball Recovery") %>%
  group_by(match_id, gender) %>%
  summarise(recoveries = n(), .groups = "drop")

# 11. Offside
offside <- events %>%
  filter(type.name == "Offside") %>%
  group_by(match_id, gender) %>%
  summarise(offside = n(), .groups = "drop") %>%
  right_join(kampoversigt, by = c("match_id", "gender")) %>%
  mutate(offside = replace_na(offside, 0))

# 12. Skudafstand
shot_distance <- events %>%
  filter(type.name == "Shot", !is.na(location)) %>%
  mutate(
    loc_x = map_dbl(location, 1),
    loc_y = map_dbl(location, 2),
    distance_to_goal = sqrt((120 - loc_x)^2 + (40 - loc_y)^2)
  ) %>%
  group_by(match_id, gender) %>%
  summarise(avg_shot_distance = mean(distance_to_goal), .groups = "drop")

#13. Driblinger
dribbles <- events %>%
  filter(type.name == "Dribble") %>%
  group_by(match_id, gender) %>%
  summarise(dribbles = n(), .groups = "drop")

# 14. Pres
pressure <- events %>%
  filter(type.name == "Pressure") %>%
  group_by(match_id, gender) %>%
  summarise(pressure = n(), .groups = "drop")

# 15. Skud
shots <- events %>%
  filter(type.name == "Shot") %>%
  group_by(match_id, gender) %>%
  summarise(shots = n(), .groups = "drop")

# 16. Skud under pres
# Beregn antal skud under pres pr. kamp
skud_under_pressure_data <- events %>%
  filter(type.name == "Shot", under_pressure == TRUE) %>%
  group_by(gender, match_id) %>%
  summarise(total_shots_under_pressure = n(), .groups = "drop")

# Flet referenceoversigten med skuddata og sæt manglende værdier til 0 (195 observationer -> 200)
skud_under_pressure_complete <- kampoversigt %>%
  left_join(skud_under_pressure_data, by = c("match_id", "gender")) %>%
  mutate(total_shots_under_pressure = replace_na(total_shots_under_pressure, 0))

# 17. Beregn xG under pres
xg_under_pressure <- events %>%
  filter(type.name == "Shot", under_pressure == TRUE) %>%
  group_by(match_id, gender) %>%
  summarise(xg_under_pressure = mean(shot.statsbomb_xg, na.rm = TRUE), .groups = "drop")

#Flet referenceoversigten med xg og sæt manglende værdier til 0 (195 observationer -> 200)
xg_under_pressure_complete <- kampoversigt %>%
  left_join(xg_under_pressure, by = c("match_id", "gender")) %>%
  mutate(xg_under_pressure = replace_na(xg_under_pressure, 0))

# 18. Saml alle data
data_lister <- list(
  skud_under_pressure_complete, tackles, cards, duels, fouls, passes, bad_passes, pass_success, pass_length,
  recoveries, offside, shot_distance, dribbles, pressure,
  shots, xg_under_pressure_complete 
)

#samel overstående i en dataframe (klagøring til gennemsnit)
full_data <- reduce(data_lister, full_join, by = c("match_id", "gender"))

# passes.x og y til passes 
full_data <- full_data %>%
  mutate(
    passes = passes.x,
  ) %>%
  select(-ends_with(".x"), -ends_with(".y"))

# 19. Beregn gennemsnit pr. køn
summary_by_gender <- full_data %>%
  group_by(gender) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))


#### t-test####

# samlet t-tests med p-værdi
t_test_detaljer <- map_dfr(names(t_tests), function(navn) {
  test <- t_tests[[navn]]
  tibble(
    Variabel = navn,
    t_værdi = round(test$statistic, 3),
    df = round(test$parameter, 1),
    p_værdi = format.pval(test$p.value, digits = 3, eps = .Machine$double.eps), 
    Mean_Herrer = round(test$estimate["mean in group Herrer"], 2),
    Mean_Kvinder = round(test$estimate["mean in group Kvinder"], 2),
    CI_nedre = round(test$conf.int[1], 2),
    CI_øvre = round(test$conf.int[2], 2)
  )
})

#plot t-test 
# Konverter p-værdier til numeriske for plot (bruger test$p.value direkte)
t_test_detaljer_plot <- map_dfr(names(t_tests), function(navn) {
  test <- t_tests[[navn]]
  tibble(
    Variabel = navn,
    log_p = -log10(test$p.value),
    p_label = format.pval(test$p.value, digits = 3, eps = .Machine$double.eps)
  )
})

# Plot
ggplot(t_tests_results_df, aes(x = reorder(Variable, Log_P_Value), y = Log_P_Value)) +
  geom_col(fill = "steelblue", color = "black") +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
  annotate("text", x = 1, y = -log10(0.05) + 1.5, label = "Statistisk signifikans (p < 0.05)", 
           color = "red", hjust = 0.1, size = 4.2, fontface = "italic") +
  labs(
    title = "T-test: p-værdier for forskelle mellem køn",
    subtitle = "Højere søjler = stærkere statistisk signifikans",
    caption = "Datakilde: StatsBomb, 100 kampe i verdensmesterskaberne for hhv. kvinder og mænd",
    x = "Variabel",
    y = "-log10(p-værdi)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 9, face = "italic", hjust = 1)
  )



#### Visualiseringer til 5.1 ####

#### cirkeldiagram til skud under pres ####

# 1. Forbered data og omdann "NA" til "Ikke under pres"
shot_data <- events %>%
  filter(type.name == "Shot") %>%
  mutate(Pres = case_when(
    under_pressure == TRUE ~ "Under pres",           
    under_pressure == FALSE ~ "Ikke under pres",      
    is.na(under_pressure) ~ "Ikke under pres",        
    TRUE ~ "Ukendt"                                   
  )) %>%
  group_by(gender, Pres) %>%
  summarise(antal = n(), .groups = "drop") %>%
  group_by(gender) %>%
  mutate(
    procent = round(antal / sum(antal) * 100, 1),
    label = paste0(procent, "%")
  ) %>%
  ungroup()

# Del op i køn
kvinder_data <- filter(shot_data, gender == "Kvinder")
herrer_data  <- filter(shot_data, gender == "Herrer")

# 2. Farver (match køn + presstatus)
kvinde_colors <- c(
  "Under pres" = "#AA3377",
  "Ikke under pres" = "#F4C6D7"
)

herre_colors <- c(
  "Under pres" = "#345C8A",
  "Ikke under pres" = "#B7CCE4"
)


# 3. Pie chart – Kvinder
p_kvinder <- ggplot(kvinder_data, aes(x = "", y = antal, fill = Pres)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 5.5, fontface = "bold") +
  scale_fill_manual(values = kvinde_colors) +
  ggtitle("Kvinder") +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  )

# 4. Pie chart – Herrer
p_herrer <- ggplot(herrer_data, aes(x = "", y = antal, fill = Pres)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 5.5, fontface = "bold") +
  scale_fill_manual(values = herre_colors) +
  ggtitle("Herrer") +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  )

# 5. Saml og giv overordnet stil
(p_herrer + p_kvinder) +
  plot_annotation(
    title = "Kvinder præsterer flere skud under pres",
    subtitle = "Andel af skud under pres ud af alle afslutninger",
    caption = "Datakilde: StatsBomb, 100 kampe i verdensmesterskaberne for hhv. kvinder og mænd",
    theme = theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 13, hjust = 0.5),
      plot.caption = element_text(size = 9, face = "italic", hjust = 1),
      plot.background = element_rect(fill = "white", color = "grey80", linewidth = 1)
    )
  )


#### graf - tracklinger og bolderobringer #### 

# Filtrering af relevante kolonner (Recoveries og Tacklinger) fra full_data
intensity_data <- full_data %>%
  select(gender, recoveries, tackles)

# Beregn gennemsnit pr. køn
intensity_summary <- intensity_data %>%
  group_by(gender) %>%
  summarise(across(c(recoveries, tackles), mean, na.rm = TRUE))

# Long format til plotting
intensity_long <- intensity_summary %>%
  pivot_longer(-gender, names_to = "Type", values_to = "Antal")

# Tilføj faktorniveauer for orden
intensity_long$Type <- factor(intensity_long$Type, levels = c("recoveries", "tackles"))
intensity_long$gender <- factor(intensity_long$gender, levels = c("Kvinder", "Herrer"))

# Farver
custom_colors <- c("Kvinder" = "lightpink",  # Lyserød
                   "Herrer"  = "steelblue")  # Blå

# Plot
ggplot(intensity_long, aes(x = gender, y = Antal, fill = gender)) +
  geom_col(width = 0.6, color = "black", show.legend = FALSE) +  
  facet_wrap(~ Type, scales = "free_y") +
  geom_text(aes(label = round(Antal, 1)), vjust = -0.3, size = 4.5) +
  scale_fill_manual(values = custom_colors) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  labs(
    title = "Kvinder er mere aktive i intense spilsekvenser",
    subtitle = "Gennemsnitligt antal bolderobringer og tacklinger pr. kamp",
    x = NULL,
    caption = "Datakilde: StatsBomb, 100 kampe i verdensmesterskaberne for hhv. kvinder og mænd"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.major.y = element_line(color = "grey90"),
    axis.text.x = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )


