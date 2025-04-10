library(tidyverse)
source("util.R") ## VIGTIG --> kør den før det her 

#### Opgave 5.2 #####
#### Matches Connection ####
conm <- mongo(
  url = "mongodb://localhost",
  db = "fsoccer",
  collection = "matches"
)

matches = conm$find(query = "{}", fields = "{}")

matches = jsonlite::flatten(matches)

f_matches <- matches %>% 
  filter(competition.competition_name == "Women's World Cup",
         season.season_name >= 2019)

m_matches <- matches %>% 
  filter(competition.competition_name == "FIFA World Cup",
         season.season_name >= 2018)

m_18_id = m_matches$match_id[m_matches$season.season_name == 2018]
m_2022_id = m_matches$match_id[m_matches$season.season_name == 2022]

f_19_id = f_matches$match_id[f_matches$season.season_name == 2019]
f_2023_id = f_matches$match_id[f_matches$season.season_name == 2023]

## dataindhentning
events = readRDS("Komprimeret arkiv/events.rds")
events_female = readRDS("Komprimeret arkiv/f_events.rds")
events_male = readRDS("Komprimeret arkiv/m_events.rds")

alleskud = events[events$type.name == "Shot", ]
skud_female = f_events[f_events$type.name == "Shot", ]
skud_male = m_events[m_events$type.name == "Shot", ]

#kør for 2023 og 2022 
skud_female = skud_female[skud_female$match_id %in% f_2023_id,]
skud_male = skud_male[skud_male$match_id %in% m_2022_id, ]

## Indsætter alle dataframes i nested-df ## -- IKKE NØDVENDIGT (!!)
ff_female = skud_female[, "shot.freeze_frame"]
ff_female_nested = tibble(data = ff_female)

#### MÆND EGO + PLOTS ####

# Initialiser kolonner
skud_male$ego <- NA
skud_male$plot <- vector("list", length = nrow(skud_male))

goal_x <- 120
goal_y_top <- 44
goal_y_bottom <- 36

for (i in seq_len(nrow(skud_male))) {
  
  if (is.null(skud_male$shot.freeze_frame[[i]])) next
  
  testff <- as.data.frame(skud_male$shot.freeze_frame[[i]])
  
  if (!"location" %in% names(testff) || !is.list(testff$location)) next
  
  testff <- testff %>%
    mutate(
      x = map_dbl(location, ~ if (length(.) >= 1) .[[1]] else NA_real_),
      y = map_dbl(location, ~ if (length(.) >= 2) .[[2]] else NA_real_)
    )
  
  # Hvis der er NA i x eller y, hop til næste iteration
  if (any(is.na(testff$x)) || any(is.na(testff$y))) next
  
  medspillere <- testff %>% filter(teammate == TRUE)
  modstandere <- testff %>% filter(teammate == FALSE)
  
  if (nrow(medspillere) == 0 || nrow(modstandere) == 0) next
  
  shooter <- skud_male[i, ]
  if (is.null(shooter$location[[1]]) || length(shooter$location[[1]]) < 2) next
  
  shooter_x <- shooter$location[[1]][1]
  shooter_y <- shooter$location[[1]][2]
  
  # Antal modstandere i skudkeglen
  shooter_antalmodstandere <- sum(map_dbl(seq_len(nrow(modstandere)), function(k) {
    is_opponent_inside_triangle(modstandere$x[k], modstandere$y[k], shooter_x, shooter_y)
  }))
  
  # Beregn for medspillere
  medspillere <- medspillere %>%
    rowwise() %>%
    mutate(
      antalmodstandere = sum(map_dbl(seq_len(nrow(modstandere)), function(k) {
        is_opponent_inside_triangle(modstandere$x[k], modstandere$y[k], x, y)
      })),
      cone = list(generate_pass_cone(
        from = c(x = shooter_x, y = shooter_y),
        to = c(x = x, y = y),
        width_deg = 3
      )),
      blocked = any(point.in.polygon(
        point.x = modstandere$x,
        point.y = modstandere$y,
        pol.x = cone$x,
        pol.y = cone$y
      ) > 0)
    ) %>%
    ungroup()
  
  # Ego-score logik
  sumtjek <- sum(medspillere$antalmodstandere < shooter_antalmodstandere & medspillere$blocked == FALSE)
  skud_male$ego[i] <- sumtjek > 0
  
  # VISUALISERING (gemmes i skud_male$plot)
  shot_cone <- generate_shot_cone(c(x = shooter_x, y = shooter_y))
  
  p <- ggplot() +
    annotate_pitch(dimensions = pitch_statsbomb, colour = "white", fill = "springgreen4") +
    theme_pitch() +
    
    # Skudkegle
    geom_polygon(data = shot_cone, aes(x = x, y = y), fill = "yellow", alpha = 0.25) +
    
    # Passkegler til medspillere
    lapply(seq_len(nrow(medspillere)), function(m) {
      cone_df <- medspillere$cone[[m]]
      geom_polygon(data = cone_df, aes(x = x, y = y), fill = "blue", alpha = 0.15)
    }) +
    
    # Spillere
    geom_point(data = modstandere, aes(x = x, y = y), color = "red", size = 1.5) +
    geom_point(data = medspillere, aes(x = x, y = y), color = "blue", size = 1.5) +
    geom_point(data = data.frame(x = shooter_x, y = shooter_y), aes(x = x, y = y),
               color = "yellow", shape = 17, size = 4) +
    geom_segment(aes(x = goal_x, y = goal_y_bottom, xend = goal_x, yend = goal_y_top),
                 color = "white", size = 1.2) +
    labs(title = paste("Skud nr.:", i, "- Ego:", skud_male$ego[i], "- Skytte:", skud_male$player.name[i])) +
    coord_fixed(xlim = c(60, 120), ylim = c(0, 80))
  
  # Gem plot i samme række
  skud_male$plot[[i]] <- p
  
  cat("✅ Done with row", i, "\n")
}

# For at se et specifikt plot
print(skud_male$plot[[17]])


#### KVINDER EGO + PLOTS ####

# Initialiser kolonner
skud_female$ego <- NA
skud_female$plot <- vector("list", length = nrow(skud_female))

goal_x <- 120
goal_y_top <- 44
goal_y_bottom <- 36

for (i in seq_len(nrow(skud_female))) {
  
  if (is.null(skud_female$shot.freeze_frame[[i]])) next
  
  testff <- as.data.frame(skud_female$shot.freeze_frame[[i]])
  
  if (!"location" %in% names(testff) || !is.list(testff$location)) next
  
  testff <- testff %>%
    mutate(
      x = map_dbl(location, ~ if (length(.) >= 1) .[[1]] else NA_real_),
      y = map_dbl(location, ~ if (length(.) >= 2) .[[2]] else NA_real_)
    )
  
  # Hvis der er NA i x eller y, hop til næste iteration
  if (any(is.na(testff$x)) || any(is.na(testff$y))) next
  
  medspillere <- testff %>% filter(teammate == TRUE)
  modstandere <- testff %>% filter(teammate == FALSE)
  
  if (nrow(medspillere) == 0 || nrow(modstandere) == 0) next
  
  shooter <- skud_female[i, ]
  if (is.null(shooter$location[[1]]) || length(shooter$location[[1]]) < 2) next
  
  shooter_x <- shooter$location[[1]][1]
  shooter_y <- shooter$location[[1]][2]
  
  # Antal modstandere i skudkeglen
  shooter_antalmodstandere <- sum(map_dbl(seq_len(nrow(modstandere)), function(k) {
    is_opponent_inside_triangle(modstandere$x[k], modstandere$y[k], shooter_x, shooter_y)
  }))
  
  # Beregn for medspillere
  medspillere <- medspillere %>%
    rowwise() %>%
    mutate(
      antalmodstandere = sum(map_dbl(seq_len(nrow(modstandere)), function(k) {
        is_opponent_inside_triangle(modstandere$x[k], modstandere$y[k], x, y)
      })),
      cone = list(generate_pass_cone(
        from = c(x = shooter_x, y = shooter_y),
        to = c(x = x, y = y),
        width_deg = 3
      )),
      blocked = any(point.in.polygon(
        point.x = modstandere$x,
        point.y = modstandere$y,
        pol.x = cone$x,
        pol.y = cone$y
      ) > 0)
    ) %>%
    ungroup()
  
  # Ego-score logik
  sumtjek <- sum(medspillere$antalmodstandere < shooter_antalmodstandere & medspillere$blocked == FALSE)
  skud_female$ego[i] <- sumtjek > 0
  
  # VISUALISERING (gemmes i skud_female$plot)
  shot_cone <- generate_shot_cone(c(x = shooter_x, y = shooter_y))
  
  p <- ggplot() +
    annotate_pitch(dimensions = pitch_statsbomb, colour = "white", fill = "springgreen4") +
    theme_pitch() +
    
    # Skudkegle
    geom_polygon(data = shot_cone, aes(x = x, y = y), fill = "yellow", alpha = 0.25) +
    
    # Passkegler til medspillere
    lapply(seq_len(nrow(medspillere)), function(m) {
      cone_df <- medspillere$cone[[m]]
      geom_polygon(data = cone_df, aes(x = x, y = y), fill = "blue", alpha = 0.15)
    }) +
    
    # Spillere
    geom_point(data = modstandere, aes(x = x, y = y), color = "red", size = 1.5) +
    geom_point(data = medspillere, aes(x = x, y = y), color = "blue", size = 1.5) +
    geom_point(data = data.frame(x = shooter_x, y = shooter_y), aes(x = x, y = y),
               color = "yellow", shape = 17, size = 4) +
    geom_segment(aes(x = goal_x, y = goal_y_bottom, xend = goal_x, yend = goal_y_top),
                 color = "white", size = 1.2) +
    labs(title = paste("Skud nr.:", i, "- Ego:", skud_female$ego[i], "- Skytte:", skud_female$player.name[i])) +
    coord_fixed(xlim = c(60, 120), ylim = c(0, 80))
  
  # Gem plot i samme række
  skud_female$plot[[i]] <- p
  
  cat("✅ Done with row", i, "\n")
}

# For at se et specifikt plot
print(skud_female$plot[[17]])


#### Undersøgelse af fordeling ####

# Beregn fordeling af ego-score for mænd i 2022
mænd_ego <- as.data.frame(table(skud_male$ego))
names(mænd_ego) <- c("ego", "Freq")
mænd_ego$procent <- round(100 * mænd_ego$Freq / sum(mænd_ego$Freq), 1)
mænd_ego$køn <- "Mænd 2022"  # Tilføj køn og år for mændene

# Beregn fordeling af ego-score for kvinder i 2023
kvinde_ego <- as.data.frame(table(skud_female$ego))
names(kvinde_ego) <- c("ego", "Freq")
kvinde_ego$procent <- round(100 * kvinde_ego$Freq / sum(kvinde_ego$Freq), 1)
kvinde_ego$køn <- "Kvinder 2023"  # Tilføj køn og år for kvinderne

# Kombiner mænd_ego og kvinde_ego til én dataframe
ego_df <- bind_rows(mænd_ego, kvinde_ego)

# Tilføj labels til ego
ego_df$ego_label <- ifelse(ego_df$ego == "TRUE", "Egoistisk skud", "Ikke-egoistisk skud")


#### PLot ####

ggplot(ego_df, aes(x = ego_label, y = procent, fill = ego_label)) +
  geom_col(width = 0.6, color = "black", linewidth = 0.4) +
  geom_text(aes(label = paste0(procent, "%")),
            vjust = -0.3,
            size = 5.2,
            fontface = "bold") +
  scale_fill_manual(
    values = c("Egoistisk skud" = "firebrick", "Ikke-egoistisk skud" = "steelblue")
  ) +
  facet_wrap(~køn) +
  labs(
    title = "Mænd tager flere egoistiske skud end kvinder",
    x = NULL,
    y = "Andel (%)",
    fill = "Skudtype",
    caption = "Datakilde: StatsBomb, baseret på verdensmesterkampe for hhv. kvinder og mænd"
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.08))
  ) +
  theme_minimal(base_size = 15) +
  theme(
    strip.text = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text = element_text(size = 13),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(size = 9, hjust = 1, face = "italic"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "grey60", fill = NA, linewidth = 0.8),
    plot.background = element_rect(color = "grey80", fill = "white", linewidth = 0.8)
  )





