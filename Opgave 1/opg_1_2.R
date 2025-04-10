#### plots opgave 1.2 ####

library(ggplot2)
library(dplyr)
library(ggsoccer)
library(dplyr)
library(ggplot2)

# Forbered data én gang
shots23 <- shots23 %>%
  mutate(
    goal_numeric = as.numeric(SHOTISGOAL == "true"),
    shot_angle_geom = as.numeric(shot_angle_geom),
    meters_to_goal = as.numeric(meters_to_goal)
  )

#### VINKEL ####
# Binning – Skudvinkel
angle_bins <- shots23 %>%
  mutate(angle_bin = cut(shot_angle_geom, breaks = seq(0, 150, by = 5))) %>%
  group_by(angle_bin) %>%
  summarise(
    mid_angle = mean(shot_angle_geom, na.rm = TRUE),
    goal_rate = mean(goal_numeric, na.rm = TRUE),
    n = n()
  ) %>%
  filter(n >= 5)

# Logistisk regression – Skudvinkel
model_angle <- glm(goal_numeric ~ shot_angle_geom, data = shots23, family = "binomial")
curve_data <- data.frame(shot_angle_geom = seq(0, 150, by = 1))
curve_data$pred <- predict(model_angle, newdata = curve_data, type = "response")

# Plot – Skudvinkel
ggplot() +
  geom_point(data = angle_bins, aes(x = mid_angle, y = goal_rate), 
             size = 2.5, shape = 21, fill = "black", color = "white", stroke = 0.3) +
  geom_line(data = curve_data, aes(x = shot_angle_geom, y = pred), 
            color = "#a11d1d", linewidth = 1.5) +
  labs(
    title = "Chancen for mål stiger markant med større skudvinkel",
    subtitle = "Bin-gennemsnit (5°) og logistisk regression",
    x = "Skudvinkel (grader)",
    y = "Sandsynlighed for mål"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

#### AFSTAND ####
# Binning – Afstand
distance_bins <- shots23 %>%
  mutate(dist_bin = cut(meters_to_goal, breaks = seq(0, 50, by = 2))) %>%
  group_by(dist_bin) %>%
  summarise(
    mid_dist = mean(meters_to_goal, na.rm = TRUE),
    goal_rate = mean(goal_numeric, na.rm = TRUE),
    n = n()
  ) %>%
  filter(n >= 5)

# Logistisk regression – Afstand
model_dist <- glm(goal_numeric ~ meters_to_goal, data = shots23, family = "binomial")
curve_data_dist <- data.frame(meters_to_goal = seq(0, 50, by = 1))
curve_data_dist$pred <- predict(model_dist, newdata = curve_data_dist, type = "response")

# Plot – Afstand
ggplot() +
  geom_point(data = distance_bins, aes(x = mid_dist, y = goal_rate), 
             size = 2.5, shape = 21, fill = "black", color = "white", stroke = 0.3) +
  geom_line(data = curve_data_dist, aes(x = meters_to_goal, y = pred), 
            color = "#0a3c66", linewidth = 1.5) +
  labs(
    title = "Chancen for mål falder markant med øget afstand til mål",
    subtitle = "Bin-gennemsnit (2m) og logistisk regression",
    x = "Afstand til mål (meter)",
    y = "Sandsynlighed for mål"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )





#### ANGREBSTYPE ####

# Forbered data
df <- shots23 %>%
  filter(POSSESSIONTYPE1 %in% c("attack", "set_piece_attack", "corner", "counterattack")) %>%
  mutate(
    goal = ifelse(SHOTISGOAL == "true", "Mål", "Ikke mål"),  # Ændrer til labels
    poss_label = case_when(
      POSSESSIONTYPE1 == "attack" ~ "Opbygget",
      POSSESSIONTYPE1 == "set_piece_attack" ~ "Dødbold",
      POSSESSIONTYPE1 == "corner" ~ "Hjørne",
      POSSESSIONTYPE1 == "counterattack" ~ "Omstilling"
    )
  )

# Label data – skud, mål, pct
label_data <- df %>%
  group_by(poss_label) %>%
  summarise(
    skud = n(),
    mål = sum(goal == "Mål"),
    pct = round(100 * mål / skud, 1),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0("Skud: ", skud, "\nMål: ", mål, "\n", pct, "% mål"),
    x = 49,
    y = 12  # Hæv lidt op så det ikke rammer bunden
  )

# Plot
ggplot(df, aes(x = LOCATIONX, y = LOCATIONY)) +
  annotate_pitch(fill = "#228B22", colour = "white") +
  
  geom_point(
    aes(fill = goal),
    shape = 21, color = "black", size = 2, alpha = 0.7, stroke = 0.5
  ) +
  
  # Labels
  geom_text(data = label_data, aes(x = x, y = y, label = label),
            inherit.aes = FALSE, hjust = 0, vjust = 1, color = "black", size = 4.5, fontface = "bold") +
  
  scale_fill_manual(
    values = c("Mål" = "#e63946", "Ikke mål" = "white"),
    name = "Afslutning"
  ) +
  
  coord_flip(xlim = c(40, 100), ylim = c(0, 100)) +
  facet_wrap(~ poss_label) +
  labs(
    title = "Opbygget spil skaber flest skud og mål – men omstillinger udnytter chancer bedst",
    subtitle = "Rød = mål | Hvid = ikke mål",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 13),
    strip.text = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid = element_blank()
  )





#### SKUD KROPSDEL ####
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# Klargør data
bodypart_df <- shots23 %>%
  filter(!is.na(SHOTBODYPART)) %>%
  mutate(
    bodypart = case_when(
      SHOTBODYPART == "right_foot" ~ "Højre fod",
      SHOTBODYPART == "left_foot" ~ "Venstre fod",
      SHOTBODYPART == "head_or_other" ~ "Hoved/andet"
    ),
    goal = SHOTISGOAL == "true"
  ) %>%
  group_by(bodypart) %>%
  summarise(
    Skud = n(),
    Mål = sum(goal),
    Scoringsprocent = round(100 * Mål / Skud, 1),
    .groups = "drop"
  ) %>%
  mutate(bodypart = factor(bodypart, levels = c("Højre fod", "Venstre fod", "Hoved/andet")))

# Farver
farver <- c("Højre fod" = "#457b9d", "Venstre fod" = "#1d3557", "Hoved/andet" = "#a8dadc")

# Plot 1: Antal skud og mål
long_data <- bodypart_df %>%
  pivot_longer(cols = c("Skud", "Mål"), names_to = "Type", values_to = "Antal")

p1 <- ggplot(long_data, aes(x = bodypart, y = Antal, fill = Type)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(aes(label = Antal), 
            position = position_dodge(width = 0.6), 
            vjust = -0.4, size = 4.5) +
  scale_fill_manual(values = c("Skud" = "#457b9d", "Mål" = "#e63946")) +
  labs(x = "Kropsdel", y = "Antal") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_blank(),
    legend.position = "bottom"
  )

# Plot 2: Scoringsprocent
p2 <- ggplot(bodypart_df, aes(x = bodypart, y = Scoringsprocent, fill = bodypart)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(Scoringsprocent, "%")), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = farver) +
  labs(x = "Kropsdel", y = "Scoringsprocent (%)") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_blank(),
    legend.position = "none"
  )

# Kombinér med patchwork
p1 + p2 +
  plot_annotation(
    title = "Fødderne står for flest skud og mål – men hoved/andet scorer relativt oftere",
    subtitle = "Antal og scoringsprocent pr. kropsdel, Superligaen 23/24",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  )



#### ZONE ####
# Pakker
library(shiny)
library(ggplot2)
library(dplyr)
library(tibble)
library(DT)
library(shadowtext)
library(viridis)

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #f4f4f4; }
      .container-fluid { padding: 20px; }
      .tab-content { background-color: white; padding: 20px; border-radius: 10px; }
      .dataTables_wrapper .dataTables_filter input { width: 200px; }
    "))
  ),
  
  titlePanel(
    h2("Zonevisualisering med xG Statistik – Superligaen 23/24", style = "font-weight: bold; color: #2C3E50;")
  ),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Interaktiv visualisering af skudzoner med xG og målstatistik."),
      helpText("Klik på fanerne for at skifte mellem grafisk visning og tabel."),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Banekort", plotOutput("zone_plot", height = "900px")),
        tabPanel("Tabel med zonestatistik", dataTableOutput("zone_table"))
      ),
      width = 9
    )
  )
)

server <- function(input, output) {
  # Brug hele sæson 23/24 som datagrundlag
  zone_data <- shots23
  
  zone_data <- zone_data %>%
    mutate(
      SHOTX = as.numeric(LOCATIONX),
      SHOTY = as.numeric(LOCATIONY),
      SHOTXG = as.numeric(SHOTXG),
      SHOTISGOAL = as.numeric(SHOTISGOAL == "true"),
      SHOTONTARGET = as.numeric(SHOTONTARGET == "true")
    ) %>%
    mutate(zone_id = case_when(
      between(SHOTX, 94, 100) & between(SHOTY, 45, 55) ~ "1",
      between(SHOTX, 94, 100) & between(SHOTY, 37, 45) ~ "2h",
      between(SHOTX, 94, 100) & between(SHOTY, 55, 63) ~ "2v",
      between(SHOTX, 84, 94)  & between(SHOTY, 37, 63) ~ "3",
      between(SHOTX, 84, 94)  & between(SHOTY, 19, 37) ~ "4h",
      between(SHOTX, 84, 94)  & between(SHOTY, 63, 81) ~ "4v",
      between(SHOTX, 94, 100) & between(SHOTY, 19, 37) ~ "6h",
      between(SHOTX, 94, 100) & between(SHOTY, 63, 81) ~ "6v",
      between(SHOTX, 64, 84)  & between(SHOTY, 19, 81) ~ "5",
      between(SHOTX, 50, 64)  & between(SHOTY, 19, 81) ~ "7",
      between(SHOTX, 84, 100) & SHOTY < 19             ~ "8h",
      between(SHOTX, 84, 100) & SHOTY > 81             ~ "8v",
      between(SHOTX, 64, 84)  & SHOTY < 19             ~ "9h",
      between(SHOTX, 64, 84)  & SHOTY > 81             ~ "9v",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(zone_id), !is.na(SHOTXG), !is.na(SHOTISGOAL), !is.na(SHOTONTARGET))
  
  zone_stats <- zone_data %>%
    group_by(zone_id) %>%
    summarise(
      shots = n(),
      goals = sum(SHOTISGOAL),
      pct_on_target = round(mean(SHOTONTARGET) * 100, 1),
      pct_goal = round(mean(SHOTISGOAL) * 100, 1),
      avg_xG = round(mean(SHOTXG), 3),
      .groups = "drop"
    )
  
  # Samlet statistik til annotation
  overall_stats <- zone_stats %>%
    summarise(
      total_shots = sum(shots),
      total_goals = sum(goals),
      total_goal_pct = round(100 * total_goals / total_shots, 1)
    )
  
  zone_coords <- tribble(
    ~zone_id, ~xmin, ~xmax, ~ymin, ~ymax,
    "1",   94, 100, 45, 55,     # Central inde i feltet
    "2h",  94, 100, 37, 45,     # Højre side af felt
    "2v",  94, 100, 55, 63,     # Venstre side af felt
    "3",   84, 94, 37, 63,      # Foran feltet centralt
    "4h",  84, 94, 19, 37,      # Højre halvrum udenfor felt
    "4v",  84, 94, 63, 81,      # Venstre halvrum udenfor felt
    "5",   64, 84, 19, 81,      # Midten udenfor felt
    "6h",  94, 100, 19, 37,     # Højre lille boks
    "6v",  94, 100, 63, 81,     # Venstre lille boks
    "7",   50, 64, 19, 81,      # Midtbane/udezoner
    "8h",  84, 100, 0, 19,      # Fløj nede
    "8v",  84, 100, 81, 100,    # Fløj oppe
    "9h",  64, 84, 0, 19,       # Halvrum nede
    "9v",  64, 84, 81, 100      # Halvrum oppe
  )
  
  zone_plot_data <- left_join(zone_coords, zone_stats, by = "zone_id") %>%
    mutate(
      xmid = (xmin + xmax) / 2,
      ymid = (ymin + ymax) / 2,
      label = paste0(zone_id, "\n", shots, " skud\n", pct_goal, "% mål\nxG: ", avg_xG),
      text_color = ifelse(avg_xG > 0.15, "white", "black"),
      text_size = ifelse(zone_id %in% c("1", "2h", "2v"), 3.8, 4.2)
    )
  
  output$zone_plot <- renderPlot({
    ggplot() +
      geom_rect(aes(xmin = 0, xmax = 100, ymin = 0, ymax = 100), fill = "#228B22") +
      geom_segment(aes(x = 94, xend = 94, y = 19, yend = 81), color = "white", linewidth = 1.2) +
      geom_segment(aes(x = 100, xend = 100, y = 19, yend = 81), color = "white", linewidth = 1.2) +
      geom_segment(aes(x = 94, xend = 100, y = 19, yend = 19), color = "white", linewidth = 1.2) +
      geom_segment(aes(x = 94, xend = 100, y = 81, yend = 81), color = "white", linewidth = 1.2) +
      geom_rect(data = zone_plot_data,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = avg_xG),
                color = "white", alpha = 0.9) +
      geom_text(data = zone_plot_data,
                aes(x = xmid, y = ymid, label = label, color = text_color, size = text_size),
                fontface = "bold", lineheight = 1.1, show.legend = FALSE) +
      scale_color_identity() +
      scale_size_identity() +
      scale_fill_gradient(low = "#d9f0a3", high = "#e34a33", name = "Gns. xG",
                          limits = c(0, max(zone_plot_data$avg_xG, na.rm = TRUE))) +
      coord_fixed(xlim = c(40, 100), ylim = c(0, 100), expand = FALSE) +
      theme_void() +
      theme(
        legend.position = "right",
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.margin = ggplot2::margin(10, 10, 10, 10, unit = "pt")
      ) +
      ggtitle("Centrale skudzoner tæt på målet genererer flest mål og højeste xG") +
      annotate("text", x = 70, y = 2, label = paste0(
        "Samlet: ", overall_stats$total_shots, " skud, ",
        overall_stats$total_goals, " mål (", overall_stats$total_goal_pct, "%)"
      ), size = 5.5, fontface = "bold", color = "black", hjust = 0.5)
  })
  
  output$zone_table <- renderDataTable({
    datatable(zone_stats,
              colnames = c("Zone", "Skud", "Mål", "% På mål", "% Mål", "Gns. xG"),
              options = list(pageLength = 15, autoWidth = TRUE),
              class = 'stripe hover cell-border compact') %>%
      formatStyle(
        'avg_xG',
        background = styleColorBar(range(zone_stats$avg_xG), 'lightblue'),
        backgroundSize = '100% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
}

# Kør appen
shinyApp(ui, server)

