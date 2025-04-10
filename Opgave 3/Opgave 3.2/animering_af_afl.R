

#### Animering af aflevering der førte til første mål i kampen ved 24:30 min ####

#### Oprettelse af ny dataframes med alle hjemmespillere ####

hjemme_alle_afl = dfsub_afl[[6]] %>% 
  map_dfr(~ .x, .id = "frameIdx") %>% 
  mutate(
    xyz = ifelse(xyz == "NULL" | xyz == "", NA, xyz),
    xyz = str_remove_all(xyz, "c\\(|\\)"),
  ) %>% 
  separate(xyz, into = c("x", "y", "z"), sep = ",", convert = TRUE, fill = "right") %>%
  mutate(
    x = x + 52.5,
    y = y + 40,
    frame_id = as.integer(frameIdx)
  )

ude_alle_afl = dfsub_afl[[7]] %>% 
  map_dfr(~ .x, .id = "frameIdx") %>%
  mutate(
    xyz = ifelse(xyz == "NULL" | xyz == "", NA, xyz),
    xyz = str_remove_all(xyz, "c\\(|\\)"),
  ) %>%
  separate(xyz, into = c("x", "y", "z"), sep = ",", convert = TRUE, fill = "right") %>%
  mutate(
    x = x + 52.5,
    y = y + 40,
    frame_id = as.integer(frameIdx)
  )

aflevering_gif = ude_alle_afl[ude_alle_afl$name == "J. Breum", ]

frames_afl = unique(hjemme_alle_afl$frame_id)

#### generering af alle cones i spillesekevensen

generate_frame_cones_afl = function(frame_id) {
  frameaway_afl = ude_alle_afl %>% filter(frame_id == !!frame_id)
  framehome_afl = hjemme_alle_afl %>% filter(frame_id == !!frame_id)
  bold_afl = dfsub_afl %>% filter(frame_id == !!frame_id)
  passer_pos = aflevering_gif %>% filter(frame_id == !!frame_id)
  passer = c(x = passer_pos$x, y = passer_pos$y)
  
  cones <- frameaway_afl %>%
    mutate(
      cone = map2(x, y, ~generate_pass_cone(passer, c(x = .x, y = .y), width_deg = 3))
    ) %>%
    mutate(
      blocked = map_lgl(cone, function(polygon) {
        any(point.in.polygon(framehome_afl$x, framehome_afl$y, polygon$x, polygon$y) > 0)
      }),
      frame_id = frame_id
    ) %>%
    select(name, cone, blocked, frame_id) %>%
    unnest(cone)
  
  return(cones)
}

#### Begrænsning af cones til progression mod mål ####

all_cones_afl = map_dfr(frames_afl, generate_frame_cones_afl)
all_cones_afl = all_cones_afl[all_cones_afl$name != "K. Larsen" & all_cones_afl$name != "J. Skjelvik" & all_cones_afl$name != "H. Christian Bernat" , ]


#### Plotting og animering ####

dfsub_afl$frame_id = 1:nrow(dfsub_afl)

ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, fill = "springgreen4", 
                 colour = "beige", linewidth = 0.8) +
  theme_pitch() +
  theme(panel.background = element_rect(fill = "#008434")) +
  
  geom_polygon(data = all_cones_afl,
               aes(x = x, y = y, group = interaction(name, frame_id), fill = blocked),
               alpha = 0.4,
               color = NA) +
  
  geom_point(data = hjemme_alle_afl, aes(x = x, y = y, group = name), color = "#fa2d2d", size = 3.5) +
  geom_point(data = ude_alle_afl, aes(x = x, y = y, group = name), color = "#1923b3", size = 3.5) +
  geom_point(data = dfsub_afl, aes(x = testx, y = testy), color = "white", size = 2.5) +
  
  scale_fill_manual(values = c(`TRUE` = "red", `FALSE` = "green")) +
  guides(fill = "none") +
  coord_flip(xlim = c(0, 65)) +
  scale_y_reverse() +
  
  transition_manual(frame_id)

animate(last_plot(), fps = 25, duration = 5, width = 900, height = 600, 
        renderer = gifski_renderer("afl_til_mål.gif"))

