

#### Animering af første mål i kampen ved 24:30 min ####

#### Oprettelse af ny dataframes med alle hjemmespillere ####

hjemme_alle = dfsub[[6]] %>% 
  map_dfr(~ .x, .id = "frame_id") %>% 
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

ude_alle = dfsub[[7]] %>% 
  map_dfr(~ .x, .id = "frame_id") %>%
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

skudskyder_gif = ude_alle[ude_alle$name == "M. Frokjaer-Jensen" , ]

frames = unique(hjemme_alle$frame_id)

#### generering af alle cones i spillesekevensen

generate_frame_cones = function(frame_id) {
  frameaway = ude_alle %>% filter(frame_id == !!frame_id)
  framehome = hjemme_alle %>% filter(frame_id == !!frame_id)
  bold = dfsub %>% filter(frame_id == !!frame_id)
  shooter_pos = skudskyder_gif %>% filter(frame_id == !!frame_id)
  shooter = c(x = shooter_pos$x, y = shooter_pos$y)
  
  cones <- frameaway %>%
    mutate(
      cone = map2(x, y, ~generate_pass_cone(shooter, c(x = .x, y = .y), width_deg = 3))
    ) %>%
    mutate(
      blocked = map_lgl(cone, function(polygon) {
        any(point.in.polygon(framehome$x, framehome$y, polygon$x, polygon$y) > 0)
      }),
      frame_id = frame_id
    ) %>%
    select(name, cone, blocked, frame_id) %>%
    unnest(cone)
  
  return(cones)
}

#### Begrænsning af cones til progression mod mål ####
 
all_cones = map_dfr(frames, generate_frame_cones)
all_cones = all_cones[all_cones$name == "M. Frokjaer-Jensen" | all_cones$name == "E.  Sabbi" | all_cones$name == "I. Jebali" , ]


#### Plotting og animering ####

ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, fill = "springgreen4", 
                 colour = "beige", linewidth = 0.8) +
  theme_pitch() +
  theme(panel.background = element_rect(fill = "#008434")) +
  
  geom_polygon(data = all_cones,
               aes(x = x, y = y, group = interaction(name, frame_id), fill = blocked),
               alpha = 0.4,
               color = NA) +
  
  geom_point(data = hjemme_alle, aes(x = x, y = y, group = name), color = "#fa2d2d", size = 3.5) +
  geom_point(data = ude_alle, aes(x = x, y = y, group = name), color = "#1923b3", size = 3.5) +
    geom_point(data = dfsub, aes(x = testx, y = testy), color = "white", size = 2.5) +
  
  scale_fill_manual(values = c(`TRUE` = "red", `FALSE` = "green")) +
  guides(fill = "none") +
  coord_flip(xlim = c(0, 60)) +
  scale_y_reverse() +
  
  transition_manual(frame_id)

animate(last_plot(), fps = 25, duration = 5, width = 900, height = 600, 
        renderer = gifski_renderer("covering_shadows.gif"))

