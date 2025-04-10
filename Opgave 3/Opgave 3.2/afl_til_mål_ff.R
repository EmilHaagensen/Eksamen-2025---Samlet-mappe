#### Trackingdata, afl. der fører til mål, fremfor målet ####

which(dfraw$Time == "24:14:00") ## 36501 
which(dfraw$Time == "24:20:00") ## 36651 

dfsub_afl = dfraw[36351:36501 , ] 

row.names(dfsub_afl) <- NULL

dfsub_afl$testx <- dfsub_afl$BoldX + 52.5
dfsub_afl$testy <- dfsub_afl$BoldY + 40

#### Frame extract ####
framehome_afl <- dfsub_afl[[6]][[71]]
frameaway_afl <- dfsub_afl[[7]][[71]]

framehome_afl <- framehome_afl %>%
  mutate(xyz = ifelse(xyz == "NULL" | xyz == "", NA, xyz)) %>%
  mutate(xyz = str_remove_all(xyz, "c\\(|\\)")) %>% 
  separate(xyz, into = c("x", "y", "z"), sep = ",", convert = TRUE, fill = "right")

frameaway_afl <- frameaway_afl %>%
  mutate(xyz = ifelse(xyz == "NULL" | xyz == "", NA, xyz)) %>%
  mutate(xyz = str_remove_all(xyz, "c\\(|\\)")) %>% 
  separate(xyz, into = c("x", "y", "z"), sep = ",", convert = TRUE, fill = "right")


#### ggsoccer ####
framehome_afl$x <- framehome_afl$x + 52.5
framehome_afl$y <- framehome_afl$y + 40
frameaway_afl$x <- frameaway_afl$x + 52.5
frameaway_afl$y <- frameaway_afl$y + 40




afleveringsshab = frameaway_afl[6,]

afleveringsshab <- c(x = 54.17, y = 25.22)

#### Oprettelse af covering shadows ####

generate_pass_cone <- function(from, to, width_deg = 3, length = NULL, points = 10) {
  if (is.null(length)) {
    length <- sqrt((to["x"] - from["x"])^2 + (to["y"] - from["y"])^2)
  }
  
  width_rad <- (width_deg / 180) * pi
  angle <- atan2(to["y"] - from["y"], to["x"] - from["x"])
  angles <- seq(angle - width_rad, angle + width_rad, length.out = points)
  
  data.frame(
    x = c(from["x"], from["x"] + cos(angles) * length),
    y = c(from["y"], from["y"] + sin(angles) * length)
  )
}


# fortsættelse af covering shadows + blokadefarveskifte
cones_afl <- frameaway_afl %>%
  mutate(
    cone = map2(x, y, ~generate_pass_cone(afleveringsshab, c(x = .x, y = .y), width_deg = 3))
  ) %>%
  mutate(
    blocked = map_lgl(cone, function(polygon) {
      any(point.in.polygon(framehome_afl$x, framehome_afl$y, polygon$x, polygon$y) > 0)
    })
  ) %>%
  select(name, cone, blocked) %>%
  unnest(cone)


## afgrænsning til afleveringsmuligheder med henhold til progression mod mål
cones_afl = cones_afl[cones_afl$name != "K. Larsen" & cones_afl$name != "J. Skjelvik" & cones_afl$name != "H. Christian Bernat" , ]


#### Freeze-frame ####
ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, fill = "springgreen4", 
                 colour = "beige", 
                 linewidth = 0.8) +
  theme_pitch() +
  theme(panel.background = element_rect(fill = "#008434")) +
  #geom_line(data = dfsub[68:nrow(dfsub),], aes(x = testx, y = testy), color = "black", size = 0.8)+
  geom_point(data = framehome_afl, aes(x = x, y = y), color = "#fa2d2d", size = 3.5)+
  geom_text(data = framehome_afl, aes(x = x+2, y = y, label = name))+
  geom_point(data = frameaway_afl, aes(x = x, y = y), color = "#1923b3", size = 3.5)+
  geom_text(data = frameaway_afl, aes(x = x+2, y = y, label = name))+
  geom_point(data = dfsub_afl[71,], aes(x = testx, y = testy), color = "white", size = 2.5)+
  geom_polygon(data = cones_afl,
               aes(x = x, y = y, group = name, fill = blocked),
               alpha = 0.4,
               color = NA) +
  scale_fill_manual(values = c(`TRUE` = "red", `FALSE` = "green")) +
  guides(fill = "none") + 
  coord_flip(xlim = c(0, 65)) +
  scale_y_reverse()
