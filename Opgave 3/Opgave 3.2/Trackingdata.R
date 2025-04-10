library(jsonlite)
library(mongolite)
library(tidyverse)
library(ggsoccer)
library(ggforce)
library(gganimate)
library(sp)
#### Tracking data ####
#### Connections ####

conMeta = mongo(
  url = "mongodb://localhost",
  db = "Eksamen_vbob",
  collection = "Meta"
)


conRaw = mongo(
  url = "mongodb://localhost",
  db = "Eksamen_vbob",
  collection = "Raw"
)

#### Oprettelse af dataframes ####

dfraw = conRaw$find(query = "{}", fields = "{}")
dfmeta = conMeta$find(query = "{}", fields = "{}")

#### Feature engineering ####

## Flatten + kronologisk rækkefølge
dfmeta = jsonlite::flatten(dfmeta)
dfraw = jsonlite::flatten(dfraw)
dfraw = dfraw %>% 
  arrange(period, gameClock)

## Indsætning af spillernanvne på ID i de nestede dataframes
spillere_hjemmehold = as.data.frame(dfmeta$homePlayers)
spillere_udehold = as.data.frame(dfmeta$awayPlayers)
spillere = bind_rows(spillere_hjemmehold, spillere_udehold)

dfraw$homePlayers <- lapply(dfraw$homePlayers, function(df) {
  merge(df, spillere[, c("ssiId", "name")], by.x = "playerId", by.y = "ssiId", all.x = TRUE)
})

dfraw$awayPlayers <- lapply(dfraw$awayPlayers, function(df) {
  merge(df, spillere[, c("ssiId", "name")], by.x = "playerId", by.y = "ssiId", all.x = TRUE)
})

#### Omformater gameclock ####
convert_to_time <- function(seconds) {
  minutes <- floor(seconds / 60)
  secs <- floor(seconds %% 60)
  frames <- round((seconds %% 1) * 25)  # Konverter til frames (25 fps)
  sprintf("%02d:%02d:%02d", minutes, secs, frames)
}

# Anvend funktionen på din kolonne
dfraw$Time <- sapply(dfraw$gameClock, convert_to_time)

#### Subsetting og finder korrekt mål i kamp ####

dfraw = readRDS("rawtracking.rds") ## det ovenstående FE gemt i RDS

which(dfraw$Time == "24:20:00") ## 36501 
which(dfraw$Time == "24:26:00") ## 36651  

dfsub <- dfraw[36501:36651,]
row.names(dfsub) <- NULL

dfsub$testx <- dfsub$BoldX + 52.5
dfsub$testy <- dfsub$BoldY + 40

#### Frame extract ####
framehome <- dfsub[[6]][[68]]
frameaway <- dfsub[[7]][[68]]

framehome <- framehome %>%
  mutate(xyz = ifelse(xyz == "NULL" | xyz == "", NA, xyz)) %>%
  mutate(xyz = str_remove_all(xyz, "c\\(|\\)")) %>% 
  separate(xyz, into = c("x", "y", "z"), sep = ",", convert = TRUE, fill = "right")

frameaway <- frameaway %>%
  mutate(xyz = ifelse(xyz == "NULL" | xyz == "", NA, xyz)) %>%
  mutate(xyz = str_remove_all(xyz, "c\\(|\\)")) %>% 
  separate(xyz, into = c("x", "y", "z"), sep = ",", convert = TRUE, fill = "right")


#### ggsoccer ####
framehome$x <- framehome$x + 52.5
framehome$y <- framehome$y + 40
frameaway$x <- frameaway$x + 52.5
frameaway$y <- frameaway$y + 40




skudskyderen = frameaway[1,]

skudskyderen <- c(x = 13.8, y = 12)

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
cones <- frameaway %>%
  mutate(
    cone = map2(x, y, ~generate_pass_cone(skudskyderen, c(x = .x, y = .y), width_deg = 3))
  ) %>%
  mutate(
    blocked = map_lgl(cone, function(polygon) {
      any(point.in.polygon(framehome$x, framehome$y, polygon$x, polygon$y) > 0)
    })
  ) %>%
  select(name, cone, blocked) %>%
  unnest(cone)


## afgrænsning til afleveringsmuligheder med henhold til progression mod mål
cones = cones[cones$name == "M. Frokjaer-Jensen" | cones$name == "E.  Sabbi" | cones$name == "I. Jebali" , ]


#### Freeze-frame ####
ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, fill = "springgreen4", 
                 colour = "beige", 
                 linewidth = 0.8) +
  theme_pitch() +
  theme(panel.background = element_rect(fill = "#008434")) +
  #geom_line(data = dfsub[68:nrow(dfsub),], aes(x = testx, y = testy), color = "black", size = 0.8)+
  geom_point(data = framehome, aes(x = x, y = y), color = "#fa2d2d", size = 3.5)+
  geom_text(data = framehome, aes(x = x+2, y = y, label = name))+
  geom_point(data = frameaway, aes(x = x, y = y), color = "#1923b3", size = 3.5)+
  geom_text(data = frameaway, aes(x = x+2, y = y, label = name))+
  geom_point(data = dfsub[68,], aes(x = testx, y = testy), color = "white", size = 2.5)+
  geom_polygon(data = cones,
               aes(x = x, y = y, group = name, fill = blocked),
               alpha = 0.4,
               color = NA) +
  scale_fill_manual(values = c(`TRUE` = "red", `FALSE` = "green")) +
  guides(fill = "none") + 
  coord_flip(xlim = c(0, 60))+
  scale_y_reverse()
 
