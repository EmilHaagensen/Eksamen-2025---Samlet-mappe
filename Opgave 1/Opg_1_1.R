library(dplyr)
library(jsonlite)
library(stringr)
library(tidyr)

#### OPG 1.1 ####

#Hent skud data fra SQL
shots <- dbGetQuery(con, "SELECT * FROM SHOTS")

#### NYE VARIABLER ####
### LÆNGDE ####
#udregner distance til mål i procent andel
shots <- shots %>%
  mutate(
    percent_distance_to_goal = sqrt((100 - LOCATIONX)^2 + (50 - LOCATIONY)^2),
  )

#udregner om til banens længe i meter
banelængde = 105  # fodboldbanens længde i meter
banebredde = 68    # fodboldbanens bredde i meter

# Omregning af distance fra procent til meter
shots$meters_to_goal = shots$percent_distance_to_goal * banelængde / 100

#### VINKEL ####
# Vinkel til målet (i grader) - 
goal_width <- 7.32  
goal_center_y <- 50 
goal_x <- 100 

#Måler vinklen fra spillerens koordinater til de to stolper
shots <- shots %>%
  mutate(
    x = abs(goal_x - POSSESSIONENDLOCATIONX),  # distance to goal line
    y = abs(POSSESSIONENDLOCATIONY - goal_center_y),  #lateral distance from goal center
    
    # calculate the goal angle using the geometry method
    shot_angle_geom = atan2(goal_width * x, 
                            x^2 + y^2 - (goal_width / 2)^2) * 180 / pi
  )

#### ZONE VÆRDI ####
shots <- shots %>%
  mutate(ZONE = case_when(
    LOCATIONX > 94 & LOCATIONY >= 45 & LOCATIONY <= 55 ~ 1,
    LOCATIONX > 94 & LOCATIONY >= 37 & LOCATIONY < 45 | LOCATIONY > 55 & LOCATIONY <= 63 ~ 2,
    LOCATIONX >= 90 & LOCATIONX <= 94 & LOCATIONY >= 37 & LOCATIONY <= 63 ~ 3,
    LOCATIONX >= 90 & LOCATIONX <= 94 & (LOCATIONY >= 19 & LOCATIONY < 37 | LOCATIONY > 63 & LOCATIONY <= 81) ~ 4,
    LOCATIONX >= 80 & LOCATIONX < 90 & LOCATIONY >= 19 & LOCATIONY <= 81 ~ 5,
    LOCATIONX > 94 & (LOCATIONY >= 19 & LOCATIONY < 37 | LOCATIONY > 63 & LOCATIONY <= 81) ~ 6,
    LOCATIONX >= 60 & LOCATIONX < 80 & LOCATIONY >= 19 & LOCATIONY <= 81 ~ 7,
    LOCATIONX >= 80 & LOCATIONX <= 100 & (LOCATIONY >= 0 & LOCATIONY < 19 | LOCATIONY > 81 & LOCATIONY <= 100) ~ 8,
    LOCATIONX >= 60 & LOCATIONX < 80 & (LOCATIONY >= 0 & LOCATIONY < 19 | LOCATIONY > 81 & LOCATIONY <= 100) ~ 9,
    TRUE ~ NA_real_  # Hvis ingen betingelser er opfyldt, tildel NA
  ))

### TEST OG TRÆNINGSDATA ####

#### shots sæson 23/24 ####
#skud per hold i sæson 23/24
shots23 <- shots %>%
  filter(SEASON_WYID == 188945)

# goal binær værdi 0 og 1
shots23$goal <- ifelse(shots23$SHOTISGOAL == "true", 1, 0)

set.seed(123) 
#udvælger rækker til træning på baggrund af goal - fordelingen af mål/ikke mål bevares
library(caret)

træning <- createDataPartition(shots23$goal, p = 0.7, list = FALSE)

træningsdata <- shots23[træning, ] #opretter 70% trænings dataframe
testdata <- shots23[-træning, ] #opretter 30% test dataframe


#dataframe med udvalgte variabler fra træningsdata
randomforest_træning <- træningsdata[, c("goal",
                                         "SHOTBODYPART", 
                                         "meters_to_goal", 
                                         "shot_angle_geom",
                                         "ATTACKFLANK", 
                                         "MATCHPERIOD",
                                         "LOCATIONX", 
                                         "LOCATIONY", 
                                         "ZONE",
                                         "POSSESSIONTYPE1",
                                         "ROLECODE3")]

str(randomforest_træning)

#### DATA FORBEREDELSE ####
#Omformater kategoriske variabler til faktorer
randomforest_træning$ROLECODE3 <- factor(randomforest_træning$ROLECODE3, levels = c("FWD","MID" ,"DEF","GKP"))

randomforest_træning$ZONE <- as.factor(randomforest_træning$ZONE)

randomforest_træning = randomforest_træning %>% 
  mutate(across(where(is.character), as.factor))

#Målvariabel som faktor 
randomforest_træning$goal <- as.factor(randomforest_træning$goal)

#Fjerner NA-værdier
randomforest_træning <- na.omit(randomforest_træning)

#### RANDOM FORREST ####
library(randomForest)
rf_model <- randomForest(formula = goal ~ ., data = randomforest_træning, importance = TRUE) 
print(rf_model)

#Visualisér variabelens betydning
importance(rf_model)
varImpPlot(rf_model)

#### MULTIKOLLINEARITET TEST ####
library(corrplot)
cor_træning <- cor(randomforest_træning[, sapply(randomforest_træning, is.numeric)])

par(font = 3) 

corrplot(cor_træning, 
         method = "color",         
         type = "lower",           
         order = "hclust",         
         addCoef.col = "black",    
         col = colorRampPalette(c("#DC143C", "white", "#08306B"))(200), 
         tl.col = "black",
         number.cex = 1,
         diag = FALSE,
         cl.pos = "n",
         tl.srt = 45,
         outline = TRUE,
         tl.cex = 1)



