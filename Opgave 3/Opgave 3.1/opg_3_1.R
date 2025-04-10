library(dplyr)
library(DBI)
library(RMariaDB)
library(ggplot2)
library(readr)

con = dbConnect(MariaDB(),
                user = "root",
                dbname = "Fodbolddata",
                host = "localhost",
                port = 3306,
                password = "--"
                
)
passes <- dbGetQuery(con, "SELECT * FROM passes")

wyscout_players_sl <- read_csv("DATA/wyscout_players_sl.csv")

# Vælg kun de relevante kolonner fra shots
player_info <- wyscout_players_sl %>%
  distinct(PLAYER_WYID, .keep_all = TRUE) %>%
  select(PLAYER_WYID, FIRSTNAME, LASTNAME, HEIGHT, BIRTHDATE, ROLECODE2, FOOT, CURRENTTEAM_WYID)

# Merge dataframes, hvor vi bevarer alle rækker i passes
passes <- left_join(passes, player_info, by = "PLAYER_WYID")

#kun afleveringer
passes <- passes %>% filter(PRIMARYTYPE == "pass")

#samlet navn
passes <- passes %>%
  mutate(FULLNAME = paste(FIRSTNAME, LASTNAME, sep = " "))

library(dplyr)

passes_udvalgte <- passes %>%
  select(ACCURATE, ANGLE, LENGTH, LOCATIONX, LOCATIONY, Home_team, PLAYER_WYID, 
         SECONDARYTYPE1, SECONDARYTYPE2, POSSESSIONTYPE1, ENDLOCATIONX, ENDLOCATIONY, ROLECODE2, FOOT, 
         CURRENTTEAM_WYID, FULLNAME, SEASON_WYID)

#skud per hold i sæson 23/24
passes23 <- passes_udvalgte %>% ##sæson 23/24
  filter(SEASON_WYID == 188945)

passes24 <- passes_udvalgte %>% ##sæson 24/25
  filter(SEASON_WYID == 189918)

# Bevar info kolonner til senere tilføjelse
passes_kmeans_info <- passes_udvalgte %>%
  select(PLAYER_WYID, Home_team, FULLNAME, ROLECODE2)

#kun numeriske til test - begge sæsoner
passes_kmeans <- passes_udvalgte %>%
  mutate(ACCURATE = ifelse(ACCURATE == "true", 1, 0)) %>%  #binær
  select(LENGTH, ANGLE, LOCATIONX, LOCATIONY, ENDLOCATIONX, ENDLOCATIONY, ACCURATE) %>%  
  na.omit()  # Fjern rækker med manglende værdier

# Skalér data
passes_kmeans_scaled <- scale(passes_kmeans)

library(factoextra)

#for meget data til alm kmeans - laver minibatch
library(ClusterR)
# test af  tid for MiniBatchKmeans
system.time({
  km_mb <- MiniBatchKmeans(passes_kmeans_scaled, clusters = 3, batch_size = 500)
})

#### test af antal klynger - WSS ####
library(ClusterR)

#test af flere K-værdier og beregn SSE manuelt
k_values <- 2:10  
sse <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  km_mb <- MiniBatchKmeans(passes_kmeans_scaled, clusters = k_values[i], batch_size = 100000)
  sse[i] <- sum(km_mb$WCSS_per_cluster)  # Sum of squared errors for antal klynger = k
}

# Plot SSE mod K for MiniBatchKmeans
plot(k_values, sse, type = "b", xlab = "Antal klynger", ylab = "Sum af kvadrerede fejl", 
     main = "Elbow metode for MiniBatchKmeans")

#3 eller 6 klynger
set.seed(1234)
# Kør MiniBatchKmeans
km_mb <- MiniBatchKmeans(passes_kmeans_scaled, 
                         clusters = 6, 
                         batch_size = 100000)

# minibatch returnerer ikke en klynge - derfor bruges predict til at forudsige klyngetildeling for hver række i passes_kmeans_scaled

passes_kmeans$CLUSTER <- predict_MBatchKMeans(passes_kmeans_scaled, CENTROIDS = km_mb$centroids)
passes_kmeans <- cbind(passes_kmeans_info, passes_kmeans) #tilføjer original spiller info

#### cluster analyse ####

library(dplyr)

cluster_analyse <-  passes_kmeans %>%
  group_by(CLUSTER) %>%
  summarise(
    Antal = n(),
    Gennemsnit_længde = mean(LENGTH, na.rm = TRUE),
    Gennemsnit_vinkel = mean(ANGLE, na.rm = TRUE),
    Start_X = mean(LOCATIONX, na.rm = TRUE),
    Start_Y = mean(LOCATIONY, na.rm = TRUE),
    Slut_X = mean(ENDLOCATIONX, na.rm = TRUE),
    Slut_Y = mean(ENDLOCATIONY, na.rm = TRUE),
    Succesrate = round(mean(ACCURATE, na.rm = TRUE) * 100, 1)
  )

### positioner i klynger ####
# 1: optælling af roller i klynger
position_cluster_count <- passes_kmeans %>%
  group_by(CLUSTER, ROLECODE2) %>%
  summarise(Player_Count = n(), .groups = "drop") %>%
  arrange(CLUSTER, desc(Player_Count))

#hvilke spillere er mest repræsenteret i hvilken klynge? hvilke hold "lingner " hinandenn afleveringsmæssigt: 

#### spiller klynge ####
#alle spillere er tilddelt flere klynger -  kun vil have én klynge pr. spiller,
# Tæller antallet af afleveringer pr. spiller og klynge
player_cluster_count <- passes_kmeans %>%
  group_by(FULLNAME, CLUSTER) %>%
  summarise(Total_Passes = n(), .groups = "drop") %>%  # Summerer afleveringer pr. spiller og klynge
  arrange(FULLNAME, CLUSTER)  # Sorter efter spiller og klynge

#tildeler hver spiller en klynge ud fra max antal afleveringer i klynge
player_cluster <- passes_kmeans %>%
  group_by(FULLNAME, CLUSTER) %>%
  summarise(Total_Passes = n(), .groups = "drop") %>%
  group_by(FULLNAME) %>%
  filter(Total_Passes == max(Total_Passes)) %>%  # Vælg den klynge med flest afleveringer for hver spiller
  arrange(desc(Total_Passes))

table(player_cluster$CLUSTER)

#team klynge analyse
#1: antal afleveringer hvert hold har i hver klynge
team_cluster_count <- passes_kmeans %>%
  group_by(Home_team, CLUSTER) %>%
  summarise(Total_Passes = n(), .groups = "drop") %>%
  mutate(Cluster_Share = Total_Passes / sum(Total_Passes)) %>%
  arrange(Home_team, desc(Cluster_Share))

#2: klynge til hvert hold
team_cluster <- team_cluster_count %>%
  group_by(Home_team) %>%
  slice_max(Cluster_Share, with_ties = FALSE)  # tager den største for hvert hold


## PCA
prcomp_res <- prcomp(passes_kmeans_scaled)
fviz_pca_var(prcomp_res, col.var = "contrib")
#locationx og endloactionx (samme med y), ligger tæt, da de er tæt korrelerede



#### visualisering af klynger ####

    #### alle afleveringer + gns vinkel og placering for hvert cluster ####
library(ggplot2)
library(dplyr)
library(gridExtra)
library(RColorBrewer)

# Farvepalet 
cluster_palette <- RColorBrewer::brewer.pal(6, "Set2")

# Funktion til at plotte et cluster ad gangen
plot_cluster <- function(cluster_number, color) {
  
  cluster_data <- passes_kmeans %>% filter(CLUSTER == cluster_number) #alt data
  
  cluster_avg_data <- cluster_analyse %>% filter(CLUSTER == cluster_number) #gns data
  
  p <- ggplot(cluster_data, aes(x = LOCATIONX, y = LOCATIONY)) +
    geom_point(aes(color = as.factor(CLUSTER)), alpha = 0.5, size = 2) +  
    scale_color_manual(values = color) +  
    theme_minimal() +
    coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +  # Fodboldbane
    labs(
      title = paste("Cluster", cluster_number),
      x = "X",
      y = "Y",
      color = "Cluster"
    ) +
    theme(
      text = element_text(family = "Helvetica"),
      plot.title = element_text(face = "bold", size = 12,   hjust = 0.5),
      panel.grid.major = element_line(color = "white", linewidth = 0.3),  
      panel.grid.minor = element_blank(),  
      panel.background = element_rect(fill = "#e0f7e0"),  
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14)
    ) +
    
    #gennemsnitlige position og vinkel for det valgte cluster
    geom_point(data = cluster_avg_data, aes(x = Start_X, y = Start_Y), 
               size = 6, shape = 16, color = "black", alpha = 0.8) + 
    
    geom_segment(data = cluster_avg_data, aes(x = Start_X, y = Start_Y, #pil med retning
                                              xend = Start_X + cos(Gennemsnit_vinkel * pi / 180) * 20,  
                                              yend = Start_Y + sin(Gennemsnit_vinkel * pi / 180) * 20),  
                 arrow = arrow(length = unit(0.5, "cm")),  
                 size = 1, alpha = 0.8, color = "black")+ 
    
    #deler banen ved x og y = 50
    geom_vline(xintercept = 50, linetype = "solid", color = "white", linewidth = 1) + 
    geom_hline(yintercept = 50, linetype = "solid", color = "white", linewidth = 1) +  
    scale_x_continuous(breaks = seq(0, 100, 10), minor_breaks = NULL) +
    scale_y_continuous(breaks = seq(0, 100, 10), minor_breaks = NULL) +
    guides(color = "none")
  
  return(p)
}

# plots for alle klynger med en farve fra paletten
cluster_plots <- list()

for(i in 1:6) {
  cluster_plots[[i]] <- plot_cluster(i, cluster_palette[i])
}

#samlet plot af alle 6 klynger
grid.arrange(grobs = cluster_plots[1:6], nrow = 3, ncol = 2) 


    #### rolle i clusters ####
#roller i clusters
rolecode_count <- passes_kmeans %>%
  filter(!is.na(ROLECODE2) & !is.na(CLUSTER)) %>%
  group_by(ROLECODE2, CLUSTER) %>%
  summarise(Player_count = n(), .groups = "drop") %>%
  mutate(Cluster_Share = Player_count / sum(Player_count)) %>%
  arrange(ROLECODE2, desc(Cluster_Share))


library(ggplot2)
library(RColorBrewer)
library(dplyr)

ggplot(rolecode_count, aes(x = factor(CLUSTER), y = Cluster_Share, fill = ROLECODE2)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Paired") +  
  labs(title = "DF dominerer cluster 1 og 2, mens cluster 4 og 6 repræsenterer FW og MD",
       x = "Cluster",
       y = "Andel af spillerne",
       fill = "Rolle") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),  
        legend.text = element_text(size = 10, face = "bold"),
        text = element_text(family = "Helvetica"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"))


####variation i klynger - vinkel og aflevering####

library(ggplot2)
library(gridExtra)
library(grid)

#gennemsnitlig længde per klynge
plot_length <- ggplot(cluster_analyse, aes(x = factor(CLUSTER), y = Gennemsnit_længde, fill = factor(CLUSTER))) +
  geom_bar(stat = "identity", show.legend = FALSE, fill = "lightblue") +  
  labs(title = "Gns. længde per cluster", x = "Klynge", y = "Gennemsnitlig Længde") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),  
        legend.text = element_text(size = 10, face = "bold"),
        text = element_text(family = "Helvetica"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
        axis.text = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"))

#gennemsnitlig vinkel per klynge
plot_angle <- ggplot(cluster_analyse, aes(x = factor(CLUSTER), y = Gennemsnit_vinkel, fill = factor(CLUSTER))) +
  geom_bar(stat = "identity", show.legend = FALSE, fill = "darkblue") + 
  labs(title = "Gns. vinkel per cluster", x = "Klynge", y = "Gennemsnitlig Vinkel") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),  
        legend.text = element_text(size = 10, face = "bold"),
        text = element_text(family = "Helvetica"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
        axis.text = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"))

#samler plots
grid.arrange(
  plot_length, 
  plot_angle, 
  ncol = 2, 
  top = textGrob(
    "Afleveringsvinkel har størst variation", 
    gp = gpar(fontsize = 18, fontface = "bold", fontfamily = "Helvetica")
  )
)
