# Indhentning af anvendte pakker
library(shiny)
library(tidyverse)
library(DBI)
library(RMariaDB)
library(ggplot2)
library(plotly)
library(ggsoccer)
library(factoextra)
library(ClusterR)
library(RColorBrewer)

# bagvedliggende source-script med databehandling 
source("Shinydata.R")


# Laver en farvepalette til vores clustering 
cluster_palette <- RColorBrewer::brewer.pal(6, "Set2")

    ############
    #### UI ####
    ############

# Side 1: Forside
ui_forside <- tabPanel("Forside",
                       fluidPage(
                         tags$head(
                           tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Poppins:wght@700&display=swap"),
                           tags$style(HTML("
        .forside-container {
          position: fixed;
          inset: 0;
          background-image: url('https://media.lex.dk/media/150565/article_topimage_Br%C3%B8ndby2.jpg');
          background-size: cover;
          background-position: center;
          z-index: -1;
        }
        .forside-overlay {
          position: fixed;
          inset: 0;
          background-color: rgba(0, 0, 0, 0.80);
          z-index: 0;
        }
        .forside-content {
          position: relative;
          z-index: 1;
          height: 100vh;
          display: flex;
          flex-direction: column;
          justify-content: center;
          align-items: center;
          text-align: center;
        }
        .forside-img {
          max-width: 250px;
          height: auto;
          margin-bottom: 30px;
          box-shadow: 0px 10px 25px rgba(0,0,0,0.7);
        }
        .forside-title {
          font-family: 'Poppins', sans-serif;
          font-size: 48px;
          color: #F5D000;
          text-shadow: 3px 3px 8px rgba(0,0,0,0.7);
        }
      "))
                         ),
                         div(class = "forside-container"),
                         div(class = "forside-overlay"),
                         div(class = "forside-content",
                             img(
                               src = "https://upload.wikimedia.org/wikipedia/en/thumb/b/b5/Brondby_IF_logo.svg/800px-Brondby_IF_logo.svg.png",
                               class = "forside-img"
                             ),
                             div(class = "forside-title",
                                 "Shinyapp for visualisering af afleveringsclustre"
                             )
                         )
                       )
)

# Side 2: Dynamisk Visualisering af clustering pr. kamp
ui_visualisering <- tabPanel("Clustering pr. kamp",
                             fluidPage(
                               tags$div(
                                 style = "position: relative; 
               background-image: url('https://media.lex.dk/media/150565/article_topimage_Br%C3%B8ndby2.jpg'); 
               height: 300px; 
               background-size: cover; 
               background-position: center; 
               margin-bottom: 20px;",
                                 tags$div(
                                   style = "position: absolute; 
                 top: 0; left: 0; right: 0; bottom: 0; 
                 background-color: rgba(0, 0, 0, 0.4);"
                                 ),
                                 tags$div(
                                   class = "app-title",
                                   style = "position: absolute; 
                 top: 30%; 
                 left: 50%; 
                 transform: translate(-50%, -30%);",
                                   "Visualisering af afleverings-clusters for Brøndby IF"
                                 )
                               ),
                               tags$head(
                                 tags$style(HTML("
        body {
          background-color: #0033A0;
          color: white;
          font-family: 'Lato', sans-serif;
        }
        .app-title {
          font-size: 50px;
          font-weight: bold;
          text-align: center;
          color: #F5D000;
          text-shadow: 2px 2px 4px #000000;
          -webkit-text-stroke: 1px black;
        }
        h1, h2, h3, h4 {
          color: #F5D000;
        }
        .well {
          background-color: #002266;
          border: none;
        }
        .form-control, .selectize-input {
          background-color: #F5D000 !important;
          color: #0033A0 !important;
          font-weight: bold;
        }
        .btn {
          background-color: #F5D000;
          color: #0033A0;
          font-weight: bold;
        }
        table {
          color: white;
        }
        table > tbody > tr:nth-child(odd) {
           background-color: #002266 !important;
        }
        table > tbody > tr:nth-child(even) {
           background-color: #003366 !important;
        }
      "))
                               ),
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput("match_id", "Vælg kamp:", 
                                               choices = setNames(matchscore$MATCH_WYID, matchscore$samlet)),
                                   sliderInput("num_clusters", "Antal clusters (K):", 
                                               min = 1, max = 10, value = 3),
                                   checkboxInput("vis_wss", "Vis WSS-plot", value = FALSE),
                                   br(),
                                   h4("Afleveringsstats for kamp"),
                                   tableOutput("statsTable")
                                 ),
                                 mainPanel(
                                   plotlyOutput("clusterPlot"),
                                   br(),
                                   h4("Stats opdelt pr. cluster"),
                                   tableOutput("clusterStatsTable"),
                                   br(),
                                   conditionalPanel(
                                     condition = "input.vis_wss == true",
                                     h4("WSS-plot (elbow-metoden):"),
                                     plotOutput("wssPlot")
                                   )
                                 )
                               )
                             )
)

# Side 3: Statisk Cluster Analyse
ui_cluster_static <- tabPanel("Clustering Superliga",
                              fluidPage(
                                tags$div(
                                  style = "position: relative; 
               background-image: url('https://media.lex.dk/media/150565/article_topimage_Br%C3%B8ndby2.jpg'); 
               height: 300px; 
               background-size: cover; 
               background-position: center; 
               margin-bottom: 20px;",
                                  tags$div(
                                    style = "position: absolute; 
                 top: 0; left: 0; right: 0; bottom: 0;
                 background-color: rgba(0, 0, 0, 0.4);"
                                  ),
                                  tags$div(
                                    class = "app-title",
                                    style = "position: absolute;
                 top: 30%;
                 left: 50%;
                 transform: translate(-50%, -30%);",
                                    "Visualisering af afleverings-clusters for Brøndby IF"
                                  )
                                ),
                                tags$head(
                                  tags$style(HTML("
        body {
          background-color: #0033A0;
          color: white;
          font-family: 'Lato', sans-serif;
        }
        .app-title {
          font-size: 50px;
          font-weight: bold;
          text-align: center;
          color: #F5D000;
          text-shadow: 2px 2px 4px #000000;
          -webkit-text-stroke: 1px black;
        }
        h1, h2, h3, h4 {
          color: #F5D000;
        }
        .well {
          background-color: #002266;
          border: none;
        }
        .form-control, .selectize-input {
          background-color: #F5D000 !important;
          color: #0033A0 !important;
          font-weight: bold;
        }
        .btn {
          background-color: #F5D000;
          color: #0033A0;
          font-weight: bold;
        }
        table {
          color: white;
        }
        table > tbody > tr:nth-child(odd) {
           background-color: #002266 !important;
        }
        table > tbody > tr:nth-child(even) {
           background-color: #003366 !important;
        }
      "))
                                ),
                                sidebarLayout(
                                  sidebarPanel(
                                    h3("Vælg clustre til fremvisning:"),
                                    # Checkbox for hvert clusterplot; alle slået fra som default
                                    checkboxInput("show_cluster1", "Cluster 1", value = FALSE),
                                    checkboxInput("show_cluster2", "Cluster 2", value = FALSE),
                                    checkboxInput("show_cluster3", "Cluster 3", value = FALSE),
                                    checkboxInput("show_cluster4", "Cluster 4", value = FALSE),
                                    checkboxInput("show_cluster5", "Cluster 5", value = FALSE),
                                    checkboxInput("show_cluster6", "Cluster 6", value = FALSE),
                                    br(),
                                    h4("Afleveringsstats for Superligaen"),
                                    tableOutput("staticStatsTable")
                                  ),
                                  mainPanel(
                                    conditionalPanel(condition = "input.show_cluster1 == true", plotOutput("clusterPlot1")),
                                    conditionalPanel(condition = "input.show_cluster2 == true", plotOutput("clusterPlot2")),
                                    conditionalPanel(condition = "input.show_cluster3 == true", plotOutput("clusterPlot3")),
                                    conditionalPanel(condition = "input.show_cluster4 == true", plotOutput("clusterPlot4")),
                                    conditionalPanel(condition = "input.show_cluster5 == true", plotOutput("clusterPlot5")),
                                    conditionalPanel(condition = "input.show_cluster6 == true", plotOutput("clusterPlot6")),
                                    br(),
                                    h4("Stats opdelt pr. cluster"),
                                    tableOutput("staticClusterStatsTable")
                                  )
                                )
                              )
)

# Den samlede app med tre sider
ui <- navbarPage(
  title = "BIF Clustering // Shinyapp",
  ui_forside,
  ui_visualisering,
  ui_cluster_static
)
    
    ################
    #### Server ####
    ################

server <- function(input, output, session) {
  
  ###  Side 2 - Pr. kamp ###
  filteredData <- reactive({
    passes %>% filter(MATCH_WYID == input$match_id)
  })
  
  output$statsTable <- renderTable({
    data <- filteredData()
    secondary_cols <- paste0("SECONDARYTYPE", 1:10)
    secondary_cols <- secondary_cols[secondary_cols %in% colnames(data)]
    secondary_combined <- apply(data[, secondary_cols, drop = FALSE], 1, paste, collapse = " ")
    
    total_passes <- nrow(data)
    accurate_passes <- sum(as.character(data$ACCURATE) == "true", na.rm = TRUE)
    pass_accuracy_pct <- round(accurate_passes / total_passes * 100, 2)
    avg_pass_length <- round(mean(data$LENGTH, na.rm = TRUE), 2)
    long_passes <- round(sum(data$LENGTH > 30, na.rm = TRUE) / total_passes * 100, 2)
    forward_passes <- round(sum(grepl("forward_pass", secondary_combined)) / total_passes * 100, 2)
    back_passes <- round(sum(grepl("back_pass", secondary_combined)) / total_passes * 100, 2)
    lateral_passes <- round(sum(grepl("lateral_pass", secondary_combined)) / total_passes * 100, 2)
    
    
    
    data.frame(
      Statistik = c(
        "Totale afleveringer",
        "Afleveringsnøjagtighed (%)",
        "Gennemsnitlig afleveringslængde",
        "Lange afleveringer (>30m) (%)",
        "Fremadrettede afleveringer (%)",
        "Bagudrettede afleveringer (%)",
        "Lateral afleveringer (%)"
      ),
      Værdi = c(
        total_passes,
        pass_accuracy_pct,
        avg_pass_length,
        long_passes,
        forward_passes,
        back_passes,
        lateral_passes
      )
    )
  })
  
  output$wssPlot <- renderPlot({
    data <- filteredData()
    clustering_vars <- data %>% select(LOCATIONX, LOCATIONY, LENGTH, ANGLE)
    clustering_vars_scaled <- scale(clustering_vars)
    
    wss <- sapply(1:10, function(k) {
      kmeans(clustering_vars_scaled, centers = k, nstart = 10)$tot.withinss
    })
    
    plot(1:10, wss, type = "b", pch = 19,
         xlab = "Antal clusters (K)", ylab = "Total WSS",
         col = "#F5D000", main = "WSS pr. antal clusters",
         panel.first = grid(), cex.main = 1.2)
  })
  
  clusteredData <- reactive({
    data <- filteredData()
    clustering_vars <- data %>% select(LOCATIONX, LOCATIONY, LENGTH, ANGLE)
    clustering_vars_scaled <- scale(clustering_vars)
    
    set.seed(1234)
    kmeans_result <- kmeans(clustering_vars_scaled, centers = input$num_clusters, nstart = 25)
    
    data$Cluster <- as.factor(kmeans_result$cluster)
    data
  })
  
  output$clusterStatsTable <- renderTable({
    data <- clusteredData()
    secondary_cols <- paste0("SECONDARYTYPE", 1:10)
    secondary_cols <- secondary_cols[secondary_cols %in% colnames(data)]
    secondary_combined <- apply(data[, secondary_cols, drop = FALSE], 1, paste, collapse = " ")
    data$secondary_combined <- secondary_combined
    
    data %>%
      group_by(Cluster) %>%
      summarise(
        "Antal afleveringer" = n(),
        "Succesfulde afleveringer (%)" = round(sum(as.character(ACCURATE) == "true", na.rm = TRUE) / n() * 100, 2),
        "Gennemsnitlig længde" = round(mean(LENGTH, na.rm = TRUE), 2),
        "Gennemsnitlig vinkel" = round(mean(ANGLE, na.rm = TRUE), 2),
        "Gennemsnitlig X-lokation" = round(mean(LOCATIONX, na.rm = TRUE), 2),
        "Gennemsnitlig Y-Lokation" = round(mean(LOCATIONY, na.rm = TRUE), 2),
        "Fremadrettede afleveringer (%)" = round(sum(grepl("forward_pass", secondary_combined)) / n() * 100, 2),
        "Bagudrettede afleveringer (%)" = round(sum(grepl("back_pass", secondary_combined)) / n() * 100, 2),
        "Laterale afleveringer (%)" = round(sum(grepl("lateral_pass", secondary_combined)) / n() * 100, 2)
      )
  }, striped = TRUE, bordered = TRUE)
  
  output$clusterPlot <- renderPlotly({
    data <- clusteredData()
    
    p <- ggplot(data, aes(x = LOCATIONX, y = LOCATIONY)) +
      annotate_pitch(dimensions = pitch_wyscout, colour = "white", fill = "springgreen4") +
      geom_point(aes(color = Cluster,
                     text = paste("Spiller:", FULLNAME,
                                  "<br>Længde:", LENGTH,
                                  "<br>Vinkel:", ANGLE,
                                  "<br>ACCURATE:", ACCURATE)),
                 size = 2, alpha = 0.9) +
      scale_color_brewer(palette = "Set1") +
      theme_pitch() +
      theme(legend.position = "bottom", 
            panel.background = element_rect(fill = "#008434"))
    
    ggplotly(p, tooltip = "text")
  })
  
  ### Side 3 - Samlet ###
  
 # Statisk clustering på hele datasættet med MiniBatchKmeans (k = 6)
  static_cluster <- reactive({
    # Udvælger num var til clust
    passes_kmeans_static <- passes_udvalgte %>%
      mutate(ACCURATE = ifelse(ACCURATE == "true", 1, 0)) %>% 
      select(LENGTH, ANGLE, LOCATIONX, LOCATIONY, ENDLOCATIONX, ENDLOCATIONY, ACCURATE) %>%  
      mutate(across(everything(), ~ as.numeric(.))) %>%  
      na.omit()
    
    passes_kmeans_scaled_static <- scale(passes_kmeans_static)
    
    set.seed(1234)
    km_mb_static <- MiniBatchKmeans(passes_kmeans_scaled_static, clusters = 6, batch_size = 100000)
    
  
    cluster_assignment <- predict_MBatchKMeans(passes_kmeans_scaled_static, CENTROIDS = km_mb_static$centroids)
    
    # Tilføj spillerinfo (fra passes_udvalgte fra sourcefilen)
    passes_kmeans_static <- cbind(passes_kmeans_info, passes_kmeans_static)
    passes_kmeans_static$CLUSTER <- cluster_assignment
    
    passes_kmeans_static
  })
  
  static_cluster_analyse <- reactive({
    static_cluster() %>%
      group_by(CLUSTER) %>%
      summarise(
        "Antal afleveringer" = n(),
        "Afleveringsnøjagtighed (%)" = round(mean(ACCURATE, na.rm = TRUE) * 100, 1),
        "Gennemsnitlig længde" = mean(LENGTH, na.rm = TRUE),
        "Gennemsnitlig vinkel" = mean(ANGLE, na.rm = TRUE),
        "Start X" = mean(LOCATIONX, na.rm = TRUE),
        "Start Y" = mean(LOCATIONY, na.rm = TRUE),
        "Slut X" = mean(ENDLOCATIONX, na.rm = TRUE),
        "Slut Y" = mean(ENDLOCATIONY, na.rm = TRUE),
        Start_X = mean(LOCATIONX, na.rm = TRUE),
        Start_Y = mean(LOCATIONY, na.rm = TRUE),
        Gennemsnitlig_vinkel = mean(ANGLE, na.rm = TRUE)
      )
  })
  

  
  
  # stats for hele datasættet på side 3
  static_stats <- reactive({
    data <- passes_udvalgte %>%
      filter(!is.na(LENGTH), !is.na(ANGLE)) %>%
      mutate(across(c(LENGTH, ANGLE), ~ as.numeric(.)))
    secondary_cols <- paste0("SECONDARYTYPE", 1:10)
    secondary_cols <- secondary_cols[secondary_cols %in% colnames(data)]
    secondary_combined <- apply(data[, secondary_cols, drop = FALSE], 1, paste, collapse = " ")
    
    total_passes <- nrow(data)
    accurate_passes <- sum(as.character(data$ACCURATE) == "true", na.rm = TRUE)
    pass_accuracy_pct <- round(accurate_passes / total_passes * 100, 2)
    avg_pass_length <- round(mean(data$LENGTH, na.rm = TRUE), 2)
    long_passes <- round(sum(data$LENGTH > 30, na.rm = TRUE) / total_passes * 100, 2)
    forward_passes <- round(sum(grepl("forward_pass", secondary_combined)) / total_passes * 100, 2)
    back_passes <- round(sum(grepl("back_pass", secondary_combined)) / total_passes * 100, 2)
    lateral_passes <- round(sum(grepl("lateral_pass", secondary_combined)) / total_passes * 100, 2)
    
    data.frame(
      Statistik = c(
        "Totale afleveringer",
        "Afleveringsnøjagtighed (%)",
        "Gennemsnitlig afleveringslængde",
        "Lange afleveringer (>30m) (%)",
        "Fremadrettede afleveringer (%)",
        "Bagudrettede afleveringer (%)",
        "Lateral afleveringer (%)"
      ),
      Værdi = c(
        total_passes,
        pass_accuracy_pct,
        avg_pass_length,
        long_passes,
        forward_passes,
        back_passes,
        lateral_passes
      )
    )
  })
  
  # Vis superliga stats i sidepanel
  output$staticStatsTable <- renderTable({
    static_stats()
  })
  
  # vis cluster stats under plot
  output$staticClusterStatsTable <- renderTable({
    static_cluster_analyse() %>%
      select(-Start_X, -Start_Y, -Gennemsnitlig_vinkel)
  }, striped = TRUE, bordered = TRUE)
  
  
  # dynamisk fremvisning af statisk cluster plot ting
  static_plot_cluster <- function(cluster_number, color) {
    data_static <- static_cluster() %>% filter(CLUSTER == cluster_number)
    avg_data <- static_cluster_analyse() %>% filter(CLUSTER == cluster_number)
    
    p <- ggplot(data_static, aes(x = LOCATIONX, y = LOCATIONY)) +
      annotate_pitch(dimensions = pitch_wyscout, colour = "white", fill = "springgreen4") + 
      geom_point(aes(color = as.factor(CLUSTER)), alpha = 0.5, size = 2) +  
      scale_color_manual(values = color) +  
      theme_pitch() +
      coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
      labs(
        title = paste("Cluster", cluster_number),
        subtitle = case_when(
          cluster_number == 1 ~ "laterale afleveringer fra venstre banehalvdel",
          cluster_number == 2 ~ "lateral afleveringer fra højre banehalvdel",
          cluster_number == 3 ~ "lange, centrale og ligefrem / fremadgående afleveringer",
          cluster_number == 4 ~ "Korte afleveringer i venstre offensive zone",
          cluster_number == 5 ~ "Lange afleveringer fra bagkæden mod midten",
          cluster_number == 6 ~ "Korte afleveringer i højre offensive zone",
          TRUE ~ ""
        ),
        x = "",
        y = "",
        color = "Cluster"
      ) +
      theme(
        text = element_text(family = "Helvetica"),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(face = "bold", size = 12, hjust = 0.5),
        panel.background = element_rect(fill = "#008434")
      ) +
      geom_point(data = avg_data, aes(x = Start_X, y = Start_Y), 
                 size = 6, shape = 16, color = "black", alpha = 0.8) + 
      geom_segment(data = avg_data, aes(x = Start_X, y = Start_Y,
                                        xend = Start_X + cos(Gennemsnitlig_vinkel * pi / 180) * 20,  
                                        yend = Start_Y + sin(Gennemsnitlig_vinkel * pi / 180) * 20),  
                   arrow = arrow(length = unit(0.5, "cm")),  
                   size = 1, alpha = 0.8, color = "black") + 
      scale_x_continuous(breaks = seq(0, 100, 10), minor_breaks = NULL) +
      scale_y_continuous(breaks = seq(0, 100, 10), minor_breaks = NULL) +
      guides(color = "none")
    
    p
  }
  
  # plot af hver cluster fra 3.1
  output$clusterPlot1 <- renderPlot({
    if (input$show_cluster1) {
      static_plot_cluster(1, cluster_palette[1])
    }
  })
  output$clusterPlot2 <- renderPlot({
    if (input$show_cluster2) {
      static_plot_cluster(2, cluster_palette[2])
    }
  })
  output$clusterPlot3 <- renderPlot({
    if (input$show_cluster3) {
      static_plot_cluster(3, cluster_palette[3])
    }
  })
  output$clusterPlot4 <- renderPlot({
    if (input$show_cluster4) {
      static_plot_cluster(4, cluster_palette[4])
    }
  })
  output$clusterPlot5 <- renderPlot({
    if (input$show_cluster5) {
      static_plot_cluster(5, cluster_palette[5])
    }
  })
  output$clusterPlot6 <- renderPlot({
    if (input$show_cluster6) {
      static_plot_cluster(6, cluster_palette[6])
    }
  })
  
}

shinyApp(ui = ui, server = server)
