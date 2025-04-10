#### 1.5 #####
# Libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(ggsoccer)
library(DT)
library(bslib)
library(viridis)
library(pROC)

#### XG på alt data ####
#### sæson 23/24 #####
#Forudsiger sandsynligheden for, at et skud bliver til mål på testdata
shots23$predicted_prob <- predict(glm, newdata = shots23, type = "response")

#Test af optimal grænseværdi- CT og youden
roc_kurve23 <- roc(shots23$goal, shots23$predicted_prob)
grænseværdi_shots23 <- coords(roc_kurve23, "best", best.method = "youden")
print(grænseværdi_shots23) #0.1347321

#Konfusionsmatrix
#laver faktor ud fra grænseværdi på 0.129
shots23$predicted_goal <- ifelse(shots23$predicted_prob > 0.134, 1, 0)
shots23_xg <- shots23[,c(82,83)]

#### sæson 24/25 #####
#skud per hold i sæson 23/24
shots24 <- shots %>%
  filter(SEASON_WYID == 189918)

#Forudsiger sandsynligheden for, at et skud bliver til mål på testdata
shots24$predicted_prob <- predict(glm, newdata = shots24, type = "response")

#Test af optimal grænseværdi- CT og youden
roc_kurve24 <- roc(shots24$goal, shots24$predicted_prob)
grænseværdi_shots24 <- coords(roc_kurve24, "best", best.method = "youden")
print(grænseværdi_shots24) #0.101203

#Konfusionsmatrix
#laver faktor ud fra grænseværdi på 0.129
shots24$predicted_goal <- ifelse(shots24$predicted_prob > 0.10, 1, 0)
shots24_xg <- shots24[,c(82,83)]

#samler skuddata fra begge sæsoner med deres respektive xG kolonner
shots_all <- bind_rows(
  mutate(shots23, Season = "2023/2024"),
  mutate(shots24, Season = "2024/2025")
)

#### SHINY APP ####
# UI
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("Interaktivt xG Heatmap for Superligaen"),
  tags$h5("Analyse af skud og expected goals (xG) i Superligaen 23/24", style = "margin-top: -10px; margin-bottom: 20px; color: gray;"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Vælg filtre for at analysere skud og expected goals (xG)."),
      selectizeInput("season", "Vælg sæson:", choices = unique(shots_all$Season)),
      selectizeInput("team", "Vælg hold:", choices = c("Alle", unique(shots_all$Home_team))),
      selectizeInput("player", "Vælg spiller:", choices = c("Alle", unique(shots_all$SHORTNAME))),
      selectInput("attacktype", "Vælg angrebstype:", choices = c("Alle", unique(shots_all$POSSESSIONTYPE1))),
      tags$hr(),
      helpText("Forklaring:"),
      tags$ul(
        tags$li("xG: Forventet mål-sandsynlighed"),
        tags$li("Afstand: Fra mål (meter)"),
        tags$li("Vinkel: Skudvinkel (grader)")
      )
    ),
    
    mainPanel(
      tags$div(
        class = "card p-3 mb-3",
        uiOutput("player_image"),
        tags$p(style = "font-size: 18px; font-weight: bold;", textOutput("summary"))
      ),
      
      tabsetPanel(
        tabPanel("xG Heatmap", plotlyOutput("heatmap", height = "850px")),
        tabPanel("Tabel", DTOutput("shot_table"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  observeEvent(input$season, {
    data <- shots_all %>% filter(Season == input$season)
    updateSelectInput(session, "team", choices = c("Alle", sort(unique(data$Home_team))))
  })
  
  observeEvent(list(input$season, input$team), {
    data <- shots_all %>% filter(Season == input$season)
    if (input$team != "Alle") {
      data <- data %>% filter(Home_team == input$team)
    }
    updateSelectInput(session, "player", choices = c("Alle", sort(unique(data$SHORTNAME))))
  })
  
  filtered_data <- reactive({
    data <- shots_all %>% filter(Season == input$season)
    if (input$team != "Alle") data <- data %>% filter(Home_team == input$team)
    if (input$player != "Alle") data <- data %>% filter(SHORTNAME == input$player)
    if (input$attacktype != "Alle") data <- data %>% filter(POSSESSIONTYPE1 == input$attacktype)
    data
  })
  
  # Spillerbillede + navn
  output$player_image <- renderUI({
    df <- filtered_data()
    if (input$player == "Alle") return(NULL)
    
    img_url <- df$IMAGEDATAURL[1]
    navn <- input$player
    
    if (is.null(img_url) || img_url == "") return(NULL)
    
    tagList(
      tags$div(style = "text-align: center;",
               tags$img(
                 src = img_url,
                 height = "170px",
                 style = "border-radius: 10px; box-shadow: 0 2px 8px rgba(0,0,0,0.25); margin-bottom: 10px;"
               ),
               tags$h5(navn, style = "margin: 0; font-weight: bold; color: #333;")
      )
    )
  })
  
  output$summary <- renderText({
    df <- filtered_data()
    paste0(
      "Gennemsnitlig xG: ", round(mean(df$predicted_prob, na.rm = TRUE), 3),
      " | Antal skud: ", nrow(df),
      " | Mål: ", sum(df$SHOTISGOAL == "true", na.rm = TRUE)
    )
  })
  
  output$heatmap <- renderPlotly({
    plot_data <- filtered_data()
    
    validate(need(nrow(plot_data) > 0, "Ingen data matcher dine valg – prøv en anden kombination."))
    
    if (!"xG_model_tree" %in% colnames(plot_data)) plot_data$xG_model_tree <- NA
    if (!"xG_model_mlp" %in% colnames(plot_data)) plot_data$xG_model_mlp <- NA
    
    plot_data$tooltip <- paste0(
      "Spiller: ", plot_data$SHORTNAME,
      "<br>Hold: ", plot_data$Home_team,
      "<br>xG: ", round(plot_data$predicted_prob, 3),
      "<br>Afstand: ", round(plot_data$meters_to_goal, 1), " m",
      "<br>Vinkel: ", round(plot_data$shot_angle_geom, 1), "°",
      "<br>Mål: ", ifelse(plot_data$SHOTISGOAL == "true", "Ja", "Nej"),
      "<br>På mål: ", ifelse(plot_data$SHOTONTARGET == "true", "Ja", "Nej")
    )
    
    p <- ggplot(plot_data, aes(x = LOCATIONX, y = LOCATIONY)) +
      annotate_pitch(colour = "white", fill = "#228B22") +
      geom_point(aes(color = predicted_prob, text = tooltip), size = 2, alpha = 0.8) +
      scale_color_viridis_c(option = "C", name = "xG", limits = c(0, 0.6), oob = scales::squish) +
      coord_flip(xlim = c(40, 100), ylim = c(0, 100)) +
      labs(title = "xG Heatmap på fodboldbane") +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$shot_table <- DT::renderDataTable({
    df <- filtered_data()
    validate(need(nrow(df) > 0, "Ingen data at vise i tabellen."))
    
    df %>%
      select(
        Sæson = Season,
        Hold = Home_team,
        Spiller = SHORTNAME,
        xG_LogReg = predicted_prob,
        Afstand_m = meters_to_goal,
        Vinkel_deg = shot_angle_geom,
        Mål = SHOTISGOAL,
        På_mål = SHOTONTARGET
      ) %>%
      mutate(
        xG_LogReg = round(xG_LogReg, 3),
        Afstand_m = round(Afstand_m, 1),
        Vinkel_deg = round(Vinkel_deg, 1),
        Mål = ifelse(Mål == "true", "Ja", "Nej"),
        På_mål = ifelse(På_mål == "true", "Ja", "Nej")
      ) %>%
      datatable(
        extensions = "Buttons",
        options = list(
          pageLength = 10,
          lengthMenu = c(5, 10, 25, 50, 100),
          autoWidth = TRUE,
          dom = "Blfrtip",
          buttons = c("copy", "csv", "excel", "print")
        ),
        filter = "top",
        rownames = FALSE
      ) %>%
      formatStyle(
        "Mål",
        target = "row",
        backgroundColor = styleEqual(c("Ja", "Nej"), c("#e63946", "white"))
      )
  })
}

# App
shinyApp(ui = ui, server = server)
