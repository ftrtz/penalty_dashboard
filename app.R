### packages
library(readxl)
library(dplyr)
library(heatmaply)

### load data
# own penalties
penalties_own <- read_excel("data/FUT Elfmeter.xlsx", sheet = "Eigene Elfmeter", skip = 2) %>%
  mutate(across(where(is.character), as.factor))
# split x and y axis for shots
penalties_own$Schuss_x <- as.factor(substr(penalties_own$Schussposition, 1,1))
penalties_own$Schuss_y <- as.factor(substr(penalties_own$Schussposition, 2,2))
# opponent penalties
penalties_opp <- read_excel("data/FUT Elfmeter.xlsx", sheet = "Gegnerische Elfmeter", skip = 2) %>%
  mutate(across(where(is.character), as.factor))
# split x and y axis for shots
penalties_opp$Schuss_x <- as.factor(substr(penalties_opp$Schussposition, 1,1))
penalties_opp$Schuss_y <- as.factor(substr(penalties_opp$Schussposition, 2,2))

# ----------------------------------------------- FUNCTIONS -----------------------------------------------------

# ---- filter data based on chosen options ----

filter_all <- function(df, input){
  filterData = reactiveVal(df)
  
  reactive({
    res <- filterData() %>% filter(Spielminute >= input$time[1] & Spielminute <= input$time[2])
    if (input$goal != "Alle"){
      res <- res %>% filter(Tor == input$goal)
    }
    
    if(input$foot != "Alle"){
      res <- res %>% filter(Fuss == input$foot)
    }
    res
  })
}

# ---- plots ----

### heatmap
heat_plot <- function(df, input){

  # table with absolute values
  heat_abs <- df %>% {table(.$Schuss_y, .$Schuss_x)}
  class(heat_abs) <- "matrix"
  
  # table with relative values
  filter_tbl <- df %>% filter(Tor == "ja") %>% {table(.$Schuss_y, .$Schuss_x)}
  heat_rel <- round((filter_tbl / heat_abs) * 100, 2)
  class(heat_rel) <- "matrix"
  
  if (input$abs_rel == "Absolut (Schüsse)"){
    heat_tbl <- heat_abs
  } else {
    heat_tbl <- heat_rel
  }
  
  # plot heatmap
  heatmaply(heat_tbl, Rowv = F, Colv = F, dendrogram = c("none"), hide_colorbar = T,column_text_angle = 0,
            labRow = c("Hoch", "Flach"), labCol = c("Linke Ecke", "Mitte", "Rechte Ecke"), 
            colors = Blues, draw_cellnote = T, cellnote_size = 20, cellnote_textposition = "middle center", plot_method = "plotly") %>% 
    config(displayModeBar = F)
}

### Timeline
timeline_plot <- function(df, input){
  # 0 y-axis to create timeline look
  df$yaxis <- c(rep(0))
  
  plot_ly(data = df, x = ~Spielminute, y = ~yaxis, type = "scatter",
          marker = list(color = toRGB("lightblue"), size = 7, line = list(color = toRGB("black"), width = 1)),
          text = ~paste(df$Spielminute, "'", "\n", df$Schussposition, "\nTor:", df$Tor, "\nSpielstand:", df$Spielstand),
          hoverinfo = "text")  %>% 
    layout(height = 200, margin = c(10,10),
           yaxis = list(title = " ",zerolinewidth  = .5, zerolinecolor = toRGB("gray"), showticklabels = F, showgrid = F),
           xaxis = list(range = c(input$time[1],input$time[2]), zeroline = FALSE, dtick = 10, gridcolor = toRGB("gray"))) %>% 
    config(displayModeBar = F)
}

### selected info
info_text <- function(df){
  HTML(paste0("<p id = 'n_pen'>",nrow(df),"</p>", "<p id = 'subtitle'>Elfmeter</p>"))
}

### pie chart
pie_plot <- function(df, input){
  levels(df$Tor) <- c("Tor", "kein Tor")
  plot_ly(df, labels = ~Tor,  type = 'pie', textposition = 'inside', textinfo = 'label+percent', showlegend = F) %>%
    layout(title = "Torerfolg") %>%  config(displayModeBar = F)
}


# --------------------------------------------------- UI --------------------------------------------------------


ui <- fluidPage(
  
  # custom styles
  tags$style("p#n_pen {font-size: 90px; text-align: center; margin: -30px 0px -20px;}"),
  tags$style("p#subtitle {font-size: 22px; text-align: center;}"),
  tags$style("div.goaldiv {border-left: 5px solid; border-right: 5px solid; border-top: 5px solid;
             text-align: center;}"),
  
 titlePanel("FIFA 19 Elfmeterstatistik"),

   sidebarPanel(width = 3,
     h2("Anzeigeoptionen"),
     selectInput("abs_rel", label = "Anzeige der heatmap", c("Absolut (Schüsse)", "Relativ (Torerfolg in %)"), selected = "Absolut (Schüsse)"),
     h2("Datenauswahl"),
     sliderInput("time", "Spielminute", min = 0, max = 120, c(0,120), step = 5),
     selectInput("goal", label = "Tor", c("Alle", levels(penalties_own$Tor)), selected = "Alle"),
     selectInput("foot", label = "Starker Fuss des Schuetzen", c("Alle", levels(penalties_own$Fuss)), selected = "Alle")
   ),
 
   mainPanel(
     tabsetPanel(
       tabPanel(
         "Eigene Elfmeter",
         column(9,
                br(),
                div(class="goaldiv", plotlyOutput("shots_heat_own")),
                plotlyOutput("timeplot_own")
       ), column(3,
                 radioButtons("detail_own", h3("Detailinfo"), choices = list("Gesamt" = 1, "Auswahl" = 2), selected = 1),
                 hr(),
                 htmlOutput("selection_own"),
                 hr(),
                 plotlyOutput("pie_own")
                 
       )
       ),
       tabPanel(
         "Gegnerische Elfmeter",
         column(9,
                br(),
                div(class="goaldiv",plotlyOutput("shots_heat_opp")),
                plotlyOutput("timeplot_opp")
         ), column(3,
                   radioButtons("detail_opp", h3("Detailinfo"), choices = list("Gesamt" = 1, "Auswahl" = 2), selected = 1),
                   hr(),
                   htmlOutput("selection_opp"),
                   hr(),
                   plotlyOutput("pie_opp")
                   
         )
       )
     )
   )

)

# --------------------------------------------------- SERVER --------------------------------------------------------

server <- function(input, output) {
  
  # ---- filter data based on chosen options ----
  
  pen_own <- filter_all(penalties_own, input)
  
  pen_opp <- filter_all(penalties_opp, input)

  # ----extra information on selected box ----
  ### filter selected data
  # make click event reactive
  s <- reactive({
    event_data("plotly_click")
  })

  pos <- rbind(c("LU", "MU", "RU"), c("LO", "MO", "RO"))
  
  selected_pen_own <- reactive({
    pen_own() %>% filter(Schussposition == pos[s()[[4]], s()[[3]]])
  })
  
  selected_pen_opp <- reactive({
    pen_opp() %>% filter(Schussposition == pos[s()[[4]], s()[[3]]])
  })
  
  
  # ---- Dynamic UI ---- 
  
  ### HEATMAP
  output$shots_heat_own <- renderPlotly({
    heat_plot(pen_own(), input)
    })

  output$shots_heat_opp <- renderPlotly({
    heat_plot(pen_opp(), input)
  })
  
  ### TIMELINE
  output$timeplot_own <- renderPlotly({
    if (input$detail_own == 1 | is.null(s())){
      timeline_plot(pen_own(), input)
    } else {
      timeline_plot(selected_pen_own(), input)
    }
  })
  
  output$timeplot_opp <- renderPlotly({
    if (input$detail_opp == 1 | is.null(s())){
      timeline_plot(pen_opp(), input)
    } else {
      timeline_plot(selected_pen_opp(), input)
    }
  })  
  

  
  ### SELECTED INFORMATION
  # numbers/ text own
  output$selection_own <- renderUI({
    if (input$detail_own == 1){
      info_text(pen_own())
    } else if (is.null( s())){
      paste("Klicken Sie auf die heatmap um detailliertere Infos zu erhalten")
    } else {
      info_text(selected_pen_own())
    }
  })
  
  # pie chart own
  output$pie_own <- renderPlotly({
    if (input$detail_own == 1){
      pie_plot(pen_own())
    } else if (is.null( s())){
    } else {
      pie_plot(selected_pen_own())
    }
    
  })
  
  # numbers/ text opp
  output$selection_opp <- renderUI({
    if (input$detail_opp == 1){
      info_text(pen_opp())
    } else if (is.null( s()))
      paste("Klicken Sie auf die heatmap um detailliertere Infos zu erhalten")
    else {
      info_text(selected_pen_opp())
    }
  })
  
  # pie chart opp
  output$pie_opp <- renderPlotly({
    if (input$detail_opp == 1){
      pie_plot(pen_opp())
    } else if (is.null( s())){
    } else {
      pie_plot(selected_pen_opp())
    }
    
  })
  
}


shinyApp(ui = ui, server = server)

