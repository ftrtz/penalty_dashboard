library(heatmaply)
library(readxl)
penalties_own <- read_excel("data/FUT Elfmeter.xlsx", sheet = "Eigene Elfmeter", skip = 2)
penalties_own$yaxis <- c(rep(0))
penalties_opp <- read_excel("data/FUT Elfmeter.xlsx", sheet = "Gegnerische Elfmeter", skip = 2)
penalties_opp$yaxis <- c(rep(0))



ui <- fluidPage(
  titlePanel("Fabians FIFA 19 Elfmeterstatistik"),
  sidebarLayout(
    sidebarPanel(
      h2("Anzeigeoptionen"),
      selectInput("abs_rel", label = "Anzeige der heatmap", c("Absolut (Schuesse)", "Relativ (Torerfolg in %)"), selected = "Absolut (Schuesse)"),
      selectInput("detail", label = "Totale oder Detaillierte Darstellung", c("Total", "Detail"), selected = "Total"),
      h2("Datenauswahl"),
      sliderInput("time", "Spielminute", min = 0, max = 120, c(0,120), step = 5),
      selectInput("goal", label = "Tor", c("Alle", unique(penalties_own$Tor)), selected = "Alle"),
      selectInput("foot", label = "Starker Fuss des Schuetzen", c("Alle", unique(penalties_own$Fuss)), selected = "Alle")
         ),
    mainPanel(
      tabsetPanel(
        tabPanel("Eigene Elfmeter",
                 br(),
                 plotlyOutput("shots_heat_own"),
                 htmlOutput("selection_own"),
                 plotlyOutput("timeplot_own")
        ),
      tabPanel("Gegnerische Elfmeter",
               br(),
               plotlyOutput("shots_heat_opp"),
               htmlOutput("selection_opp"),
               plotlyOutput("timeplot_opp")
               )
      )
    )
  )
)

server <- function(input, output) {

  shots1 <- reactive({
    if (input$foot == "Alle" & input$goal == "Alle"){
      subset(penalties_own, Spielminute >= input$time[1] & Spielminute <= input$time[2])
    } else if (input$foot == "Alle" & input$goal != "Alle"){
      subset(penalties_own, Tor == input$goal & Spielminute >= input$time[1] & Spielminute <= input$time[2])
    } else if (input$goal == "Alle" & input$foot != "Alle"){
      subset(penalties_own, Fuss == input$foot & Spielminute >= input$time[1] & Spielminute <= input$time[2])
    }else {
      subset(penalties_own, Fuss == input$foot & Tor == input$goal & Spielminute >= input$time[1] & Spielminute <= input$time[2])
    }
  })
  
  shots2 <- reactive({
    if (input$foot == "Alle" & input$goal == "Alle"){
      subset(penalties_opp, Spielminute >= input$time[1] & Spielminute <= input$time[2])
    } else if (input$foot == "Alle" & input$goal != "Alle"){
      subset(penalties_opp, Tor == input$goal & Spielminute >= input$time[1] & Spielminute <= input$time[2])
    } else if (input$goal == "Alle" & input$foot != "Alle"){
      subset(penalties_opp, Fuss == input$foot & Spielminute >= input$time[1] & Spielminute <= input$time[2])
    }else {
      subset(penalties_opp, Fuss == input$foot & Tor == input$goal & Spielminute >= input$time[1] & Spielminute <= input$time[2])
    }
  })


s <- reactive({
  event_data("plotly_click")
})

selected_shots1 <- reactive({
  if (s()[3]== 1 & s()[4]== 1)  {
    subset(shots1(), Schussposition == "LU")
  } else if (s()[3]== 1 & s()[4]== 2)  {
    subset(shots1(), Schussposition == "LO")
  } else if (s()[3]== 2 & s()[4]== 1)  {
    subset(shots1(), Schussposition == "MU")
  } else if (s()[3]== 2 & s()[4]== 2)  {
    subset(shots1(), Schussposition == "MO")
  } else if (s()[3]== 3 & s()[4]== 1)  {
    subset(shots1(), Schussposition == "RU")
  } else if (s()[3]== 3 & s()[4]== 2)  {
    subset(shots1(), Schussposition == "RO")
  }
})

selected_shots2 <- reactive({
  if (s()[3]== 1 & s()[4]== 1)  {
    subset(shots2(), Schussposition == "LU")
  } else if (s()[3]== 1 & s()[4]== 2)  {
    subset(shots2(), Schussposition == "LO")
  } else if (s()[3]== 2 & s()[4]== 1)  {
    subset(shots2(), Schussposition == "MU")
  } else if (s()[3]== 2 & s()[4]== 2)  {
    subset(shots2(), Schussposition == "MO")
  } else if (s()[3]== 3 & s()[4]== 1)  {
    subset(shots2(), Schussposition == "RU")
  } else if (s()[3]== 3 & s()[4]== 2)  {
    subset(shots2(), Schussposition == "RO")
  }
})


output$selection_own <- renderUI({
  if (input$detail == "Total"){
    str1_total <-  paste(nrow(shots1()), "Elfmeter")
    str2_total <-  paste("Davon mit Torerfolg:", nrow(subset(shots1(), Tor == "ja")), "(", round(nrow(subset(shots1(), Tor == "ja"))/nrow(shots1())*100,2), "%)")
    HTML(paste("Gesamt", "<br>", str1_total, "<br/>", str2_total))
  } else if (is.null( s()))
    paste("Klicken Sie auf die heatmap um detailliertere Infos zu erhalten")
  else {
    str1_select <-  paste(nrow(selected_shots1()), "Elfmeter")
    str2_select <-  paste("Davon mit Torerfolg:", nrow(subset(selected_shots1(), Tor == "ja")), "(", round(nrow(subset(selected_shots1(), Tor == "ja"))/nrow(selected_shots1())*100,2), "%)")
    HTML(paste(unique(selected_shots1()$Schussposition), "<br>", str1_select, "<br>", str2_select))
  }
})

output$selection_opp <- renderUI({
  if (input$detail == "Total"){
    str1_total <-  paste(nrow(shots2()), "Elfmeter")
    str2_total <-  paste("Davon mit Torerfolg:", nrow(subset(shots2(), Tor == "ja")), "(", round(nrow(subset(shots2(), Tor == "ja"))/nrow(shots2())*100,2), "%)")
    HTML(paste("Gesamt", "<br>", str1_total, "<br/>", str2_total))
  } else if (is.null( s()))
    paste("Klicken Sie auf die heatmap um detailliertere Infos zu erhalten")
  else {
    str1_select <-  paste(nrow(selected_shots2()), "Elfmeter")
    str2_select <-  paste("Davon mit Torerfolg:", nrow(subset(selected_shots2(), Tor == "ja")), "(", round(nrow(subset(selected_shots2(), Tor == "ja"))/nrow(selected_shots2())*100,2), "%)")
    HTML(paste(unique(selected_shots2()$Schussposition), "<br>", str1_select, "<br>", str2_select))
  }
})


output$timeplot_own <- renderPlotly({
  if (input$detail == "Total" | is.null(s())){
    plot_ly(data = shots1(), x = ~Spielminute, y = ~yaxis, type = "scatter",
            marker = list(color = toRGB("lightblue"), size = 7, line = list(color = toRGB("black"), width = 1)),
            text = ~paste(shots1()$Spielminute, "'", "\n", shots1()$Schussposition, "\nTor:", shots1()$Tor, "\nSpielstand:", shots1()$Spielstand),
            hoverinfo = "text")  %>% 
      layout(title = "Darstellung der einzelnen Elfmeter",
             height = 200, margin = c(10,10),
             yaxis = list(title = " ",zerolinewidth  = .5, zerolinecolor = toRGB("gray"), showticklabels = F, showgrid = F),
             xaxis = list(range = c(input$time[1],input$time[2]), zeroline = FALSE, dtick = 10, gridcolor = toRGB("gray")))
  } else {
    plot_ly(data = selected_shots1(), x = ~Spielminute, y = ~yaxis, type = "scatter",
            marker = list(color = toRGB("lightblue"), size = 7, line = list(color = toRGB("black"), width = 1)),
            text = ~paste(selected_shots1()$Spielminute, "'", "\n", selected_shots1()$Schussposition, "\nTor:", selected_shots1()$Tor, "\nSpielstand:", selected_shots1()$Spielstand),
            hoverinfo = "text")  %>% 
      layout(title = "Darstellung der einzelnen Elfmeter",
             height = 200, margin = c(10,10),
             yaxis = list(title = " ",zerolinewidth  = .5, zerolinecolor = toRGB("gray"), showticklabels = F, showgrid = F),
             xaxis = list(range = c(input$time[1],input$time[2]), zeroline = FALSE, dtick = 10, gridcolor = toRGB("gray")))
  }
})

output$timeplot_opp <- renderPlotly({
  if (input$detail == "Total" | is.null(s())){
    plot_ly(data = shots2(), x = ~Spielminute, y = ~yaxis, type = "scatter",
            marker = list(color = toRGB("lightblue"), size = 7, line = list(color = toRGB("black"), width = 1)),
            text = ~paste(shots2()$Spielminute, "'", "\n", shots2()$Schussposition, "\nTor:", shots2()$Tor, "\nSpielstand:", shots2()$Spielstand),
            hoverinfo = "text")  %>% 
      layout(title = "Darstellung der einzelnen Elfmeter",
             height = 200, margin = c(10,10),
             yaxis = list(title = " ",zerolinewidth  = .5, zerolinecolor = toRGB("gray"), showticklabels = F, showgrid = F),
             xaxis = list(range = c(input$time[1],input$time[2]), zeroline = FALSE, dtick = 10, gridcolor = toRGB("gray")))
  } else {
    plot_ly(data = selected_shots2(), x = ~Spielminute, y = ~yaxis, type = "scatter",
            marker = list(color = toRGB("lightblue"), size = 7, line = list(color = toRGB("black"), width = 1)),
            text = ~paste(selected_shots2()$Spielminute, "'", "\n", selected_shots2()$Schussposition, "\nTor:", selected_shots2()$Tor, "\nSpielstand:", selected_shots2()$Spielstand),
            hoverinfo = "text")  %>% 
      layout(title = "Darstellung der einzelnen Elfmeter",
             height = 200, margin = c(10,10),
             yaxis = list(title = " ",zerolinewidth  = .5, zerolinecolor = toRGB("gray"), showticklabels = F, showgrid = F),
             xaxis = list(range = c(input$time[1],input$time[2]), zeroline = FALSE, dtick = 10, gridcolor = toRGB("gray")))
  }
})  

  output$shots_heat_own <- renderPlotly({
    heat1_abs <- rbind(c(length(which(shots1()$Schussposition == "LO")),
                         length(which(shots1()$Schussposition == "MO")),
                         length(which(shots1()$Schussposition == "RO"))),
                       c(length(which(shots1()$Schussposition == "LU")),
                         length(which(shots1()$Schussposition == "MU")),
                         length(which(shots1()$Schussposition == "RU"))))
    
    heat1_rel <- rbind(c(round((length(which(shots1()$Schussposition == "LO" & shots1()$Tor == "ja"))/length(which(shots1()$Schussposition == "LO"))*100),2),
                         round((length(which(shots1()$Schussposition == "MO" & shots1()$Tor == "ja"))/length(which(shots1()$Schussposition == "MO"))*100),2),
                         round((length(which(shots1()$Schussposition == "RO" & shots1()$Tor == "ja"))/length(which(shots1()$Schussposition == "RO"))*100),2)),
                       c(round((length(which(shots1()$Schussposition == "LU" & shots1()$Tor == "ja"))/length(which(shots1()$Schussposition == "LU"))*100),2),
                         round((length(which(shots1()$Schussposition == "MU" & shots1()$Tor == "ja"))/length(which(shots1()$Schussposition == "MU"))*100),2),
                         round((length(which(shots1()$Schussposition == "RU" & shots1()$Tor == "ja"))/length(which(shots1()$Schussposition == "RU"))*100),2)))
    
    if (input$abs_rel == "Absolut (Schuesse)"){
      heatmaply(heat1_abs, Rowv = F, Colv = F, dendrogram = c("none"), hide_colorbar = T,column_text_angle = 0,
                labRow = c("Hoch", "Flach"), labCol = c("Linke Ecke", "Mitte", "Rechte Ecke"), 
                colors = Blues, draw_cellnote = T, cellnote_size = 20, cellnote_textposition = "middle center", plot_method = "plotly")
    } else {
      heatmaply(heat1_rel, Rowv = F, Colv = F, dendrogram = c("none"), hide_colorbar = T,column_text_angle = 0,
                labRow = c("Hoch", "Flach"), labCol = c("Linke Ecke", "Mitte", "Rechte Ecke"), 
                colors = Blues, cellnote = cbind(paste(heat1_rel,"%")[1:2], paste(heat1_rel,"%")[3:4], paste(heat1_rel,"%")[5:6]), draw_cellnote = T, cellnote_size = 20, cellnote_textposition = "middle center", plot_method = "plotly")
    }

  })
  
  output$shots_heat_opp <- renderPlotly({
    heat2_abs <- rbind(c(length(which(shots2()$Schussposition == "LO")),
                         length(which(shots2()$Schussposition == "MO")),
                         length(which(shots2()$Schussposition == "RO"))),
                       c(length(which(shots2()$Schussposition == "LU")),
                         length(which(shots2()$Schussposition == "MU")),
                         length(which(shots2()$Schussposition == "RU"))))
    
    heat2_rel <- rbind(c(round((length(which(shots2()$Schussposition == "LO" & shots2()$Tor == "ja"))/length(which(shots2()$Schussposition == "LO"))*100),2),
                         round((length(which(shots2()$Schussposition == "MO" & shots2()$Tor == "ja"))/length(which(shots2()$Schussposition == "MO"))*100),2),
                         round((length(which(shots2()$Schussposition == "RO" & shots2()$Tor == "ja"))/length(which(shots2()$Schussposition == "RO"))*100),2)),
                       c(round((length(which(shots2()$Schussposition == "LU" & shots2()$Tor == "ja"))/length(which(shots2()$Schussposition == "LU"))*100),2),
                         round((length(which(shots2()$Schussposition == "MU" & shots2()$Tor == "ja"))/length(which(shots2()$Schussposition == "MU"))*100),2),
                         round((length(which(shots2()$Schussposition == "RU" & shots2()$Tor == "ja"))/length(which(shots2()$Schussposition == "RU"))*100),2)))

    if (input$abs_rel == "Absolut (Schuesse)"){
      heatmaply(heat2_abs, Rowv = F, Colv = F, dendrogram = c("none"), hide_colorbar = T,column_text_angle = 0,
                labRow = c("Hoch", "Flach"), labCol = c("Linke Ecke", "Mitte", "Rechte Ecke"), 
                colors = Blues, draw_cellnote = T, cellnote_size = 20, cellnote_textposition = "middle center", plot_method = "plotly")
    } else {
      heatmaply(heat2_rel, Rowv = F, Colv = F, dendrogram = c("none"), hide_colorbar = T,column_text_angle = 0,
                labRow = c("Hoch", "Flach"), labCol = c("Linke Ecke", "Mitte", "Rechte Ecke"), 
                colors = Blues, cellnote = cbind(paste(heat2_rel,"%")[1:2], paste(heat2_rel,"%")[3:4], paste(heat2_rel,"%")[5:6]), draw_cellnote = T, cellnote_size = 20, cellnote_textposition = "middle center", plot_method = "plotly")
    }
  })


}

shinyApp(ui = ui, server = server)


