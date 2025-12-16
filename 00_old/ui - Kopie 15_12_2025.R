# Skript by jacques.robert@gmx.ch
# Stand: 15.12.2025

library(shiny)
library(plotly)
library(shinyjs)

# Farben (matt)
col_europa      <- "#3A6EA5" # blau
col_afrika      <- "#111111" # schwarz
col_asien       <- "#B91C1C" # rot
col_americas    <- "#2E7D32" # gruen (Nord + Suedamerika)
col_ozeanien    <- "#B58900" # gelb

# 50%-Aufhellung (gleich wie im server)
lighten_color <- function(hex, factor = 0.5) {
   hex <- gsub("#", "", hex)
   if (nchar(hex) == 3) hex <- paste0(rep.int(substr(hex, 1, 1), 2),
                                      rep.int(substr(hex, 2, 2), 2),
                                      rep.int(substr(hex, 3, 3), 2))
   r <- strtoi(substr(hex, 1, 2), 16L)
   g <- strtoi(substr(hex, 3, 4), 16L)
   b <- strtoi(substr(hex, 5, 6), 16L)
   r <- round(r + (255 - r) * factor)
   g <- round(g + (255 - g) * factor)
   b <- round(b + (255 - b) * factor)
   sprintf("#%02X%02X%02X", r, g, b)
}

timerUI <- function(id, title, color_hex) {
   ns <- NS(id)
   light_col <- lighten_color(color_hex, 0.5)
   tagList(
      div(
         style = "display:flex; flex-direction:column; align-items:center; gap:8px;",
         plotlyOutput(ns("plot"), height = "220px", width = "220px"),
         actionButton(
            ns("reset"), "Start",
            class = "btn",
            style = paste0(
               "background-color:", light_col, "; color:white; border:none; width:220px; border-radius:12px;"
            )
         )
      )
   )
}

ui <- fluidPage(
   useShinyjs(),
   tags$head(
      tags$style(HTML("
      .container-fluid { max-width: 1100px; }
    "))
   ),
   
   # Titel zentriert + grosser Abstand danach
   div(h2("LL Raumtimer"), style = "text-align:center;"),
   div(style = "margin-top:100px;"),
   
   # Erste Timerreihe
   fluidRow(
      column(4, timerUI("europa", "Europa", col_europa)),
      column(4, timerUI("afrika", "Afrika", col_afrika)),
      column(4, timerUI("asien", "Asien", col_asien))
   ),
   
   # grosser Abstand zwischen den Reihen
   div(style = "margin-top:50px;"),
   
   # Zweite Timerreihe
   fluidRow(
      column(4, timerUI("nordamerika", "Nordamerika", col_americas)),
      column(4, timerUI("südamerika", "Südamerika", col_americas)),
      column(4, timerUI("ozeanien", "Ozeanien", col_ozeanien))
   )
)
