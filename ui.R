# Skript by jacques.robert@gmx.ch
# Stand: 16.12.2025

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


timerUI <- function(id, title, color_hex, size = 220, plot_offset_y = 0) {
   ns <- NS(id)
   light_col <- lighten_color(color_hex, 0.5)
   
   tagList(
      div(
         style = paste0(
            "display:flex; flex-direction:column; align-items:center; gap:8px; width:", size, "px;"
         ),
         
         # Plot (nur dieser wird vertikal verschoben)
         div(
            style = paste0("transform: translateY(", plot_offset_y, "px);"),
            plotlyOutput(ns("plot"),
                         height = paste0(size, "px"),
                         width  = paste0(size, "px"))
         ),
         
         # Button bleibt unangetastet
         actionButton(
            ns("reset"), "Start",
            class = "btn",
            style = paste0(
               "background-color:", light_col,
               "; color:white; border:none; width:", size,
               "px; border-radius:12px;"
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

      /* Fixe Spalten fuer identische horizontale Ausrichtung in beiden Reihen */
      .timer-grid-row {
        display: grid;
        /* 1. Spalte: 2x Europa (110px) + Gap (12px) = 252px */
        grid-template-columns: 230px 230px 230px;
        justify-content: space-between;
        align-items: flex-end;
      }

      .timer-europa-wrap {
        display: flex;
        gap: 12px;
      }
    "))
   ),
   
   # Titel zentriert + grosser Abstand danach
   div(h2("LL-Timer"), style = "text-align:center;"),
   div(style = "margin-top:100px;"),
   
   # Erste Timerreihe (Spaltenlayout: identisch zur 2. Reihe)
   div(
      class = "timer-grid-row",
      
      # Europa (2 kleine Timer nebeneinander, kleiner als Afrika)
     
      div(
         class = "timer-europa-wrap",
         timerUI("europa1", "Europa 1", col_europa, size = 110, plot_offset_y = -55),
         timerUI("europa2", "Europa 2", col_europa, size = 110, plot_offset_y = -55)
      ),
      

      # Afrika (Referenzhoehe)
      timerUI("afrika", "Afrika", col_afrika, size = 220),
      
      # Asien
      timerUI("asien", "Asien", col_asien, size = 220)
   ),
   
   # grosser Abstand zwischen den Reihen
   div(style = "margin-top:50px;"),
   
   # Zweite Timerreihe (gleiche Spalten wie oben: Suedamerika unter Afrika, Ozeanien unter Asien)
   div(
      class = "timer-grid-row",
      timerUI("nordamerika", "Nordamerika", col_americas, size = 220),
      timerUI("südamerika", "Südamerika", col_americas, size = 220),
      timerUI("ozeanien", "Ozeanien", col_ozeanien, size = 220)
   )
)
