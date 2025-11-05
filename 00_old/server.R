library(shiny)
library(plotly)
library(shinyjs)

# ---- Helper ----
format_mmss <- function(secs) {
   secs <- as.integer(secs)
   mm <- sprintf("%02d", secs %/% 60)
   ss <- sprintf("%02d", secs %% 60)
   paste0(mm, ":", ss)
}

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

# Zeitbasierte Restzeit; total_sec individuell je Timer
timerServer <- function(id, label, color_hex, total_sec) {
   moduleServer(id, function(input, output, session) {
      
      active    <- reactiveVal(FALSE)            # laeuft der Timer?
      finished  <- reactiveVal(FALSE)            # wurde der Timer beendet?
      startedAt <- reactiveVal(Sys.time())
      tick      <- reactiveTimer(1000, session)
      bg_color  <- lighten_color(color_hex, 0.5)
      
      # Beim Modul-Start: Button in 50%-Farbe
      observe({
         runjs(sprintf("document.getElementById('%s').style.backgroundColor = '%s';",
                       session$ns('reset'), bg_color))
         runjs(sprintf("document.getElementById('%s').style.color = 'white';",
                       session$ns('reset')))
      })
      
      # Start/Reset
      observeEvent(input$reset, {
         startedAt(Sys.time())
         active(TRUE)
         finished(FALSE)
         # Button zurueck auf 50%-Farbe bei neuem Start
         runjs(sprintf("document.getElementById('%s').style.backgroundColor = '%s';",
                       session$ns('reset'), bg_color))
      })
      
      # Restzeit (sekunden) – unterscheide "nicht gestartet" vs. "fertig"
      remaining_sec <- reactive({
         tick()
         
         # Nach Ablauf: 0 halten, bis neu gestartet wird
         if (isTRUE(finished())) return(0L)
         
         # Vor Start: volle Restzeit anzeigen
         if (!isTRUE(active())) return(total_sec)
         
         # Laufend: zeitbasiert herunterzaehlen
         elapsed <- as.numeric(difftime(Sys.time(), startedAt(), units = "secs"))
         left <- ceiling(total_sec - elapsed)
         if (left <= 0) {
            active(FALSE)
            finished(TRUE)  # Zustand auf "fertig" setzen
            return(0L)
         }
         left
      })
      
      # Wenn abgelaufen: Button vollfaerben (100% Deckfarbe) und so lassen
      observe({
         if (isTRUE(finished())) {
            runjs(sprintf("document.getElementById('%s').style.backgroundColor = '%s';",
                          session$ns('reset'), color_hex))
            runjs(sprintf("document.getElementById('%s').style.color = 'white';",
                          session$ns('reset')))
         }
      })
      
      output$plot <- renderPlotly({
         remaining <- remaining_sec()
         progress <- 1 - (remaining / total_sec)
         progress <- max(0, min(1, progress))
         
         # Epsilon gegen Pie-Artefakte; bei "fertig" zweite Scheibe gleichfaerbig machen,
         # damit der Kreis optisch 100% fuellfarbe zeigt.
         eps <- 1e-6
         v_filled <- if (progress <= 0) eps else progress
         v_empty  <- if (progress >= 1) eps else (1 - progress)
         
         cols <- c(color_hex, bg_color)
         if (isTRUE(finished())) {
            cols[2] <- color_hex   # "empty"-Teil ebenfalls in Deckfarbe faerben
            v_filled <- 1 - eps    # praktisch voll
            v_empty  <- eps
         }
         
         df <- data.frame(
            part  = c("filled", "empty"),
            value = c(v_filled, v_empty),
            col   = cols,
            stringsAsFactors = FALSE
         )
         
         center_text <- sprintf("<b>%s</b><br>%s", label, format_mmss(remaining))
         
         plot_ly(
            data = df,
            labels = ~part,
            values = ~value,
            type = "pie",
            sort = FALSE,
            direction = "clockwise",
            rotation = 90,
            hole = 0.7,
            textinfo = "none",
            hoverinfo = "skip",
            marker = list(colors = df$col, line = list(width = 0))
         ) |>
            layout(
               margin = list(l = 0, r = 0, t = 0, b = 0),
               showlegend = FALSE,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               annotations = list(
                  list(x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                       text = center_text, showarrow = FALSE, font = list(size = 16))
               )
            ) |>
            config(
               displayModeBar = FALSE,  # Modebar aus
               displaylogo    = FALSE,  # Plotly-Logo aus
               staticPlot     = TRUE    # Interaktion aus (kein Zoom/Drag/Hover)
            )
         
         
        
      })
      
      outputOptions(output, "plot", suspendWhenHidden = FALSE)
   })
}

server <- function(input, output, session) {
   # Farben (matt)
   col_europa      <- "#3A6EA5" # blau
   col_afrika      <- "#111111" # schwarz
   col_asien       <- "#B91C1C" # rot
   col_americas    <- "#2E7D32" # gruen
   col_ozeanien    <- "#B58900" # gelb
   
   # Europa 5 Minuten, alle anderen 15 Minuten
   timerServer("europa",       "Europa",       col_europa,      total_sec = 5 * 60)
   timerServer("afrika",       "Afrika",       col_afrika,      total_sec = 15 * 60)
   timerServer("asien",        "Asien",        col_asien,       total_sec = 15 * 60)
   timerServer("nordamerika",  "Nordamerika",  col_americas,    total_sec = 15 * 60)
   timerServer("südamerika",   "Südamerika",   col_americas,    total_sec = 15 * 60)
   timerServer("ozeanien",     "Ozeanien",     col_ozeanien,    total_sec = 15 * 60)
}
