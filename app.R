# Version Repositorio

# ==============================================================================
# PROYECTO PIÑA 360 - Versión: Repositorio Centralizado (GitHub + Posit Connect)
# ==============================================================================

library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(plotly)
library(DT)
library(stringr)

# --- Configuraciones Globales ---
options(shiny.maxRequestSize = 200 * 1024^2)

# --- Helpers ---
norm_name <- function(x) tolower(str_trim(x))

pick_col <- function(nms, candidates) {
  nms_norm <- norm_name(nms); cand_norm <- norm_name(candidates)
  hit <- match(cand_norm, nms_norm); hit <- hit[!is.na(hit)]
  if (length(hit) == 0) return(NULL)
  nms[hit[1]]
}

safe_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  x2 <- gsub("\\s+|,", "", as.character(x))
  suppressWarnings(as.numeric(x2))
}

fmt_int <- function(x) format(round(na.omit(x)), big.mark = ",", scientific = FALSE)

# --- UI (Interfaz de Usuario) ---
ui <- shiny::fluidPage(
  shiny::tags$head(shiny::tags$style(shiny::HTML("
      .header-container { 
        display: flex; justify-content: space-between; align-items: center; 
        margin: 20px 0; border-bottom: 2px solid #f0f0f0; padding-bottom: 10px; 
      }
      .logo-img { height: 80px; width: auto; }
      .metric-box{ padding: 15px; border-radius: 10px; background: #fff; border: 1px solid #eee; margin-bottom: 15px;}
      .metric-title{ font-size: 11px; color: #7f8c8d; font-weight: bold; text-transform: uppercase; }
      .metric-value{ font-size: 18px; font-weight: 800; color: #2c3e50; }
  "))),
  
  # Header con Logo Corporativo
  shiny::tags$div(class = "header-container", 
                  shiny::tags$h2("Proyecto Piña 360: Auditoría de Cosecha", style="margin:0;"),
                  shiny::tags$img(src = "logo.jpg", class = "logo-img", alt = "Green Xpo Lab")
  ),
  
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      # Selector de Archivos desde la carpeta 'data'
      shiny::selectInput("gpkg_sel", "1. Seleccionar Finca / Auditoría", 
                         choices = NULL), # Se llena en el servidor
      
      shiny::uiOutput("layer_ui"),
      
      shiny::selectInput("bloque_sel", "2. Seleccionar Bloque", choices = "Todos"),
      
      shiny::hr(),
      shiny::tags$b("Parámetros de Auditoría"),
      shiny::sliderInput("densidad_ideal", "Densidad Objetivo (Plantas/ha)", 30000, 80000, 75000, step = 100),
      shiny::sliderInput("perdida", "Pérdida de plantas (%)", 0, 30, 10),
      width = 3
    ),
    
    shiny::mainPanel(
      shiny::fluidRow(
        shiny::column(3, shiny::div(class="metric-box", shiny::div(class="metric-title","Cosecha Ideal"), shiny::div(class="metric-value", style="color:#2563EB", shiny::textOutput("m_total_ideal")))),
        shiny::column(3, shiny::div(class="metric-box", shiny::div(class="metric-title","Cosecha Real"), shiny::div(class="metric-value", style="color:#9333ea", shiny::textOutput("m_total_real")))),
        shiny::column(3, shiny::div(class="metric-box", shiny::div(class="metric-title","Precisión GXL"), shiny::div(class="metric-value", shiny::textOutput("m_acc_gxl")))),
        shiny::column(3, shiny::div(class="metric-box", shiny::div(class="metric-title","Precisión Manual"), shiny::div(class="metric-value", shiny::textOutput("m_acc_man"))))
      ),
      
      shiny::tabsetPanel(
        shiny::tabPanel("Mapa Interactivo", leaflet::leafletOutput("map", height = "600px")),
        shiny::tabPanel("Comparativa Volúmenes", plotly::plotlyOutput("plot_compare", height = "500px")),
        shiny::tabPanel("Análisis Densidad", plotly::plotlyOutput("plot_densidad", height = "500px")),
        shiny::tabPanel("Errores de Estimación", plotly::plotlyOutput("plot_error", height = "500px")),
        shiny::tabPanel("Base de Datos", shiny::div(style="margin-top:20px", DT::DTOutput("tbl")))
      ), width = 9
    )
  )
)

# --- Server (Lógica del Servidor) ---
server <- function(input, output, session) {
  
  # 1. Escanear carpeta 'data' al iniciar
  observar_archivos <- reactive({
    # Buscamos archivos .gpkg en la carpeta 'data'
    archivos <- list.files("data", pattern = "\\.gpkg$", full.names = FALSE)
    if(length(archivos) == 0) return("No se encontraron archivos")
    archivos
  })
  
  # 2. Actualizar el selector de archivos
  observe({
    lista <- observar_archivos()
    updateSelectInput(session, "gpkg_sel", choices = lista)
  })
  
  # 3. Leer el GeoPackage seleccionado
  sf_raw <- reactive({
    req(input$gpkg_sel, input$layer)
    req(input$gpkg_sel != "No se encontraron archivos")
    ruta <- file.path("data", input$gpkg_sel)
    sf::st_read(ruta, layer = input$layer, quiet = TRUE)
  })
  
  # 4. Generar UI dinámica para capas
  output$layer_ui <- renderUI({
    req(input$gpkg_sel)
    req(input$gpkg_sel != "No se encontraron archivos")
    ruta <- file.path("data", input$gpkg_sel)
    capas <- sf::st_layers(ruta)$name
    shiny::selectInput("layer", "Capa a cargar", choices = capas)
  })
  
  # 5. Procesamiento de Datos (Lógica Piña 360)
  data_ready_all <- reactive({
    df <- sf_raw()
    nms <- names(df)
    
    col_nombre     <- pick_col(nms, c("nombre", "name", "bloque", "block"))
    col_area       <- pick_col(nms, c("area", "hectareas", "ha"))
    col_plantas    <- pick_col(nms, c("numpoints", "plantas", "plants"))
    col_manual     <- pick_col(nms, c("estimacion", "manual", "est_manual"))
    col_gxl        <- pick_col(nms, c("gxl", "est_gxl"))
    col_real       <- pick_col(nms, c("cosecha", "real", "harvest"))
    col_dens_real  <- pick_col(nms, c("densidad", "density", "dens"))
    
    res <- df %>%
      dplyr::mutate(
        Bloque = as.character(.data[[col_nombre]]),
        Area_Ha = safe_numeric(.data[[col_area]]),
        Plantas = if(!is.null(col_plantas)) safe_numeric(.data[[col_plantas]]) else 0,
        Manual  = if(!is.null(col_manual))  safe_numeric(.data[[col_manual]])  else 0,
        GXL     = if(!is.null(col_gxl))     safe_numeric(.data[[col_gxl]])     else 0,
        Real    = if(!is.null(col_real))    safe_numeric(.data[[col_real]])    else 0,
        Dens_Real  = if(!is.null(col_dens_real)) safe_numeric(.data[[col_dens_real]]) else 0,
        Dens_Ideal = input$densidad_ideal,
        Diff_Dens  = abs((Dens_Real - Dens_Ideal) / Dens_Ideal),
        Color_Dens = ifelse(Diff_Dens > 0.15, "#EF4444", "#10B981"),
        Ideal   = Area_Ha * input$densidad_ideal * (1 - (input$perdida/100)),
        Err_GXL = round(((GXL - Real) / Real) * 100, 2),
        Err_Man = round(((Manual - Real) / Real) * 100, 2)
      ) %>%
      sf::st_transform(4326)
    
    res <- res %>% 
      mutate(num_id = as.numeric(str_extract(Bloque, "\\d+"))) %>% 
      arrange(num_id) %>%
      mutate(Bloque = factor(Bloque, levels = unique(Bloque)))
    
    res
  })
  
  observe({
    req(input$gpkg_sel)
    df <- data_ready_all()
    shiny::updateSelectInput(session, "bloque_sel", choices = c("Todos", levels(df$Bloque)))
  })
  
  data_ready <- reactive({
    df <- data_ready_all()
    if (input$bloque_sel != "Todos") df <- df %>% dplyr::filter(Bloque == input$bloque_sel)
    df
  })
  
  # --- Outputs Visuales ---
  output$m_total_ideal <- renderText({ req(input$gpkg_sel); fmt_int(sum(data_ready()$Ideal, na.rm=T)) })
  output$m_total_real  <- renderText({ req(input$gpkg_sel); fmt_int(sum(data_ready()$Real, na.rm=T)) })
  output$m_acc_gxl <- renderText({
    req(input$gpkg_sel); r <- sum(data_ready()$Real, na.rm=T); g <- sum(data_ready()$GXL, na.rm=T)
    if(is.na(r) || r==0) return("0%"); paste0(round(100 - abs((g-r)/r*100), 1), "%")
  })
  output$m_acc_man <- renderText({
    req(input$gpkg_sel); r <- sum(data_ready()$Real, na.rm=T); m <- sum(data_ready()$Manual, na.rm=T)
    if(is.na(r) || r==0) return("0%"); paste0(round(100 - abs((m-r)/r*100), 1), "%")
  })
  
  output$map <- leaflet::renderLeaflet({
    if (!isTruthy(input$gpkg_sel) || input$gpkg_sel == "No se encontraron archivos") {
      return(leaflet::leaflet() %>% leaflet::addProviderTiles("Esri.WorldImagery") %>% leaflet::setView(-84.0, 10.0, 7))
    }
    df <- data_ready()
    pal <- leaflet::colorNumeric(palette = "RdYlGn", domain = c(-30, 30))
    leaflet::leaflet(df) %>%
      leaflet::addProviderTiles("Esri.WorldImagery") %>%
      leaflet::addPolygons(
        fillColor = ~pal(Err_GXL), fillOpacity = 0.7, weight = 2, color = "white",
        popup = ~paste0(
          "<div style='font-family: Arial; min-width: 200px;'>",
          "<h4>Bloque: ", Bloque, "</h4>",
          "<b>Cosecha Ideal:</b> ", fmt_int(Ideal), "<br>",
          "<b>Cosecha Real:</b> ", fmt_int(Real), "<hr>",
          "<b>Est. Manual:</b> ", fmt_int(Manual), " (", Err_Man, "%)<br>",
          "<b>Est. GXL:</b> ", fmt_int(GXL), " (", Err_GXL, "%)",
          "</div>"
        )
      ) %>%
      leaflet::addLegend(pal = pal, values = c(-30, 30), title = "% Error GXL", position = "bottomright")
  })
  
  output$plot_densidad <- plotly::renderPlotly({
    req(input$gpkg_sel)
    df <- data_ready() %>% sf::st_drop_geometry()
    plotly::plot_ly(df, x = ~Bloque) %>%
      plotly::add_bars(y = ~Dens_Ideal, name = "Densidad Ideal", marker = list(color = '#1E293B')) %>%
      plotly::add_bars(y = ~Dens_Real, name = "Densidad Real", marker = list(color = ~Color_Dens)) %>%
      plotly::layout(title = list(text = "Análisis de Densidad (Real vs Ideal)", y = 0.98),
                     barmode = 'group', margin = list(t = 100, b = 80), legend = list(orientation = 'h', y = -0.2))
  })
  
  output$plot_compare <- plotly::renderPlotly({
    req(input$gpkg_sel)
    df <- data_ready() %>% sf::st_drop_geometry()
    plotly::plot_ly(df, x = ~Bloque) %>%
      plotly::add_bars(y = ~Ideal, name = "Ideal", marker = list(color = '#2563EB')) %>%
      plotly::add_bars(y = ~Manual, name = "Manual", marker = list(color = '#F97316')) %>%
      plotly::add_bars(y = ~GXL, name = "GXL", marker = list(color = '#FACC15')) %>%
      plotly::add_bars(y = ~Real, name = "Real", marker = list(color = '#9333ea')) %>%
      plotly::layout(title = list(text = "Comparativa de Volúmenes", y = 0.98),
                     barmode = 'group', margin = list(t = 100, b = 80), legend = list(orientation = 'h', y = -0.2))
  })
  
  output$plot_error <- plotly::renderPlotly({
    req(input$gpkg_sel)
    df <- data_ready() %>% sf::st_drop_geometry()
    plotly::plot_ly(df, x = ~Bloque) %>%
      plotly::add_bars(y = ~Err_GXL, name = "% Error GXL", marker = list(color = '#FACC15')) %>%
      plotly::add_bars(y = ~Err_Man, name = "% Error Manual", marker = list(color = '#F97316')) %>%
      plotly::layout(title = list(text = "Análisis de Desviación (% de Error)", y = 0.98),
                     barmode = 'group', margin = list(t = 100, b = 80), legend = list(orientation = 'h', y = -0.2))
  })
  
  output$tbl <- DT::renderDT({
    req(input$gpkg_sel)
    data_ready() %>% sf::st_drop_geometry() %>%
      dplyr::select(Bloque, Dens_Real, Dens_Ideal, Ideal, Manual, GXL, Real, `Err GXL %` = Err_GXL, `Err Man %` = Err_Man) %>%
      DT::datatable(extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf'), scrollX = TRUE, pageLength = 15)) %>%
      DT::formatRound(columns = c('Dens_Real', 'Dens_Ideal', 'Ideal', 'Manual', 'GXL', 'Real'), digits = 0)
  })
}

shinyApp(ui, server)