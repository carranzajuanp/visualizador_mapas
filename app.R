library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(DT)

# --- CONFIGURACIÓN Y LECTURA DE DATOS ---
tryCatch({
  datos_sf <- st_read("datos.gpkg", quiet = TRUE)

  # 1. VERIFICAR Y REPROYECTAR EL SRC a WGS 84 (EPSG:4326), si es necesario
  if (st_crs(datos_sf)$epsg != 4326) {
    message(paste("Reproyectando datos desde SRC:", st_crs(datos_sf)$input, "a WGS 84 (EPSG:4326)."))
    datos_sf <- st_transform(datos_sf, crs = 4326)
  }

  # 2. PROCESAMIENTO ESTÁNDAR
  names(datos_sf) <- make.names(names(datos_sf), unique = TRUE)

  datos_limpios <- datos_sf %>%
    st_drop_geometry()

  # IDENTIFICAR VARIABLES NUMÉRICAS
  columnas_numericas <- names(datos_limpios)[sapply(datos_limpios, is.numeric)]

  if (length(columnas_numericas) == 0) {
    warning("No se encontraron columnas de tipo 'numeric'. Intentando detectar columnas convertibles.")
    columnas_numericas <- names(datos_limpios)[sapply(datos_limpios, function(col) {
      !is.factor(col) && !is.character(col) && sum(is.na(as.numeric(as.character(col)))) / length(col) < 0.5
    })]
  }

  if (length(columnas_numericas) == 0) {
    stop("No se encontraron columnas válidas para el análisis numérico. Revise su archivo de datos.")
  }

  # Calcular la caja delimitadora (bbox)
  bbox_data <- st_bbox(datos_sf)

  print("Datos cargados y listos para la visualización.")
}, error = function(e) {
  stop(paste("Error al cargar 'datos.gpkg':", e$message,
             "Asegúrese de que el archivo exista, tenga atributos y geometrías válidas."))
})


# --- INTERFAZ DE USUARIO (UI) ---
ui <- fluidPage(

  fluidRow(
    column(width = 8,
           tags$img(src = "logo-oscds.png",
                    height = 50,
                    style = "margin-right: 15px; margin-top: 5px;"),
           tags$h4("Visualizador Geográfico del Censo 2022", style = "display: inline;")
    ),
    column(width = 4,
           # Columna vacía para alinear
    ),
    title = "Visualizador Geográfico del Censo 2022"
  ),

  fluidRow(
    # Panel principal: Mapa (8 de 12 columnas)
    column(width = 8,
           leafletOutput("mapa_poligonos", height = 650)
    ),

    # Panel lateral derecho: Datos, Estadísticas y Selección de Variable
    column(width = 4,
           h3("🔍 Análisis por Variable"),

           # 1. Desplegable de Selección de Variable
           selectInput("variable_seleccionada",
                       label = "Seleccionar Variable a Visualizar:",
                       choices = columnas_numericas,
                       selected = columnas_numericas[1]
           ),

           hr(),

           # 2. Estadísticas Descriptivas
           h4("Estadísticas de Polígonos VISIBLES:"),
           DTOutput("estadisticas_globales"),

           hr(),

           # 3. Valor para el Polígono Seleccionado
           h4("Valor del Polígono Seleccionado:"),
           DTOutput("valor_seleccionado")
    )
  )
)


# --- SERVER ---
server <- function(input, output, session) {

  # 1. --- Variables Reactivas ---
  id_poligono_clickeado <- reactiveVal(NULL)

  # 2. --- DATOS FILTRADOS POR LA VISTA DEL MAPA ---
  datos_filtrados_por_vista <- reactive({
    bounds <- input$mapa_poligonos_bounds
    req(bounds)

    bbox_sf <- st_bbox(
      c(xmin = bounds$west, ymin = bounds$south, xmax = bounds$east, ymax = bounds$north),
      crs = st_crs(datos_sf)
    ) %>% st_as_sfc()

    indices_visibles <- st_intersects(datos_sf, bbox_sf, sparse = FALSE)[, 1]

    datos_limpios[indices_visibles, ]
  })


  # 3. --- Cálculo de Cuantiles y Paleta de Colores (Reactivo a la Variable) ---
  cuantiles_y_paleta <- reactive({
    req(input$variable_seleccionada)

    var_name <- input$variable_seleccionada

    datos_var <- datos_limpios %>%
      pull(!!sym(var_name)) %>%
      as.numeric()

    datos_var_limpios <- na.omit(datos_var)

    if (length(datos_var_limpios) == 0) {
      warning(paste("La variable", var_name, "no tiene datos numéricos válidos."))
      return(list(paleta = colorBin("gray", domain = c(0, 1), bins = 2), breaks = c(0, 1)))
    }

    breaks_raw <- quantile(datos_var_limpios, probs = seq(0, 1, length.out = 9), na.rm = TRUE, type = 7)
    breaks <- unique(breaks_raw)

    pal <- colorBin("YlOrRd", domain = datos_var_limpios, bins = breaks, na.color = "#808080")

    list(paleta = pal, breaks = breaks)
  })


  # 4. --- Observar Clicks en el Mapa ---
  observeEvent(input$mapa_poligonos_shape_click, {
    click <- input$mapa_poligonos_shape_click

    if (!is.null(click$id)) {
      id_poligono_clickeado(click$id)
    } else {
      id_poligono_clickeado(NULL)
    }
  })


  # 5. --- Renderizado del Mapa (ACTUALIZADO CON OPACIDAD Y VISTA INICIAL) ---
  output$mapa_poligonos <- renderLeaflet({

    paleta_info <- cuantiles_y_paleta()
    var_name <- input$variable_seleccionada

    datos_sf_var <- datos_sf %>%
      pull(!!sym(var_name)) %>%
      as.numeric()

    # Parámetros de la vista inicial (Córdoba capital y alrededores)
    CENTRO_LAT <- -31.42868467554058
    CENTRO_LNG <- -64.18485307559784
    ZOOM_INICIAL <- 11

    # Crear el mapa:
    mapa_leaflet <- leaflet(datos_sf) %>%

      # 1. Crear el panel de alto Z-index para las etiquetas
      addMapPane("labels_pane", zIndex = 650) %>%

      # 2. Fondo Satelital (Base)
      addProviderTiles(providers$Esri.WorldImagery, group = "Satelital") %>%

      # 3. Polígonos (ACTUALIZADA LA OPACIDAD)
      addPolygons(
        fillColor = ~paleta_info$paleta(datos_sf_var),
        weight = 0.5,
        opacity = 1,
        color = "white",
        dashArray = "1",
        fillOpacity = 0.4,
        layerId = ~as.character(1:nrow(datos_sf)),
        highlight = highlightOptions(weight = 3, color = "#666", dashArray = "", fillOpacity = 0.9, bringToFront = TRUE)
      ) %>%

      # 4. Etiquetas (Tope/Overlay)
      addProviderTiles(providers$CartoDB.PositronOnlyLabels, group = "Etiquetas",
                       options = providerTileOptions(pane = "labels_pane")) %>%

      addLegend(
        pal = paleta_info$paleta,
        values = datos_sf_var,
        opacity = 0.7,
        title = var_name,
        position = "bottomright"
      ) %>%

      setView(lng = CENTRO_LNG, lat = CENTRO_LAT, zoom = ZOOM_INICIAL)

    return(mapa_leaflet)
  })


  # 6. --- Renderizar Estadísticas Descriptivas (FILTRADO POR VISTA) ---
  output$estadisticas_globales <- renderDT({
    req(input$variable_seleccionada)

    datos_filtrados <- datos_filtrados_por_vista()

    if (nrow(datos_filtrados) == 0) {
      return(datatable(data.frame(Estadística = "Total de polígonos visibles:", Valor = 0),
                       options = list(dom = 't', pageLength = -1)))
    }

    var_name <- input$variable_seleccionada

    datos_var <- datos_filtrados %>%
      pull(!!sym(var_name)) %>%
      as.numeric()

    stats <- data.frame(
      Estadística = c("Media", "Mediana", "Mínimo", "Máximo", "Desv. Estándar", "N (Visible)"),
      Valor = c(
        mean(datos_var, na.rm = TRUE),
        median(datos_var, na.rm = TRUE),
        min(datos_var, na.rm = TRUE),
        max(datos_var, na.rm = TRUE),
        sd(datos_var, na.rm = TRUE),
        sum(!is.na(datos_var))
      )
    ) %>%
      mutate(Valor = round(Valor, 2))

    datatable(stats,
              rownames = FALSE,
              options = list(dom = 't', pageLength = -1)
    ) %>%
      formatStyle('Estadística', fontWeight = 'bold')
  }, server = FALSE)


  # 7. --- Renderizar Valor del Polígono Seleccionado ---
  output$valor_seleccionado <- renderDT({
    req(id_poligono_clickeado())
    req(input$variable_seleccionada)

    indice_fila <- as.numeric(id_poligono_clickeado())
    var_name <- input$variable_seleccionada

    valor_original <- datos_limpios[indice_fila, var_name, drop = TRUE]

    data_mostrar <- data.frame(
      Campo = var_name,
      Valor = as.character(valor_original)
    )

    if (is.null(id_poligono_clickeado())) {
      data_mostrar <- data.frame(Campo = "", Valor = "Haga click en un polígono...")
    }

    datatable(data_mostrar,
              rownames = FALSE,
              options = list(
                dom = 't',
                pageLength = -1,
                headerCallback = JS("function(thead, data, start, end, display){ $(thead).remove(); }")
              )
    ) %>%
      formatStyle(0, target = 'row', fontWeight = 'bold', fontSize = '150%')
  }, server = FALSE)

}

# Ejecutar la App
shinyApp(ui = ui, server = server)
