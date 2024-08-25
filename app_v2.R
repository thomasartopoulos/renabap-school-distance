library(shiny)
library(leaflet)
library(ggplot2)
library(DT)
library(sf)
library(data.table)
library(dplyr)
library(waiter)

# UI function
ui <- fluidPage(
  use_waiter(),
  titlePanel("Distancia de barrios populares (RENABAP) a escuelas"),
  tabsetPanel(
    tabPanel("Descripción",
             fluidRow(
               column(12,
                      h3("Descripción del Objetivo de la Aplicación"),
                      p("La aplicación tiene como objetivo analizar las distancias entre barrios y diferentes tipos de escuelas dentro del Área Metropolitana de Buenos Aires (AMBA). Utilizando datos geoespaciales de RENABAP y de datos.gob.ar, la aplicación permite a los usuarios visualizar y analizar la proximidad de barrios a escuelas, proporcionando información detallada sobre las distancias a los distintos tipos de instituciones educativas. En la pestaña Distancia por barrio, tiene que seleccionar el barrio para poder visualizar las distanciasexpresadas en metros. En el caso de distancia por localidad, cuenta con un filtro para poder conocer la distancia promedio por localidad hacia cada tipo de escuela."),
                      h4("Preguntas que Busca Responder"),
                      tags$ol(
                        tags$li(
                          tags$strong("¿Cuáles son las distancias más cortas desde un barrio seleccionado hasta las diferentes categorías de escuelas?"),
                          p("La aplicación permite identificar cuál es la escuela más cercana para cada tipo (nivel y modalidad) y cuánto mide la distancia desde el centro del barrio seleccionado hasta estas escuelas.")
                        ),
                        tags$li(
                          tags$strong("¿Cómo se distribuyen las distancias a las escuelas en un barrio específico?"),
                          p("A través de gráficos de barras, se pueden observar las distancias a las distintas escuelas para entender mejor la accesibilidad educativa de los residentes en ese barrio.")
                        ),
                        tags$li(
                          tags$strong("¿Cuál es la distribución geográfica de las escuelas dentro de un partido o comuna?"),
                          p("La aplicación ofrece la posibilidad de filtrar por partido/comuna y visualizar un mapa que muestra tanto los barrios como las escuelas, destacando la proximidad entre ellos.")
                        ),
                        tags$li(
                          tags$strong("¿Qué barrios tienen mayores o menores accesos a diferentes tipos de escuelas?"),
                          p("Comparando las distancias promedio a las escuelas por tipo, la aplicación permite entender qué barrios están mejor o peor ubicados en relación con las opciones educativas disponibles.")
                        )
                      )
               )
             )
    ),
    tabPanel("Distancia por barrio",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("barrio_info")
               ),
               mainPanel(
                 leafletOutput("map"),
                 plotOutput("distance_plot"),
                 dataTableOutput("distance_table")
               )
             )
    ),
    tabPanel("Promedio de distancia por localidad",
             sidebarLayout(
               sidebarPanel(
                 selectInput("party_dropdown", "Seleccione el partido / comuna",
                             choices = NULL,
                             selected = NULL)
               ),
               mainPanel(
                 leafletOutput("partido_map", height = "400px"),
                 h3("Promedio de distancia a tipo de escuela"),
                 dataTableOutput("filtered_results_table")
               )
             )
    )
  )
)

# Server function
server <- function(input, output, session) {
  
  # Create a waiter loading screen
  w <- Waiter$new(html = spin_flower(), color = transparent(.5))
  
  # Show the loading screen
  w$show()
  
  # Load your datasets
  barrios_inside_amba <- st_read("output/barrios_inside_amba.geojson")
  escuelas_inside_amba <- st_read("output/escuelas_inside_amba.geojson")
  results_dt <- fread("output/distances_to_schools.csv")
  amba <- st_read("http://ideconurbano.ungs.edu.ar/geoserver/ows?service=WFS&version=1.0.0&request=GetFeature&typename=geonode%3AAMBA&outputFormat=json&srs=EPSG%3A4326&srsName=EPSG%3A4326")

  # Join amba geometry to results_dt
  amba_sf <- st_as_sf(amba)
  results_dt_sf <- results_dt %>%
    left_join(amba_sf %>% select(nam, geometry), by = c("partido_comuna" = "nam")) %>%
    st_as_sf()

  # Reproject to EPSG:4326 for Leaflet
  barrios_inside_amba_wgs84 <- st_transform(barrios_inside_amba, crs = 4326)
  escuelas_inside_amba_wgs84 <- st_transform(escuelas_inside_amba, crs = 4326)

  # Update the choices for the party dropdown
  updateSelectInput(session, "party_dropdown",
                    choices = unique(results_dt$partido_comuna),
                    selected = unique(results_dt$partido_comuna)[1])

  # Hide the loading screen
  w$hide()
  
  # Initialize the Leaflet map
  output$map <- renderLeaflet({
    leaflet(barrios_inside_amba_wgs84) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~"blue", fillOpacity = 0.5, color = "#BDBDC3",
        weight = 1, layerId = ~id_renabap
      )
  })
  
  # Observe map click event
  observeEvent(input$map_shape_click, {
    selected_id <- input$map_shape_click$id
    
    if (!is.null(selected_id)) {
      update_barrio_info(selected_id)
    }
  })

  # Function to update barrio information and plot based on the selected marker
  update_barrio_info <- function(selected_id_renabap) {
    selected_id_renabap <- as.character(selected_id_renabap)
    
    # Filter results for the selected id_renabap
    selected_data <- results_dt[id_renabap == selected_id_renabap]

    if (nrow(selected_data) == 0) {
      output$barrio_info <- renderUI({
        wellPanel(
          h4("No hay datos disponibles para este barrio.")
        )
      })
      output$distance_plot <- renderPlot({
        ggplot() + theme_void() + labs(title = "Datos no disponibles")
      })
      output$distance_table <- renderDataTable({
        datatable(data.frame(Mensaje = "No hay datos disponibles"))
      })
    } else {
      output$barrio_info <- renderUI({
        wellPanel(
          h4(paste("ID de Barrio Seleccionado:", selected_id_renabap)),
          p(paste("Nombre del Barrio:", selected_data$nombre_barrio[1]))
        )
      })
      
      # Plot distances in a bar chart
      output$distance_plot <- renderPlot({
        ggplot(selected_data, aes(x = school_type, y = distance, fill = school_type)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(
            title = paste("Distancias más Cortas a Tipos de Escuela para", selected_id_renabap),
            x = "Tipo de Escuela",
            y = "Distancia (metros)"
          ) +
          guides(fill = "none")
      })

      # Render data table with school types and distances
      output$distance_table <- renderDataTable({
        datatable(selected_data[, .(Tipo_Escuela = school_type, Distancia = distance)])
      })

      # Highlight selected barrio and draw lines to nearest schools
      leafletProxy("map") %>%
        addPolygons(
          data = barrios_inside_amba_wgs84[barrios_inside_amba_wgs84$id_renabap == selected_id_renabap, ],
          fillColor = "red", fillOpacity = 0.7, color = "red", weight = 2
        )

      # Loop over each school type to draw lines
      for (i in 1:nrow(selected_data)) {
        school_type <- selected_data$school_type[i]
        modalidad <- unlist(strsplit(school_type, "_"))[1]
        nivel <- unlist(strsplit(school_type, "_"))[2]
        
        nearest_school <- escuelas_inside_amba_wgs84[
          escuelas_inside_amba_wgs84$modalidad == modalidad &
            escuelas_inside_amba_wgs84$nivel == nivel, ]
        
        distances <- st_distance(st_transform(
          barrios_inside_amba_wgs84[barrios_inside_amba_wgs84$id_renabap == selected_id_renabap, ], 
          crs = st_crs(nearest_school)), nearest_school)
        nearest_school <- nearest_school[which.min(distances), ]
        
        selected_barrio_centroid <- st_centroid(st_geometry(barrios_inside_amba_wgs84[barrios_inside_amba_wgs84$id_renabap == selected_id_renabap, ]))
        nearest_school_centroid <- st_centroid(st_geometry(nearest_school))
        
        leafletProxy("map") %>%
          addPolylines(
            lng = c(st_coordinates(selected_barrio_centroid)[1, 1], st_coordinates(nearest_school_centroid)[1, 1]),
            lat = c(st_coordinates(selected_barrio_centroid)[1, 2], st_coordinates(nearest_school_centroid)[1, 2]),
            color = "blue", weight = 2
          )
      }
    }
  }
  
  # Reactive expression for filtered data
  filtered_data <- reactive({
    results_dt[partido_comuna == input$party_dropdown]
  })
  
  # Render filtered results table with average distances
  output$filtered_results_table <- renderDataTable({
    avg_distances <- filtered_data() %>%
      group_by(school_type) %>%
      summarize(avg_distance = mean(distance, na.rm = TRUE))
    
    datatable(avg_distances, options = list(pageLength = 10))
  })
  
  # Render map for selected partido/comuna
  output$partido_map <- renderLeaflet({
    selected_partido <- results_dt_sf[results_dt_sf$partido_comuna == input$party_dropdown, ]
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = selected_partido,
        fillColor = "blue",
        fillOpacity = 0.5,
        color = "#BDBDC3",
        weight = 1
      ) %>%
      addMarkers(
        data = escuelas_inside_amba_wgs84[escuelas_inside_amba_wgs84$partido_comuna == input$party_dropdown, ],
        popup = ~paste("School: ", nombre, "<br>Type: ", modalidad, "_", nivel)
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)