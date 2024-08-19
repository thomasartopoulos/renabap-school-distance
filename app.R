library(shiny)
library(leaflet)
library(ggplot2)
library(DT)
library(sf)
library(data.table)

# Load your datasets
barrios_inside_amba <- st_read("output/barrios_inside_amba.geojson")
escuelas_inside_amba <- st_read("output/escuelas_inside_amba.geojson")
results_dt <- fread("output/distances_to_schools.csv")

# Reproject to EPSG:4326 for Leaflet
barrios_inside_amba_wgs84 <- st_transform(barrios_inside_amba, crs = 4326)
escuelas_inside_amba_wgs84 <- st_transform(escuelas_inside_amba, crs = 4326)

# Server function
server <- function(input, output, session) {
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
          h4("No data available for this barrio.")
        )
      })
      output$distance_plot <- renderPlot({
        ggplot() + theme_void() + labs(title = "No Data Available")
      })
      output$distance_table <- renderDataTable({
        datatable(data.frame(Message = "No data available"))
      })
    } else {
      output$barrio_info <- renderUI({
        wellPanel(
          h4(paste("Selected Barrio ID:", selected_id_renabap)),
          p(paste("Nombre Barrio:", selected_data$nombre_barrio[1]))
        )
      })

      # Plot distances in a bar chart
      output$distance_plot <- renderPlot({
        ggplot(selected_data, aes(x = school_type, y = distance, fill = school_type)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(
            title = paste("Shortest Distances to School Types for", selected_id_renabap),
            x = "School Type",
            y = "Distance (meters)"
          ) +
          guides(fill = "none")
      })

      # Render data table with school types and distances
      output$distance_table <- renderDataTable({
        datatable(selected_data[, .(School_Type = school_type, Distance = distance)])
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
}

# UI function
ui <- fluidPage(
  titlePanel("Barrio a escuela"),
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
)

# Run the application 
shinyApp(ui = ui, server = server)
