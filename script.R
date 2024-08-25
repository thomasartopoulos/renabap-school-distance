library(sf)
library(data.table)
library(feather)
library(fst)
library(furrr)

setwd('~/renabap-school-distance')

# Load datasets
escuelas <- st_read('data/establecimientos-educativos.geojson')
barrios <- st_read('data/renabap-2023-12-06.geojson')
amba <- st_read("http://ideconurbano.ungs.edu.ar/geoserver/ows?service=WFS&version=1.0.0&request=GetFeature&typename=geonode%3AAMBA&outputFormat=json&srs=EPSG%3A4326&srsName=EPSG%3A4326")

amba_df <- as.data.frame(amba)
write.csv(amba_df, "output/amba.csv", row.names = FALSE)

# Transform to UTM
utm_crs <- 32633
escuelas <- st_transform(escuelas, crs = utm_crs)
barrios <- st_transform(barrios, crs = utm_crs)
amba <- st_transform(amba, crs = utm_crs)

# Repair and simplify geometries
barrios <- st_make_valid(barrios) %>% st_simplify(dTolerance = 1)
amba <- st_make_valid(amba) %>% st_simplify(dTolerance = 1)
escuelas <- st_make_valid(escuelas)

# Ensure unique identifiers
if(!"id_renabap" %in% names(barrios) || any(is.na(barrios$id_renabap))) {
  stop("id_renabap missing")
}
if(!"nombre_barrio" %in% names(barrios) || any(is.na(barrios$nombre_barrio))) {
  stop("nombre_barrio missing")
}

# Use st_intersection to clip the datasets to AMBA area
escuelas_inside_amba <- st_intersection(escuelas, amba)
barrios_inside_amba <- st_intersection(barrios, amba)

# Save the results as GeoJSON files
st_write(escuelas_inside_amba, "output/escuelas_inside_amba.geojson", driver = "GeoJSON")
st_write(barrios_inside_amba, "output/barrios_inside_amba.geojson", driver = "GeoJSON")

# Check for duplicates
duplicate_barrios <- barrios_inside_amba[duplicated(barrios_inside_amba$id_renabap) | 
                                           duplicated(barrios_inside_amba$id_renabap, fromLast = TRUE), ]
if (nrow(duplicate_barrios) > 0) {
  warning("Duplicados encontrados. Mantenemos primera ocurrencia.")
  barrios_inside_amba <- barrios_inside_amba[!duplicated(barrios_inside_amba$id_renabap), ]
}

# Convert to data.table
escuelas_dt <- as.data.table(escuelas_inside_amba)
barrios_dt <- as.data.table(barrios_inside_amba)


# Get unique school types
school_types <- unique(escuelas_dt[, .(modalidad, nivel)])

######## Paralelo

plan(multisession)

# Calculate distances
calculate_distances <- function(modalidad, nivel) {
  escuelas_subset <- escuelas_inside_amba[
    escuelas_inside_amba$modalidad == modalidad & 
      escuelas_inside_amba$nivel == nivel, ]
  
  if (nrow(escuelas_subset) == 0) {
    warning(paste("No schools found for", modalidad, nivel))
    return(NULL)
  }
  
  distances <- st_distance(barrios_inside_amba, escuelas_subset, by_element = FALSE)
  min_indices <- apply(distances, 1, which.min)
  min_distances <- distances[cbind(1:nrow(distances), min_indices)]
  
  data.table(
    id_renabap = barrios_inside_amba$id_renabap,
    nombre_barrio = barrios_inside_amba$nombre_barrio,
    school_type = paste(modalidad, nivel, sep = "_"),
    distance = as.numeric(min_distances),
    idserv = escuelas_subset$idserv[min_indices],  # Include id_serv
    partido_comuna = amba$nam
  )
}

# Compute distances in parallel
results_list <- future_map2(
  school_types$modalidad, 
  school_types$nivel, 
  calculate_distances
)

# Combine results
results_dt <- rbindlist(results_list)

# Debug duplicates
duplicate_entries <- results_dt[, .N, by = .(id_renabap, nombre_barrio, school_type)][N > 1]
if (nrow(duplicate_entries) > 0) {
  warning("Duplicados encontrados.")
  results_dt <- results_dt[order(distance), .SD[1], by = .(id_renabap, nombre_barrio, school_type)]
}

# Preview results
print(head(results_dt))

# Save results
fwrite(results_dt, 'output/distances_to_schools.csv')
write_feather(as.data.frame(results_dt), 'output/distances_to_schools.feather')
write_fst(as.data.frame(results_dt), 'output/distances_to_schools.fst')
