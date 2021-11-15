#------------------------------------------------------

# Going to make a Leaflet map

library(sf)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(janitor)
library(tidyr)

sf_sp_unit <- st_read("./data/spatial/Sun_Prairie_Unit/Sun_Prairie_Unit.shp") %>%
  st_transform(4326)

# Load American Prairie Reserve (APR) data
df_apr <- read_csv("./data/base/SCBI_2020_camera_trap_survey.csv") %>%
  clean_names() %>%
  # Consistent deployment naming
  mutate(deployment_id = toupper(deployment_id))

# Camera trap locations
sf_cam_loc <- df_apr %>%
  select(deployment_id, long, lat) %>%
  distinct() %>%
  separate(deployment_id, into = c("deployment", "period"), sep = "_(?=[^_]+$)") %>%
  group_by(deployment) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

#-------------------------------------------------------

# Map

# Icons
cam <- makeAwesomeIcon(
  icon = "camera",
  iconColor = "black",
  library = "ion",
  markerColor = "white"
)

map <- sf_sp_unit %>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite Imagery") %>%
  addFullscreenControl() %>%
  addResetMapButton() %>%
  addScaleBar(position = "bottomleft",
              options = scaleBarOptions(imperial = FALSE)) %>%

  # Add polygon layers
  addPolygons(color = "darkred", weight = 2, smoothFactor = 0.2, opacity = 2, fill = FALSE,
              group = "None") %>%

  addAwesomeMarkers(data = sf_cam_loc,
                    icon = cam, group = "Cameras") %>%

  addLayersControl(overlayGroups = c("Cameras", "Satellite Imagery"),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "topright") %>%

  hideGroup(c("Cameras", "Satellite Imagery"))

map

htmlwidgets::saveWidget(map, file = "./map1.html", selfcontained = TRUE)





