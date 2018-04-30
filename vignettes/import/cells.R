
###########################
##### Injection Cellules ##
###########################

# load
devtools::load_all(".")

# Authentification
bearer <- readRDS(".httr-oauth")

# Ajouter les polygones de cellules
shp_cells <- rgdal::readOGR(dsn="./extdata/shp/",layer="cellule_terrain_BdQc")

# Reprojection vers Quebec Lambert Conic
shp_cells <- sp::spTransform(shp_cells,sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs;"))

# Loop par dessus les noms de cellules pour aller chercher son polygon.
cells_ls <- list()

library(rgdal)
library(geojsonio)

for(i in 1:nlevels(shp_cells$IJ)){

  cell_code <- levels(shp_cells$IJ)[i]
  shp <- shp_cells[shp_cells$IJ == cell_code ,]

  cells_ls[[i]] <- list()
  cells_ls[[i]]$cell_code <- cell_code
  cells_ls[[i]]$name <- shp_cells[shp_cells$IJ == cell_code ,]@data$Nom

  shp_sp <- as(shp, "SpatialPolygons")
  cells_ls[[i]]$geom <- geojsonio::geojson_list(shp)$features[[1]]$geometry

  # Add CRS fields
  cells_ls[[i]]$geom$crs <- list(type="name",properties=list(name="EPSG:4326"))

}

# Envoyer seulement les donnees avec un nom de cellule
resp_cells <- post_cells(cells_ls)
