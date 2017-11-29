# Cache env
rcoleo.env <- new.env(parent = emptyenv())

# Base URI
rcoleo.env$prod <- list()
rcoleo.env$dev <- list()

# Config de base
rcoleo.env$base <- "/api/v0"
rcoleo.env$bearer <- readRDS(".httr-oauth")

# Environnement de production
rcoleo.env$prod$server <- NULL

# Environnement de dev
rcoleo.env$dev$server <- "http://localhost:8080"

# Point d'entrées pour le retrait ou l'analyse des données
rcoleo.env$endpoints <- list()
rcoleo.env$endpoints$cells <- "/cells"
rcoleo.env$endpoints$sites <- "/sites"
rcoleo.env$endpoints$campaigns <- "/campaigns"
rcoleo.env$endpoints$medias <- "/medias"
rcoleo.env$endpoints$observations <- "/observations"
rcoleo.env$endpoints$species <- "/species"
