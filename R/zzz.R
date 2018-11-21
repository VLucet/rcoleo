# Config de base
server <- function() "https://coleo.biodiversite-quebec.ca"
base <- function() "/api/v1"
bearer <- function() as.character(readRDS(".httr-oauth"))
ua <- httr::user_agent("rcoleo")

# Point d'entrées de l'API
endpoints <- function(){
  list(
    cells = "/cells",
    sites = "/sites",
    campaigns = "/campaigns",
    media = "/media",
    observations = "/observations",
    taxa = "/taxa",
    landmarks = "/landmarks",
    attributes = "/attributes",
    traps = "/traps",
    samples = "/samples",
    lures = "/lures",
    devices = "/devices"
  )
}
