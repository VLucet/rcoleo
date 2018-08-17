# ###### Debug #####
# con <- RPostgreSQL::dbConnect("PostgreSQL",user="postgres",host="localhost",dbname="coleo_dev")
# bearer <- RPostgreSQL::dbGetQuery(con,"SELECT token FROM api_keys LIMIT 1")
# saveRDS(bearer,file=".httr-oauth")
# RPostgreSQL::dbDisconnect(con)
# ####################

# Config de base
#server <- function() "http://localhost:3001"
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
    medias = "/medias",
    observations = "/observations",
    taxa = "/taxa",
    landmarks = "/landmarks",
    attributes = "/attributes",
    traps = "/traps",
    samples = "/samples"
  )
}
