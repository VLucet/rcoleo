###### Debug #####
con <- RPostgreSQL::dbConnect("PostgreSQL",user="postgres",host="localhost",dbname="coleo_dev")
bearer <- RPostgreSQL::dbGetQuery(con,"SELECT token FROM api_keys LIMIT 1")
saveRDS(bearer,file=".httr-oauth")
RPostgreSQL::dbDisconnect(con)
####################

# Config de base
server <- function() "http://localhost:3001"
base <- function() "/api/v1"
# bearer <- function() as.character(readRDS(".httr-oauth"))
bearer <- function() "e6086afb109505fe9abd57dd1779ce5c78196ba939ff3e2323868de0d3cec0b9"
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
