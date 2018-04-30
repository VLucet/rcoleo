# Cache env
rce <- new.env()

assign("server", "http://localhost:3001", envir = rce)

###### Debug #####
con <- RPostgreSQL::dbConnect("PostgreSQL",user="postgres",host="localhost",dbname="coleo_dev")
bearer <- RPostgreSQL::dbGetQuery(con,"SELECT token FROM api_keys LIMIT 1")
saveRDS(bearer,file=".httr-oauth")
RPostgreSQL::dbDisconnect(con)
####################


# Config de base
assign("base", "/api/v1", envir = rce)
assign("bearer", readRDS(".httr-oauth"), envir = rce)
assign("ua", httr::user_agent("rcoleo"), envir = rce )

# Point d'entrées pour le retrait ou l'analyse des données
endpoints <- list()
endpoints$cells <- "/cells"
endpoints$sites <- "/sites"
endpoints$campaigns <- "/campaigns"
endpoints$medias <- "/medias"
endpoints$observations <- "/observations"
endpoints$species <- "/species"

assign("endpoints", endpoints, envir = rce)

lockEnvironment(rce)
