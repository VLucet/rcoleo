
library(readxl)
library(dplyr)
library(stringr)
library(tibble)
library(tidyr)
library(rgdal)
library(geojsonio)


###################################
####### PREP POST sur sites #######
###################################
source("vignettes/import/cells.R")

sheet <- "Végétation"

nms <- names(read_excel("./extdata/V2_CompilationDonnées_2016-2018.xlsx",sheet=sheet))

## Gerer les dates (eviter la conversion automatique)
ct <- ifelse(grepl("^Date", nms,ignore.case = TRUE), "date", "guess")


## deuxieme lecture de la page et ignore le type dans la ligne 2
df <- read_excel("./extdata/CompilationDonnées_2016-2018.xlsx",sheet=sheet,col_types = ct,trim_ws=TRUE)[-1,]

## replacer les espaces par des barres de soulignement dans les noms de colonnes
names(df) <- str_replace_all(names(df)," ", "_")

sites <- select(df,No_de_référence_de_la_cellule,No_de_référence_du_site,Type_de_milieu,Date_inventaire_printanier,Date_inventaire_estival,No_borne_forestière,Latitude,Longitude)

## Configurer la date d'ouverture du site
sites$date_open <- ifelse(is.na(sites$Date_inventaire_printanier),as.character(sites$Date_inventaire_estival),as.character(sites$Date_inventaire_printanier))
sites$date_open <- as.Date(sites$date_open)

## Retirer les dates estivales et printannière
sites <- select(sites,-Date_inventaire_estival,-Date_inventaire_printanier)

names(sites) <- c("cell_id","site_code","type","off_station_code_id","lat","lon","date_open")



## Garder les entrées unique par site
sites <- unique(sites)

## Remplacer les barres de soulignement par des tirets
sites$site_code <- str_replace_all(sites$site_code,"-", "_")

## CORRECTIONS
## Garder monit_prg_station_id juste pour les programmes externes
sites[stringr::str_detect(sites$off_station_code_id,"(\\d{3})-(\\d{3})"),"off_station_code_id"] <- NA

## Transformer en liste pour injection
sites_ls <- apply(sites,1,as.list)

# Creer geom points
loc <- apply(sites,1, function(x){
  if(!any(is.na(x["lat"]),is.na(x["lon"]))){
  return(geojson_list(as.numeric(c(x["lat"],x["lon"])))$features[[1]]$geometry)
} else {
  return(NA)
}})

# Fusionner les deux listes (locations + sites)
for(i in 1:length(sites_ls)){
  sites_ls[[i]]$loc <- loc[i][[1]]
  if(is.list(sites_ls[[i]]$loc)){
    sites_ls[[i]]$loc$crs <- list(type="name",properties=list(name="EPSG:4326"))
  }
}

##############################
####### POST sur sites #######
##############################
resp_sites <- post_sites(sites_ls)

###################################
####### PREP POST sur campaign ####
###################################

campaigns <- unique(select(df,No_de_référence_du_site,Date_inventaire_printanier,Date_inventaire_estival))
campaigns$type <- "végétation"
names(campaigns) <- c("site_code","opened_at","closed_at","type")

# S'il n'y pas de date de fermeture, alors on prend la date d'ouverture et inversement
campaigns[is.na(campaigns$closed_at),"closed_at"] <- campaigns[is.na(campaigns$closed_at),"opened_at"]
campaigns[is.na(campaigns$opened_at),"opened_at"] <- campaigns[is.na(campaigns$opened_at),"closed_at"]

## Remplacer les barres de soulignement par des tirets
campaigns$site_code <- str_replace_all(campaigns$site_code,"-", "_")

# Transforme en list
campaigns_ls <- apply(campaigns,1,as.list)

##############################
##### POST sur campaigns #####
##############################
resp_campaigns <- post_campaigns(campaigns_ls)

## Effort
  



## Campaign

## Tech

## landmarks

## Observations

## ObsSpecies
