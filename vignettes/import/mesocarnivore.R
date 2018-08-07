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
# source("./vignettes/import/microfaune.R")
sheet <- "Inv. mammifère"

nms <- names(read_excel("./extdata/V2_CompilationDonnées_2016-2018.xlsx",sheet=sheet))

## Gerer les dates (eviter la conversion automatique)
ct <- ifelse(grepl("^Date|Heure", nms, ignore.case = TRUE), "date", "guess")

## deuxieme lecture de la page et ignore le type dans la ligne 2
df <- read_excel("./extdata/V2_CompilationDonnées_2016-2018.xlsx",sheet=sheet,col_types = ct)[-1,]

## replacer les espaces par des barres de soulignement dans les noms de colonnes
names(df) <- str_replace_all(names(df)," ", "_")

## On sélectionne les colonnes d'intérêts
sites <- unique(select(df,cell_id=No_de_référence_de_la_cellule,site_code=No_de_référence_du_site,type=Type_de_milieu,opened_at="Date_d'installation",lat=Latitude, lon=Longitude))


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

responses <- post_sites(sites_ls)

###################################
####### PREP POST sur campaign ####
###################################

library(reshape2)

campaigns <- unique(select(
  df,
  site_code=No_de_référence_du_site,
  opened_at="Date_d'installation"
))

campaigns$type <- "mésocarnivores"

## Remplacer les barres de soulignement par des tirets
campaigns$site_code <- str_replace_all(campaigns$site_code,"-", "_")

# Transforme en list
campaigns_ls <- apply(campaigns,1,as.list)

techs <- unique(select(
  df,
  site_code = No_de_référence_du_site,
  opened_at = "Date_d'installation",
  Observateur1 = Nom_observateur_1,
  Observateur2 = Nom_observateur_2
))

# On melt le jeux de données d'Observateurs
techs <- melt(techs, id.vars=c("site_code","opened_at"), value.name="Observateur", na.rm =TRUE)
techs <- select(techs, -variable)

# On ajoute les technicians
campaigns_ls <- lapply(campaigns_ls, function(x){
  x$technicians <- as.list(techs[which(techs$site_code == x$site_code & as.character(techs$opened_at) == x$opened_at),]$Observateur)
  return(x)
})

##### AJOUT DEVICES


##### AJOUT LEURRES


##### POST Campaigns
responses <- post_campaigns(campaigns_ls)
diagnostic(responses)
