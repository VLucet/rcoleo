
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
sites[str_detect(sites$off_station_code_id,"(\\d{3})-(\\d{3})"),"off_station_code_id"] <- NA

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

library(reshape2)

campaigns <- unique(select(df,No_de_référence_du_site,Date_inventaire_printanier,Date_inventaire_estival,Nom_observateur_1__1, Nom_observateur_2__1,Nom_observateur_1, Nom_observateur_2))
campaigns$type <- "végétation"
names(campaigns) <- c("site_code","opened_at","closed_at","Obs1","Obs2","Obs3","Obs4","type")
campaigns$key <- rownames(campaigns)

# S'il n'y pas de date de fermeture, alors on prend la date d'ouverture et inversement
campaigns[is.na(campaigns$closed_at),"closed_at"] <- campaigns[is.na(campaigns$closed_at),"opened_at"]
campaigns[is.na(campaigns$opened_at),"opened_at"] <- campaigns[is.na(campaigns$opened_at),"closed_at"]

## Remplacer les barres de soulignement par des tirets
campaigns$site_code <- str_replace_all(campaigns$site_code,"-", "_")

## Melt pour les observateurs
techs <- select(campaigns,key,Obs1,Obs2,Obs3,Obs4)
techs <- melt(techs,id.vars=c("key"),na.rm=TRUE)
techs <- techs[which(techs$value != "NA"),]
techs <- unique(select(techs, -variable))
techs$name <- word(techs$value,1)
techs$lastname <- word(techs$value,2)
techs <- select(techs, -value)

# On enlève les champs Observateurs des campagnes
campaigns <- select(campaigns,-Obs1,-Obs2,-Obs3,-Obs4)

# Transforme en list
campaigns_ls <- apply(campaigns,1,as.list)

# On loop sur la liste pour ajouter les techniciens
for(l in 1:length(campaigns)){
  k <- as.numeric(campaigns_ls[[l]]$key)
  tech_add <- subset(techs, key == k)
  campaigns_ls[[l]]$technicians <- list()

  for(i in 1:nrow(tech_add)){
      campaigns_ls[[l]]$technicians[[i]] <- list(name=tech_add[i,"name"], lastname=tech_add[i,"lastname"])
  }

}


# Ajout de l'effort d'échantillonnage
campaigns_ls <- lapply(campaigns_ls, function(x) {
  x$efforts <- list(
    list(stratum = "bryophytes", samp_surf = "10", samp_surf_unit = "m2"),
    list(stratum = "arbustes/herbacées", samp_surf = "100", samp_surf_unit = "m2"),
    list(stratum = "arbres", samp_surf = "400", samp_surf_unit = "m2")
  )
  return(x)
})

##### POST Campaigns
responses <- post_campaigns(campaigns_ls)


###################
# AJOUT LANDMARKs #
###################

# Ajout des landmarks
landmarks_A <- unique(df[,c("No_de_référence_du_site","Date_inventaire_printanier","Date_inventaire_estival", "No_de_l'arbre_repère_A_(AR-A)", "Essence", "DHP_(mm)_AR-A", "Azimut_(o)_AR-A", "Distance_AR-A_(cm)","Latitute_AR-A","Londitude_AR-A")])
landmarks_A$type <- "végétation"
names(landmarks_A) <- c("site_code","opened_at","closed_at", "tree_code","sp","dbh","azimut","distance","lat","lon","type")

# Ajout des landmarks
landmarks_B <- unique(df[,c("No_de_référence_du_site","Date_inventaire_printanier","Date_inventaire_estival", "No_de_l'arbre_repère_B_(AR-B)", "Essence__1", "DHP_(mm)_AR-B", "Azimut_(o)_AR-B", "Distance_AR-B_(cm)", "Latitute_AR-B", "Londitude_AR-B")])
landmarks_B$type <- "végétation"
names(landmarks_B) <- c("site_code","opened_at","closed_at", "tree_code","sp","dbh","azimut","distance","lat","lon","type")

# Binding sur les lignes
landmarks <- bind_rows(landmarks_A,landmarks_B)

# S'il n'y pas de date de fermeture, alors on prend la date d'ouverture et inversement
landmarks[is.na(landmarks$closed_at),"closed_at"] <- landmarks[is.na(landmarks$closed_at),"opened_at"]
landmarks[is.na(landmarks$opened_at),"opened_at"] <- landmarks[is.na(landmarks$opened_at),"closed_at"]

## Remplacer les barres de soulignement par des tirets
landmarks$site_code <- str_replace_all(landmarks$site_code,"-", "_")

## Retirer les campagnes qui ne disposent pas de landmarks
landmarks <- landmarks[-which(landmarks[,c(4:8)]=="NA" | is.na(landmarks[,c(4:8)])),]

# Clean taxonomy
landmarks$sp <- toupper(landmarks$sp)

# Créer la correspondance avec les codes
species_code <- data.frame(code=c("ES","SB","PU","EO","BG","BP","BJ"), vernacular = c("Érable à sucre", "Sapin baumier", "Cerisier tardif", "Érable rouge", "Bouleau gris", "Bouleau à papier", "Bouleau jaune"))
# TODO: Valider les correspondances, surtout pour prunus et érable rouge

species_ls <- list()
for(i in 1:nrow(species_code)) species_ls[[i]] <- get_species(vernacular = species_code[i,"vernacular"])
species_code <- bind_cols(species_code,bind_rows(lapply(unlist(species_ls,recursive=FALSE), function(x) return(x[[1]]$body[,c("id","vernacular")]))))

landmarks$sp_id <- species_code[match(landmarks$sp,species_code$code),"id"]
landmarks$type <- "both"

## Final touch, prep loc et data
landmarks_ls <- apply(landmarks,1,as.list)

loc <- apply(landmarks,1, function(x){
  if(!any(is.na(x["lat"]),is.na(x["lon"]))){
  return(geojson_list(as.numeric(c(x["lat"],x["lon"])))$features[[1]]$geometry)
} else {
  return(NA)
}})

# Fusionner les deux listes (locations + sites)
for(i in 1:length(landmarks_ls)){
  landmarks_ls[[i]]$loc <- loc[i][[1]]
  if(is.list(landmarks_ls[[i]]$loc)){
    landmarks_ls[[i]]$loc$crs <- list(type="name",properties=list(name="EPSG:4326"))
  }
}

# Post landmarks
resp <- post_landmarks(landmarks_ls)

## Observations

## ObsSpecies
