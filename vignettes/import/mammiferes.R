# Lectures du fichier avec les métadonnées
md <- readRDS("../media-cleanup/mam_md_wt_cp.rds")

library(readxl)
sheet <- "Feuil1"
nms <- names(read_excel("./extdata/V1_Mammifère_Campagne_2016-2017.xlsx",sheet=sheet))
## Gerer les dates (eviter la conversion automatique)
ct <- ifelse(grepl("^Date", nms, ignore.case = TRUE), "date", "guess")
## deuxieme lecture de la page et ignore le type dans la ligne 2
df <- read_excel("./extdata/V1_Mammifère_Campagne_2016-2017.xlsx",sheet=sheet,col_types = ct,trim_ws=TRUE)[-1,]
names(df) <- stringr::str_replace_all(names(df)," ","_")

### On vérifie pour les sites
library(tidyr)
library(dplyr)
campaigns <- unique(select(df,
                        cell_name = "Nom_de_la_cellule",
                        cell_code = "No_de_référence_de_la_cellule",
                        milieu = "Type_de_milieu",
                        cam_code = "No_caméra",
                        opened_at = "Date_d'installation",
                        closed_at = "Date_de_retrait"
                      ))

campaigns$site_code <- ifelse(campaigns$milieu == "forestier", paste0(campaigns$cell_code,"_F01"),paste0(campaigns$cell_code,"_T01"))
campaigns$type <- "mammifères"

campaigns_ls <- apply(campaigns, 1, as.list)

# on prepare les campagnes pour injection
# 1. Techniciens
for(i in 1:length(campaigns_ls)){
  techs <- as.list(c(df[i,"Nom_observateur_1"],df[i,"Nom_observateur_2"]))
  names(techs) <- NULL
  campaigns_ls[[i]]$technicians <- as.list(techs[!is.na(techs)])
}

# on fusionne les deux listes
responses <- post_campaigns(campaigns_ls)
