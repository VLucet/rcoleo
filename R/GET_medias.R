#' Téléverse l'ensemble des médias appartenant à une campagnes
#'
#' @param site_code `character` vecteur contenant les codes de sites pour lesquelles ont désire récupérer les campagnes (ex. 134_111_F01)
#' @param opened_at `character` vecteur contenant les dates de début de campagnes (ex. 2017-01-30)
#' @param closed_at `character` vecteur contenant les dates de fin de campagnes (ex. 2017-01-30)
#' @param type `character` vecteur contenant le type de campagnes d'inventaires réalisé (ex. végétation)
#' @inheritParams get_gen
#' @return
#' Sauvegarde les images/sons attachés aux campagnes à une emplacement défini par l'utilisateur
#' @examples
#' \dontrun{
#' list_medias(site_code="141_108_F01", type="mammifères")
#' Par exemple, on veut obtenir toutes les photos prises dans le cadre d'une campagne sur le site 141_108_F01 et portant sur les mammifères 
#' get_medias(site_code="141_108_F01", type="mammifères")
#' }
#' @export


list_medias <- function(site_code = NULL, opened_at = NULL, closed_at = NULL, type = NULL, ...){
# Preparation de l'objet de sortie
  responses <- list()
  class(responses) <- "coleoGetResp"

  endpoint <- endpoints()$media

  # Si tous les arguments sont nuls
  if(all(is.null(site_code), is.null(opened_at), is.null(closed_at), is.null(type))){

    stop("Au moins un des arguments `site_code`, `opened_at`, `closed_at` et `type` doit être spécifié")

  } else {

    campaigns <- as.data.frame(get_campaigns(site_code, opened_at, closed_at, type))

    for(r in 1:nrow(campaigns)){
      responses[[r]] <- get_gen(endpoint, query = list(campaign_id = campaigns$id[r]))
    } 

    media <- as.data.frame(responses)

    # On regarde si la campagne contient des médias
    if(nrow(media) == 0){
      stop("Aucun média attaché à ces campagnes")
    }

    media$uri <- paste0("/media/",media$type,"/",media$uuid,"/original")

    return(media)

  }
}

#' Télécharge l'ensemble des médias appartenant à une campagnes
#'
#' @param output_dir `character` contenant le chemin d'accès vers le répetoire d'écriture des médias
#' @param verbose `logical` mode verbose
#' @inheritParams list_medias
#' @return
#' Sauvegarde les images/sons attachés aux campagnes à une emplacement définit par l'utilisateur
#' @examples
#' \dontrun{
#' Par exemple, on veut obtenir toutes les photos prises dans le cadre d'une campagne sur le site 141_108_F01 et portant sur les mammifères 
#' get_medias(site_code="141_108_F01", type="mammifères")
#' }
#' @export

get_medias <- function(site_code = NULL, opened_at = NULL, closed_at = NULL, type = NULL, output_dir = "./media", verbose = TRUE, ...) {

  media <- list_medias(site_code, opened_at, closed_at, type)

  # On créer le répertoire de sortie, s'il n'existe pas
  dir.create(output_dir)

  # Mieux gérer la progress bar
  # Vérifier si bug dans les chemins avec Windows
  for(m in 1:nrow(media)){
    if(verbose){
      cat("Téléversement de", m ,"/",nrow(media), "\n")
      cat("Fichier:", media$name[m], "\n")
      cat("Type:", media$type[m], "\n")
    }
    httr::GET(
    url = paste0(server(),media$uri[m]), 
    config = httr::add_headers(Authorization = paste("Bearer", bearer())), 
    ua, httr::write_disk(file.path(output_dir,paste0(media$name[m],media$og_extention[m])), overwrite=TRUE)
    )
  }
}

