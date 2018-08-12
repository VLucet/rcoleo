#' Retourne les entrées pour les repères sur le terrain spécifique à une campagne d'inventaires

#' @inheritParams get_campaigns
#' @return
#' Retourne une objet de type `list` contenant les réponses de l'API. Chaque niveau de la liste correspond à une page. Pour chacun des appels sur l'API (page), la classe retourné est `getSuccess` ou `getError`. Une réponse de classe `getSuccess` est une liste à deux niveaux composé du contenu (`body`), et la réponse [httr::response]. Une réponse de classe `getError` dispose de la même structure mais ne contiendra pas de body, seulement la réponse de l'API.
#' @examples
#' get_landmarks()
#' @export

get_landmarks <- function(site_code = NULL, opened_at = NULL, closed_at = NULL, type = NULL, ...) {

  # DEBUG
  # site_code=NULL
  # opened_at = NULL
  # closed_at = NULL
  # type = "végétation"

  # 1 call = 1 combinaison de filters
  endpoint <- endpoints()$landmarks
  responses <- list()

  # Si tous les arguments sont nuls
  if(all(is.null(site_code), is.null(opened_at), is.null(closed_at), is.null(type))){

    responses[[1]] <- get_gen(endpoint, ...)

  } else {

    # tests args to set iterator
    len <- max(c(length(site_code),length(opened_at),length(closed_at),length(type)))

    # Prep query
    for(r in 1:len){
      campaigns_ids <- unlist(lapply(get_campaigns(site_code=site_code[r],
                    opened_at = opened_at[r],
                    closed_at = closed_at[r],
                    type = type[r]), function(x) return(x[[1]]$body$id)))

      # On récupère les landmarks pour la campagne concernée
      for(i in 1:length(campaigns_ids)) responses[[i]] <- get_gen(endpoint, query=list(campaign_id=campaigns_ids[i]))

      # On ajoute les informations sur la campagne
      responses <- lapply(responses, function(x){
        if(attributes(x[[1]]$body)$n_records != 0){

          campaign_id <- unique(x[[1]]$body$campaign_id)
          stopifnot(length(campaign_id) == 1)

          # Campagne info
          campaign_info <- httr::content(httr::GET(url=paste0(server,"/api/v1/campaigns/",campaign_id), config = httr::add_headers(`Content-type` = "application/json",Authorization = paste("Bearer", bearer)),ua), simplify = TRUE)
          # Code du site
          site_code <- httr::content(httr::GET(url=paste0(server,"/api/v1/sites/",campaign_info$site_id), config = httr::add_headers(`Content-type` = "application/json",Authorization = paste("Bearer", bearer)),ua), simplify = TRUE)$site_code

          # Species info
          species_info <- apply(x[[1]]$body,1, function(rec){
            if(rec$type == "tree" | rec$type == "both"){
              httr::content(httr::GET(url=paste0(server,"/api/v1/taxa/",rec$sp_id), config = httr::add_headers(`Content-type` = "application/json",Authorization = paste("Bearer", bearer)),ua), simplify = TRUE)[c("vernacular_fr","name","rank","tsn")]
            } else {
              return(rep(NA,4))
            }
          })

          species_info <- do.call(rbind,species_info)


          # On prepare la sortie
          x[[1]]$body$site_code <- site_code
          x[[1]]$body$opened_at <- campaign_info$opened_at
          x[[1]]$body$closed_at <- campaign_info$closed_at
          x[[1]]$body$type <- campaign_info$type
          x[[1]]$body <- cbind(x[[1]]$body,species_info)

          x[[1]]$body <- dplyr::select(x[[1]]$body,
            "id",
            "site_code",
            "opened_at",
            "closed_at",
            "type",
            "campaign_id",
            "trap_id",
            "lure_id",
            "device_id",
            "tree_code",
            "vernacular_fr",
            "name",
            "rank",
            "tsn",
            "dbh",
            "dbh_unit",
            "azimut",
            "distance",
            "distance_unit",
            "notes",
            "created_at",
            "updated_at"
          )

          return(x)

        } else {

          return(x)

        }

      })
    }
  }

  return(responses)

}
