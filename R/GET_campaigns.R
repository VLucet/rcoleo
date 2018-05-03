#' Obtenir l'ensemble des campagnes attachées à un site
#'
#' @param site_ids `vector` contenant les codes de sites pour lesquelles ont désire récupérer les campagnes (ex. 134_111_F01)
#' @param dates `vector` contenant les dates de début de campagnes (ex. 2017-01-30)
#' @param type `character` contenant le type de campagnes d'inventaires réalisé (ex. végétation)
#' @examples
#' get_campaigns()
#' @export

get_campaigns <- function(site_ids = NULL, dates = NULL, type = NULL, ...){

  endpoint <- rce$endpoints$campaigns

  if(all(length(site_ids) == length(dates) && !is.null(site_ids) && !is.null(type))){
    stop("Les deux vecteurs `ids`, `dates` doivent avoir une taille identique")
  }

  if(all(length(type) > 1 && !is.null(type))){
    stop("un seul type peut être spécifié à la fois")
  }

  if(!is.null(site_ids)){
    sites <- get_sites(ids=site_ids, rce$endpoints$sites)
    fkey_ids <- sapply(sites,function(x) x$body[,"id"])
  } else {
    fkey_ids <- NULL
  }

  # tests args to set iterator
  len_args <- c(length(site_ids),length(dates),length(type))
  len <- unique(len_args[which(len_args>0)])

  responses <- list()

  # Prep query
  for(r in 1:len){
    query <- list(site_id=fkey_ids[r], date = dates[r],type = type)
    responses[[r]] <- get_gen(endpoint,query=query)
  }


  return(responses)

}
