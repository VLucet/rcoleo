#' Obtenir l'ensemble des campagnes attachées à un site
#'
#' @param site_ids `vector` contenant les codes de sites pour lesquelles ont désire récupérer les campagnes (ex. 134_111_F01)
#' @param dates `vector` contenant les dates de début de campagnes (ex. 2017-01-30)
#' @param type `character` contenant le type de campagnes d'inventaires réalisé (ex. végétation)
#' @examples
#' get_campaigns()
#' @export

get_campaigns <- function(site_ids = NULL, opened_at = NULL, closed_at = NULL, type = NULL, ...){

  endpoint <- rce$endpoints$campaigns

  # Retrieve the site id
  if(!is.null(site_ids)){
    sites <- get_sites(ids=site_ids, rce$endpoints$sites)
    fkey_ids <- sapply(sites,function(x) x$body[,"id"])
  } else {
    fkey_ids <- NULL
  }

  # tests args to set iterator
  len_args <- c(length(site_ids),length(opened_at),length(closed_at),length(type))
  len <- unique(len_args[which(len_args>0)])
  stopif(length(len)>1, "Les vecteurs site_ids, opened_at, closed_at, type n'ont pas la même longueur")

  responses <- list()

  # Prep query
  for(r in 1:len){

    query <- list(site_id=fkey_ids[r],
                  opened_at = opened_at[r],
                  closed_at = closed_at[r],
                  type = type[r])

    responses[[r]] <- get_gen(endpoint,query=query)
  }


  return(responses)

}
