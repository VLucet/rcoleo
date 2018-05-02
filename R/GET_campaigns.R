#' Obtenir l'ensemble des campagnes attachées à un site
#'
#' @param ids `vector` contenant la liste des codes de sites pour lesquelles ont désire récupérer les campagnes
#' @param simplify `logical` l'objet de sortie de la fonction est un `data.frame` à la place d'un objet de type `list`
#' @inherit get_ids
#' @examples
#' get_campaigns()
#' @export

get_campaigns <- function(ids=NULL, ...){

  sites <- get_sites(ids, rce$endpoints$sites)

  responses <- list()

  for(id in 1:length(sites)){
    responses[[id]] <- list(site_code = NULL, campaigns = NULL)
    responses[[id]]$site_code <- sites[[id]]$body$site_code
    responses[[id]]$campaigns <- do.call("rbind",sites[[id]]$body$campaigns)
  }

  return(responses)

}
