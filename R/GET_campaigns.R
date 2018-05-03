#' Obtenir l'ensemble des campagnes attachées à un site
#'
#' @param ids `vector` contenant la liste des codes de sites pour lesquelles ont désire récupérer les campagnes
#' @param simplify `logical` l'objet de sortie de la fonction est un `data.frame` à la place d'un objet de type `list`
#' @inherit get_ids
#' @examples
#' get_campaigns()
#' @export

get_campaigns <- function(ids=NULL, ...){

  ##### Filter by type and date campagne?
  ### SOL1: rebuild query, avoid using get_ids(), use get_gen()
  ### Ajout de fonctionnalitees, je veux aller chercher tous les types de campagne vegetation pour injecter en batch l'effort qui est le meme dans ce cas.

  if(!is.null(ids)){
    sites <- get_sites(ids, rce$endpoints$sites)

    responses <- list()

    ## Est-ce que c'est une bonne idee que je perde la structure list(body=body, response=response)?
    ## Non, write utils called simplify which turn into data.frame the responses

    for(id in 1:length(sites)){
      responses[[id]] <- list(site_code = NULL, campaigns = NULL)
      responses[[id]]$site_code <- sites[[id]]$body$site_code
      responses[[id]]$campaigns <- do.call("rbind",sites[[id]]$body$campaigns)
    }

    return(responses)
  }



}
