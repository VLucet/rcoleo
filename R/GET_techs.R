#' Obtenir l'ensemble des techiciens attachés à une campagne
#'
#' @param name `character` vecteur contenant le prénom du technicien (sensible à la case)
#' @param lastname `character` vecteur contenant le nom du technicien (sensible à la case)
#' @inheritParams get_gen
#' @return
#' Retourne une objet de type `list` contenant les réponses de l'API. Chaque niveau de la liste correspond à une page. Pour chacun des appels sur l'API (page), la classe retourné est `getSuccess` ou `getError`. Une réponse de classe `getSuccess` est une liste à deux niveaux composé du contenu (`body`), et la réponse [httr::response-class]. Une réponse de classe `getError`.
#' @examples
#' get_techs(name="Caroline")
#' @export

get_techs <- function(name = NULL, lastname = NULL, ...){

  responses <- list()
  endpoint <- rce$endpoints$technicians

  if (all(is.null(name),is.null(lastname))) {

    # Obtenir tous les sites
    responses <- get_gen(endpoint, ...)

  } else {

    # tests args to set iterator
    len_args <- c(length(name),length(lastname))
    len <- unique(len_args[which(len_args>0)])
    stopifnot(length(len)==1)

    # Prep query
    for(r in 1:len){

      query <- list(lastname = lastname[r],
                    name = name[r])

      responses[[r]] <- get_gen(endpoint,query=query, ...)
    }
  }

  return(responses)

}
