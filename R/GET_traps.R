#' Retourne les entrées pour les pièges présents dans la base de données
#'
#' @param trap_code `character` vecteur contenant les codes de trappes désirés
#' @param campaign_id `integer` vecteur contenant les identifiants uniques de la campagne
#' @inheritParams get_gen
#' @return
#' Retourne une objet de type `list` contenant les réponses de l'API. Chaque niveau de la liste correspond à une page. Pour chacun des appels sur l'API (page), la classe retourné est `getSuccess` ou `getError`. Une réponse de classe `getSuccess` est une liste à deux niveaux composé du contenu (`body`), et la réponse [httr::response]. Une réponse de classe `getError` dispose de la même structure mais ne contiendra pas de body, seulement la réponse de l'API.
#' @examples
#' get_traps(trap_code=c('MP-22'))
#' get_traps()
#' @export

get_traps <- function(trap_code = NULL, campaign_id = NULL,...) {

  responses <- list()
  query <- list()
  endpoint <- rce$endpoints$traps

  if (is.null(trap_code)) {

    # Obtenir toutes les cellules
    responses[[1]] <- get_gen(endpoint, ...)

  } else {

    # tests args to set iterator
    len <- max(length(trap_code), length(campaign_id))

    # Obtenir des cellules specifiques (query)
    for (r in 1:len) {
      query <- list(trap_code = trap_code[r], campaign_id = campaign_id[r])

      # Make sure NULL is passed to the query instead of NA
      query[which(is.na(query))] <- list(NULL)

      responses[[r]] <- get_gen(endpoint, query = query, ...)

    }
  }


  return(responses)

}
