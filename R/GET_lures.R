#' Retourne les entrées pour les leurres utilisées dans le cadre d'une campagne présents dans la base de données
#'
#' @param campaign_id `integer` vecteur contenant les identifiants uniques de la campagne
#' @inheritParams get_gen
#' @return
#' Retourne une objet de type `list` contenant les réponses de l'API. Chaque niveau de la liste correspond à une page. Pour chacun des appels sur l'API (page), la classe retourné est `getSuccess` ou `getError`. Une réponse de classe `getSuccess` est une liste à deux niveaux composé du contenu (`body`), et la réponse [httr::response]. Une réponse de classe `getError` dispose de la même structure mais ne contiendra pas de body, seulement la réponse de l'API.
#' @examples
#' head(get_lures())
#' @export

get_lures <- function(campaign_id = NULL,...) {

  # Preparation de l'objet de sortie
  responses <- list()
  class(responses) <- "coleoGetResp"

  endpoint <- endpoints()$lures

  if (is.null(campaign_id)) {

    # Obtenir toutes les cellules
    responses[[1]] <- get_gen(endpoint, ...)

  } else {

    # tests args to set iterator
    len <- length(campaign_id)

    # Obtenir des cellules specifiques (query)
    for (r in 1:len) {

      query <- list(campaign_id = campaign_id[r])
      responses[[r]] <- get_gen(endpoint, query = query, ...)

    }
  }


  return(responses)

}
