#' Retourne les entrées pour les échantillons présents dans la base de données
#'
#' @param sample_code `character` vecteur contenant les codes d'identification unique des échantillons
#' @inheritParams get_gen
#' @return
#' Retourne une objet de type `list` contenant les réponses de l'API. Chaque niveau de la liste correspond à une page. Pour chacun des appels sur l'API (page), la classe retourné est `getSuccess` ou `getError`. Une réponse de classe `getSuccess` est une liste à deux niveaux composé du contenu (`body`), et la réponse [httr::response]. Une réponse de classe `getError` dispose de la même structure mais ne contiendra pas de body, seulement la réponse de l'API.
#' @examples
#' get_samples(trap_code=c('2016-0026'))
#' get_samples()
#' @export

get_samples <- function(sample_code = NULL, ...) {

  # Preparation de l'objet de sortie
  responses <- list()
  class(responses) <- "coleoGetResp"

  endpoint <- endpoints()$samples

  if (is.null(sample_code)) {

    # Obtenir toutes les cellules
    responses[[1]] <- get_gen(endpoint, ...)

  } else {

    # tests args to set iterator
    len <- length(sample_code)

    # Obtenir des cellules specifiques (query)
    for (r in 1:len) {
      query <- list(sample_code = sample_code[r])

      # Make sure NULL is passed to the query instead of NA
      query[which(is.na(query))] <- list(NULL)

      responses[[r]] <- get_gen(endpoint, query = query, ...)

    }
  }


  return(responses)

}
