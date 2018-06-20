#' Obtenir les informations sur les cellules depuis l'API de coleo
#' @param cell_code `character` vecteur contenant les identifiants uniques que l'on désire obtenir. Si cell_code n'est pas spécifié, la fonction retournera l'ensemble des entrées présentes dans la table cells.
#' @return
#' Retourne une objet de type `list` contenant les réponses de l'API. Chaque niveau de la liste correspond à une page. Pour chacun des appels sur l'API (page), la classe retourné est `getSuccess` ou `getError`. Une réponse de classe `getSuccess` est une liste à deux niveaux composé du contenu (`body`), et la réponse [httr::response-class]. Une réponse de classe `getError`.
#' @inheritParams get_gen
#' @examples
#' get_cells(cell_code=c('111_91'))
#' get_cells()
#' @export

get_cells <- function(cell_code = NULL, ...) {

  responses <- list()
  endpoint <- rce$endpoints$cells

  if (is.null(cell_code)) {

    # Obtenir tous les sites
    responses <- get_gen(endpoint, ...)

  } else {

    # Obtenir les sites définis dans ids
    for (id in 1:length(cell_code)) {

      # prep query
      responses[[id]] <- unlist(get_gen(endpoint, query = list(cell_code = cell_code[id]), ...), recursive=FALSE)

      if (length(responses[[id]]$body) == 0) {

        message(cell_code[id], " n'est pas présent dans la base de données")

      } else if (nrow(responses[[id]]$body) > 1) {

        message(nrow(responses[[id]]$body), " entrées pour le numéro du site: ",
          cell_code[id])

      }

    }
  }


  return(responses)


}
