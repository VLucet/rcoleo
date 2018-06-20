#' Obtenir les informations sur les cellules depuis l'API de coleo
#' @param cell_code est un vecteur contenant les identifiants uniques que l'on désire obtenir. Si cell_code n'est pas spécifié, la fonction retournera l'ensemble des entrées présentes dans la table cells.
#' @examples
#' get_cells(cell_code=c('111_91'))
#' get_cells()
#' @export

get_cells <- function(cell_code = NULL, ...) {

  responses <- list()
  endpoint <- rce$endpoints$cells

  if (is.null(cell_code)) {

    # Obtenir tous les sites
    responses <- get_gen(endpoint)

  } else {

    # Obtenir les sites définis dans ids
    for (id in 1:length(cell_code)) {

      # prep query
      responses[[id]] <- unlist(get_gen(endpoint, query = list(cell_code = cell_code[id])), recursive=FALSE)

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
