#' Obtenir les informations sur les sites depuis l'API de coleo
#' @param site_code est un vecteur contenant les identifiants uniques que l'on désire obtenir. Si site_code n'est pas spécifié, la fonction retournera l'ensemble des entrées présentes dans la table cells.
#' @inheritParams get_gen
#' @examples
#' get_species(vernacular=c('Érable'))
#' get_species()
#' @export

#' Retourne les entrées d'espèces présentes dans la base de données

get_species <- function(species = NULL, genre = NULL, vernacular = NULL,...) {

  responses <- list()
  endpoint <- rce$endpoints$species


  if (is.null(query)) {

    # Obtenir toutes les cellules
    responses <- get_gen(endpoint, ...)

  } else {


    # Obtenir des cellules specifiques (query)
    for (c in 1:length(query)) {

      responses[[c]] <- get_gen(endpoint, query = list(q = query[c]), ...)

      if (length(responses[[c]]$content) == 0) {

        message(query[c], " n'est pas présent dans la base de données")

      } else if (nrow(responses[[c]]$content) > 1) {

        message(nrow(responses[[c]]$content), " entrées ont été retourné par la base de données pour le numéro du site: ",
          query[c])

      }

    }
  }


  return(responses)

}
