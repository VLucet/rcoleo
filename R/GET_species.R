#' Retourne les entrées d'espèces présentes dans la base de données
#'
#' @param species `character` vecteur contenant les espèces recherché (sensible à la case)
#' @param genus `character` vecteur contenant les genres d'espèce recherché (sensible à la case)
#' @param vernacular `character` vecteur contenant les noms vernaculaires des espèces recherchées (sensible à la case)
#' @inheritParams get_gen
#' @return
#' Retourne une objet de type `list` contenant les réponses de l'API. Chaque niveau de la liste correspond à une page. Pour chacun des appels sur l'API (page), la classe retourné est `getSuccess` ou `getError`. Une réponse de classe `getSuccess` est une liste à deux niveaux composé du contenu (`body`), et la réponse [httr::response-class]. Une réponse de classe `getError`.
#' @examples
#' get_species(vernacular=c('Érable'))
#' get_species()
#' @export

get_species <- function(species = NULL, genus = NULL, vernacular = NULL, type = NULL,...) {

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
