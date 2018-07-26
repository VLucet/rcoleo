#' Retourne les entrées d'espèces présentes dans la base de données
#'
#' @param name `character` vecteur contenant les noms taxonomiques recherché (sensible à la case) - Obsolète si l'argument query est spécifié
#' @param rank `character` vecteur contenant les rangs taxonomiques recherché (sensible à la case) - Obsolète si l'argument query est spécifié
#' @param vernacular_fr `character` vecteur contenant les noms vernaculaires en francais des espèces recherchées (sensible à la case) - Obsolète si l'argument query est spécifié
#' @inheritParams get_gen
#' @return
#' Retourne une objet de type `list` contenant les réponses de l'API. Chaque niveau de la liste correspond à une page. Pour chacun des appels sur l'API (page), la classe retourné est `getSuccess` ou `getError`. Une réponse de classe `getSuccess` est une liste à deux niveaux composé du contenu (`body`), et la réponse [httr::response]. Une réponse de classe `getError` dispose de la même structure mais ne contiendra pas de body, seulement la réponse de l'API.
#' @examples
#' get_species(vernacular_fr=c('Érable'))
#' get_species()
#' @export

get_species <- function(name = NULL, rank = NULL, vernacular_fr = NULL,...) {

  responses <- list()
  query <- list()
  endpoint <- rce$endpoints$taxa

  if (all(is.null(name),is.null(rank),is.null(vernacular_fr))) {

    # Obtenir toutes les cellules
    responses <- get_gen(endpoint, ...)

  } else {

    # tests args to set iterator
    len <- max(c(length(name),length(rank),length(vernacular_fr)))

    # Obtenir des cellules specifiques (query)
    for (r in 1:len) {
      query <- list(name = name[r],
                    rank = rank[r],
                    vernacular_fr = vernacular_fr[r])

      # Make sure NULL is passed to the query instead of NA
      query[which(is.na(query))] <- list(NULL)

      responses[[r]] <- get_gen(endpoint, query = query, ...)

    }
  }


  return(responses)

}
