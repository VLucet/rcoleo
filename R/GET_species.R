#' Retourne les entrées d'espèces présentes dans la base de données
#'
#' @param searchEntry `character` vecteur contenant les mots clés (recherche sur l'intégralité de la table)
#' @param species `character` vecteur contenant les espèces recherché (sensible à la case) - Obsolète si l'argument query est spécifié
#' @param genus `character` vecteur contenant les genres d'espèce recherché (sensible à la case) - Obsolète si l'argument query est spécifié
#' @param vernacular `character` vecteur contenant les noms vernaculaires des espèces recherchées (sensible à la case) - Obsolète si l'argument query est spécifié
#' @inheritParams get_gen
#' @return
#' Retourne une objet de type `list` contenant les réponses de l'API. Chaque niveau de la liste correspond à une page. Pour chacun des appels sur l'API (page), la classe retourné est `getSuccess` ou `getError`. Une réponse de classe `getSuccess` est une liste à deux niveaux composé du contenu (`body`), et la réponse [httr::response]. Une réponse de classe `getError` dispose de la même structure mais ne contiendra pas de body, seulement la réponse de l'API.
#' @examples
#' get_species(vernacular=c('Érable'))
#' get_species()
#' @export

get_species <- function(searchEntry=NULL, species = NULL, genus = NULL, vernacular = NULL,...) {

  responses <- list()
  query <- list()
  endpoint <- rce$endpoints$species


  if (all(is.null(searchEntry),is.null(species),is.null(genus),is.null(vernacular))) {

    # Obtenir toutes les cellules
    responses <- get_gen(endpoint, ...)

  } else if(!is.null(searchEntry)) {

    len <- length(searchEntry)

    for(r in 1:len){

      query$q = searchEntry[r]
      responses[[r]] <- get_gen(endpoint, query = query, ...)

    }

  } else {

    # tests args to set iterator
    len_args <- c(length(species),length(genus),length(vernacular))
    len <- unique(len_args[which(len_args>0)])
    stopifnot(length(len)==1)

    # Obtenir des cellules specifiques (query)
    for (r in 1:len) {
      query <- list(species=species[r],
                    genus = genus[r],
                    vernacular = vernacular[r])

      responses[[r]] <- get_gen(endpoint, query = query, ...)

    }
  }


  return(responses)

}
