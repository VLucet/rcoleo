#' Retourne les entrées d'espèces présentes dans la base de données

get_species <- function(query = NULL) {

  responses <- list()
  endpoint <- rce$endpoints$species


  if (is.null(query)) {

    # Obtenir toutes les cellules
    responses <- get_gen(endpoint)

  } else {

    stopifnot(is.character(query))

    # Obtenir des cellules specifiques (query)
    for (c in 1:length(query)) {

      responses[[c]] <- get_gen(endpoint, query = list(q = query[c]))

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
