#' Fonction generique permettant le retrait d'une entrée par son identifiant unique
#'
#' @param ids est un vecteur contenant les identifiants uniques que l'on désire obtenir. Si ids n'est pas spécifié, la fonction retournera l'ensemble des entrées présentes dans la table cibée.
#' @inherit get_gen
#' @export


get_ids <- function(ids = NULL, endpoint = NULL, ...) {

  responses <- list()

  if (is.null(ids)) {

    # Obtenir tous les sites
    responses <- get_gen(endpoint)

  } else {

    stopifnot(is.character(ids))

    # Obtenir les sites définis dans ids
    for (id in 1:length(ids)) {

      # prep query
      responses[[id]] <- unlist(get_gen(endpoint, query = list(code = ids[id])), recursive=FALSE)

      if (length(responses[[id]]$body) == 0) {

        message(ids[id], " n'est pas présent dans la base de données")

      } else if (nrow(responses[[id]]$body) > 1) {

        message(nrow(responses[[id]]$body), " entrées pour le numéro du site: ",
          ids[id])

      }

    }
  }


  return(responses)

}
