#' Obtenir les informations sur des sites depuis l'API de coleo
#'
#' @param ids est un vecteur contenant les identifiants uniques des sites que l'on désire obtenir. Si ids n'est pas spécifié, la fonction retournera l'ensemble des sites présents dans la base de données.
#' @return Un objet \code{list}, dont chacun des niveaux corresponds à la réponse de l'API. La réponse peut être de classe \code{getError} ou \code{getSuccess}.
#' @examples
#' get_sites(ids=c('135_104_H01'))
#' get_sites()
#' @export


get_sites <- function(ids = ids, ...) {

  responses <- list()
  endpoint <- rce$endpoints$sites

  if (is.null(ids)) {

    # Obtenir tous les sites
    responses <- get_gen(endpoint)

  } else {

    stopifnot(is.character(ids))

    # Obtenir les sites définis dans ids
    for (id in 1:length(ids)) {

      responses[[id]] <- unlist(get_gen(endpoint, query = list(code = ids[id])),recursive=FALSE)

      if (length(responses[[id]]$body) == 0) {

        message(ids[id], " n'est pas présent dans la base de données")

      } else if (nrow(responses[[id]]$body) > 1) {

        message(nrow(responses[[id]]$body), " entrées ont été retourné par la base de données pour le numéro du site: ",
          ids[id])

      }

    }
  }


  return(responses)

}
