#' Obtenir les informations sur les sites depuis l'API de coleo
#' @param site_code est un vecteur contenant les identifiants uniques que l'on désire obtenir. Si site_code n'est pas spécifié, la fonction retournera l'ensemble des entrées présentes dans la table cells.
#' @inheritParams get_gen
#' @examples
#' get_sites(site_code=c('135_104_H01'))
#' get_sites()
#' @export


get_sites <- function(site_code = NULL, ...) {

  responses <- list()
  endpoint <- rce$endpoints$sites

  if (is.null(site_code)) {

    # Obtenir tous les sites
    responses <- get_gen(endpoint)

  } else {

    stopifnot(is.character(site_code))

    # Obtenir les sites définis dans ids
    for (id in 1:length(site_code)) {

      # prep query
      responses[[id]] <- unlist(get_gen(endpoint, query = list(site_code = site_code[id])), recursive=FALSE)

      if (length(responses[[id]]$body) == 0) {

        message(site_code[id], " n'est pas présent dans la base de données")

      } else if (nrow(responses[[id]]$body) > 1) {

        message(nrow(responses[[id]]$body), " entrées pour le numéro du site: ",
          site_code[id])

      }

    }
  }


  return(responses)


}
