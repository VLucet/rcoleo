#' Obtenir les informations sur les sites depuis l'API de coleo
#'
#' @inherit get_ids
#' @examples
#' get_sites(ids=c('135_104_H01'))
#' get_sites()
#' @export


get_sites <- function(ids = NULL, ...) {

  return(get_ids(ids, rce$endpoints$sites, ...))

}
