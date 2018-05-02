#' Obtenir les informations sur les cellules depuis l'API de coleo
#'
#' @inherit get_ids
#' @examples
#' get_cells(ids=c('111_91'))
#' get_cells()
#' @export

get_cells <- function(ids = NULL, ...) {

  return(get_ids(ids, rce$endpoints$cells, ...))

}
