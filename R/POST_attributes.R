#' Publication d'attribut(s) dans la base de données de Coléo
#'
#' Cette fonction applique la méthode POST sur le point d'entrées `attributes` de l'API de Coleo
#'
#' @inheritParams post_cells
#' @export

post_attributes <- function(data, ...) {

  responses <- list()
  endpoint <- rce$endpoints$attributes

  for (i in 1:length(data)) {
    responses[[i]] <- post_gen(endpoint, data[[i]], ...)
  }

  return(responses)
}
