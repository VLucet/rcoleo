#' Publication d'une espèce sur la base de données de Coléo
#'
#' Cette fonction applique la méthode POST sur le point d'entrées `species` de l'API de Coleo
#'
#' @inheritParams post_cells
#' @export

post_species <- function(data, ...) {

  # Preparation de l'objet de sortie
  responses <- list()
  class(responses) <- "coleoPostResp"

  endpoint <- endpoints()$species

  for (i in 1:length(data)) {
    responses[[i]] <- post_gen(endpoint, data[[i]], ...)
  }

  return(responses)
}
