#' Publication d'un site sur la base de données de Coléo
#'
#' Cette fonction applique la méthode POST sur le point d'entrées `sites` de l'API de Coleo
#'
#' @inheritParams post_cells
#' @export

post_sites <- function(data, ...) {

  responses <- list()
  endpoint <- rce$endpoints$sites

  for (i in 1:length(data)) {
    # On retourne l'id unique de la table cell
    # Le unlist c'est pour les pages, mais je sais que la réponse contient une seule page (match sur un code)
    data[[i]]$cell_id <- unlist(get_cells(data[[i]]$cell_id),recursive=FALSE)$body[,c("id")]
    responses[[i]] <- post_gen(endpoint, data[[i]], ...)
  }

  return(responses)
}
