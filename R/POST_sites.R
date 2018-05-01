#' Publication d'un site sur la base de données de Coléo
#'
#' Cette fonction applique la méthode POST sur le point d'entrées \code{sites} de l'API de Coleo
#' @param data une liste ou chacun des niveaux corresponds aux données attribuées à un site.
#' @return Un objet \code{list}, dont chacun des niveaux corresponds à la réponse de l'API. La réponse peut être de classe \code{postError} ou \code{postSuccess}.
#' @examples
#' post_sites(list(list(cell_id = 1,code_site = "111_140_H01", date_open = "2017-08-31", type="forestier", off_station_code_id=NA)))
#' @seealso \code{\link{post_gen}} pour la structure de sortie de la fonction.
#' @export


post_sites <- function(data) {

  endpoint <- rce$endpoints$sites

  # Create resp
  responses <- list()

  for (i in 1:length(data)) {
    # On retourne l'id unique de la table cell
    # Le unlist c'est pour les pages, mais je sais que la réponse contient une seule page (match sur un code)
    data[[i]]$cell_id <- unlist(get_cells(data[[i]]$cell_id),recursive=FALSE)$body[,c("id")]
    responses[[i]] <- post_gen(endpoint, data[[i]])
  }

  return(responses)
}
