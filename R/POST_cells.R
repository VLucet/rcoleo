#' Retourne les campagnes d'un site ou un ensemble de sites.
#'
#' @param data une liste contenant pour chaque niveau l'information d'une cellule. L'informations qui peut être saisie sur une cellule est accessible à TODO http://XXX.
#' @return Un objet de la classe \emph{responses} (liste) contenant les codes de status (201 ou 500).
#' @examples
#' data(cells)
#' post_cells(cells[1:10])
#' @export

post_cells <- function(data) {

  responses <- list()
  endpoint <- rce$endpoints$cells

  for (i in 1:length(data)) {
    responses[[i]] <- post_gen(endpoint, data[[i]])
  }

  class(responses) <- "postCollections"

  return(responses)

}
