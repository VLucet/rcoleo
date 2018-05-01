#' Retourne les campagnes d'un site ou un ensemble de sites.
#'
#' @param data Une liste
#' @return Un objet de la classe \emph{data.frame} (tableau) contenant la liste des campagnes. Si le vecteur \emph{id} est d'une dimension supérieur à 1, une liste sera retourné. Chaque niveau de la liste correspondra à un site en particulier avec à l'intérieur un tableau decrivant les campagnes attachées à ce site.
#' @examples
#' site <- list()
#' post_sites(site)
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
