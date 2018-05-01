#' Publication d'une campagne attachée à un site sur la base de données de Coléo
#'
#' Cette fonction applique la méthode POST sur le point d'entrées \code{campagnes} de l'API de Coleo
#' @param data une liste ou chacun des niveaux corresponds aux données attribuées à une campagne.
#' @return Un objet \code{list}, dont chacun des niveaux corresponds à la réponse de l'API. La réponse peut être de classe \code{postError} ou \code{postSuccess}.
#' @examples
#' post_campaigns()
#' @seealso \code{\link{post_gen}} pour la structure de sortie de la fonction.
#' @export

post_campaigns <- function(data) {

  endpoint <- rce$endpoints$campaigns

  # Create resp
  responses <- list()

  for (i in 1:length(data)) {
    # On retourne l'id unique de la table cell
    # Le unlist c'est pour les pages, mais je sais que la réponse contient une seule page (match sur un code)
    data[[i]]$site_id <- unlist(get_sites(data[[i]]$site_code),recursive=FALSE)$body[,c("id")]
    responses[[i]] <- post_gen(endpoint, data[[i]])
  }

  return(responses)
}
