#' Retourne les campagnes d'un site ou un ensemble de sites.
#'
#' @param id Un vecteur contenant la liste des codes de sites.
#' @return Un objet de la classe \emph{data.frame} (tableau) contenant la liste des campagnes. Si le vecteur \emph{id} est d'une dimension supérieur à 1, une liste sera retourné. Chaque niveau de la liste correspondra à un site en particulier avec à l'intérieur un tableau decrivant les campagnes attachées à ce site.
#' @examples
#' get_campaigns()
#' @export


post_sites <- function(data) {
  endpoint <- rce$endpoints$sites
  # Create resp
  responses <- list()


  # Retrieve the ids from cells
  # for (i in 1:length(data)) {
  #   if()
  #   data[[i]]$id <- paste0(data[[i]]$cell_id)
  #
  # }

  for (i in 1:length(data)) {
        # Get cell_code for the site
        # Store responses in "resp" list
        responses[[i]] <- post_gen(endpoint, data[[i]])
  }

  return(responses)
}
