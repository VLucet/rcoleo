#' Retourne les campagnes d'un site ou un ensemble de sites.
#'
#' @param id Un vecteur contenant la liste des codes de sites.
#' @return Un objet de la classe \emph{data.frame} (tableau) contenant la liste des campagnes. Si le vecteur \emph{id} est d'une dimension supérieur à 1, une liste sera retourné. Chaque niveau de la liste correspondra à un site en particulier avec à l'intérieur un tableau decrivant les campagnes attachées à ce site.
#' @examples
#' getCampaigns()
#' @export

postSites <- function(lt) {
  endpoint <- "/sites"
  # Create resp
  responses <- list()

  for (i in 1:length(lt)) {
        # Get cell_code for the site
        # Store responses in "resp" list
        responses[[i]] <- post(endpoint, lt[[i]])
        if (httr::http_error(responses[[i]])) {
          stop(
            cat(
            "Error code",httr::status_code(responses[[i]]),"\n",
            httr::content(responses[[i]], "text"), "\n"
          ))
        }
  }
  return(responses)
}
