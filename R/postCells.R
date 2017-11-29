#' Retourne les campagnes d'un site ou un ensemble de sites.
#'
#' @param d une liste contenant pour chaque niveau l'information d'une cellule. L'informations qui peut être saisie sur une cellule est accessible à TODO http://XXX.
#' @return Un objet de la classe \emph{responses} (liste) contenant les codes de status (201 ou 500). Le code 201 signifie que la ou les cellules ont bien été ajouté à la base de données alors que le code 500 siginifie qu'il y a eut une ou des erreurs.
#' @examples
#' postCampaigns(list(list(
#'
#' )))
#' print(postCampaigns())
#' @export

postCells <- function(d) {
  # Create resp
  responses <- list()

  if(!is.list(d)){
    stop("TODO")
  }

  for (i in 1:length(d)) {
        # Store responses in "resp" list
        responses[[i]] <- post(rcoleo.env$endpoints$cells, d[[i]])

        if (httr::http_error(responses[[i]])) {
          stop(
            cat(
            "Error code",httr::status_code(responses[[i]]),"\n",
            httr::content(responses[[i]], "text"), "\n"
          ))
        }
  }

  class(responses) <- "responses"

  return(responses)

}
