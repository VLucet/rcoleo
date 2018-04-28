#' Retourne les sites d'une cellule.
#'
#' @param d une liste contenant pour chaque niveau l'information d'une cellule. L'informations qui peut être saisie sur une cellule est accessible à TODO http://XXX.
#' @return Un objet de la classe \emph{responses} (liste) contenant les codes de status (201 ou 500). Le code 201 signifie que la ou les cellules ont bien été ajouté à la base de données alors que le code 500 siginifie qu'il y a eut une ou des erreurs.
#' @examples
#' get_sites()
#' @export

get_sites <- function(query = NULL,...) {

  responses <- list()
  endpoint <- rce$endpoints$sites


  if (is.null(ids)) {

    # Obtenir toutes les cellules
    responses <- get_gen(endpoint)

  } else {

    stopifnot(is.character(query))

    # Obtenir des cellules specifiques (ids)
    for (c in 1:length(ids)) {

      responses[[c]] <- get_gen(endpoint, query = list(q = query[c]))

      if (length(responses[[c]]$content) == 0) {

        message(ids[c], " n'est pas présent dans la base de données")

      } else if (nrow(responses[[c]]$content) > 1) {

        message(nrow(responses[[c]]$content), " entrées ont été retourné par la base de données pour le numéro du site: ",
          ids[c])

      }

    }
  }


  return(responses)

}
