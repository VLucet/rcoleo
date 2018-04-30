#' Retourne les campagnes d'un site ou un ensemble de sites.
#'
#' @param d une liste contenant pour chaque niveau l'information d'une cellule. L'informations qui peut être saisie sur une cellule est accessible à TODO http://XXX.
#' @return Un objet de la classe \emph{responses} (liste) contenant les codes de status (201 ou 500). Le code 201 signifie que la ou les cellules ont bien été ajouté à la base de données alors que le code 500 siginifie qu'il y a eut une ou des erreurs.
#' @examples
#' get_cells(ids=c('111_91'))
#' get_cells()
#' @export

get_cells <- function(ids = ids,...) {

  responses <- list()
  endpoint <- rce$endpoints$cells


  if (length(ids)==0) {

    # Obtenir toutes les cellules
    responses <- get_gen(endpoint)

  } else {

    stopifnot(is.character(ids))

    # Obtenir des cellules specifiques (query)
    for (id in 1:length(ids)) {

      responses[[id]] <- unlist(get_gen(endpoint, query = list(code = ids[id])),recursive=FALSE)

      if (nrow(responses[[id]]$content) == 0) {

        message(ids[id], " n'est pas présent dans la base de données")

      } else if (nrow(responses[[id]]$content) > 1) {

        message(nrow(responses[[id]]$content), " entrées ont été retourné par la base de données pour le numéro de cellule: ",
          ids[id])

      }

    }
  }


  return(responses)

}
