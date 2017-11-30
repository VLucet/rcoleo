#' Retourne les campagnes d'un site ou un ensemble de sites.
#'
#' @param d une liste contenant pour chaque niveau l'information d'une cellule. L'informations qui peut être saisie sur une cellule est accessible à TODO http://XXX.
#' @return Un objet de la classe \emph{responses} (liste) contenant les codes de status (201 ou 500). Le code 201 signifie que la ou les cellules ont bien été ajouté à la base de données alors que le code 500 siginifie qu'il y a eut une ou des erreurs.
#' @examples
#' getCells(codeName="110_112")
#' getCells()
#' @export

getCells <- function(ids=NULL) {

  endpoint <- rcoleo.env$endpoints$cells

  responses <- sapply(ids, function(x){
    return(get(endpoint,query=list(id = x)))
  })

  class(responses) <- "getCollections"

  return(responses)

}
