#' Fonction générique pour retirer de l'information depuis l'API de Coléo
#'
#' @param endpoint Point d'entrée pour le retrait des données. Un point d'entrée peut être vu comme une table de la base de données
#' @return
#' @examples
#' get(endpoints$cells)
#' @export

get <- function (endpoint, ...) {

  url <- httr::modify_url(
     rcoleo.env$dev$server,
     path=paste0(rcoleo.env$base,endpoint)
  )

  resp <- httr::GET(url,
    config = httr::add_headers(
      "Content-type" = "application/json",
      "Authorization" = paste('Bearer',rcoleo.env$bearer)),
      ...)

  cont <- httr::content(resp)

  structure(
  list(
    content = cont,
    path = url,
    response = resp
  ),
    class = "getColeo"
  )
}
