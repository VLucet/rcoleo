#' Fonction générique pour retirer de l'information depuis l'API de Coléo
#'
#' @param endpoint Point d'entrée pour le retrait des données. Un point d'entrée peut être vu comme une table de la base de données
#' @return
#' @examples
#' get_gen(endpoints$cells)
#' @export

get_gen <- function (endpoint,...) {

  if(!exists("endpoint")){
    stop("Le point d'accès au données est manquant (ex. /cells)")
  }

  url <- httr::modify_url(
     rce$server,
     path=paste0(rce$base,endpoint)
  )

  resp <- httr::GET(url,
    config = httr::add_headers(
      "Content-type" = "application/json",
      "Authorization" = paste('Bearer',rce$bearer)),
      ...)

  cont <- jsonlite::fromJSON(httr::content(resp,type="text"),flatten=TRUE)

  if(!httr::http_error(resp)){
    return
      structure(
        list(
          content = cont,
          response = resp
        ),
          class = "getSuccess"
        )
  } else {
    return
      structure(
        list(
          response = resp
        ),
          class = "getError"
        )
  }

}
