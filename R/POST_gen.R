#' Fonction générique pour envoyer de l'information vers l'API de Coléo
#'
#' @param endpoint Point d'entrée pour le retrait des données.
#' @param singleton Données à injecter à l'intérieur de la base de données.
#' @return
#' @examples
#' data(cells)
#' post_gen("/cells",cells[1])
#' # Note: Cette fonction ne prend que des singletons
#' @export

post_gen <- function (endpoint,singleton, ...) {

  if(!exists("endpoint")){
    stop("Le point d'accès au données est manquant (ex. /cells)")
  }

  url <- httr::modify_url(
     rce$server,
     path=paste0(rce$base,endpoint)
  )

  resp <- httr::POST(url,
    body = jsonlite::toJSON(singleton,auto_unbox=TRUE),
    config = httr::add_headers(
      "Content-type" = "application/json",
      "Authorization" = paste('Bearer',rce$bearer)),
      ...)

  if(resp$status == 401){
    structure(
    list(
      body = httr::http_status(resp),
      response = resp
    ),
      class = "postUnAuthorized"
    )
  }
  else if(resp$status == 400){
    structure(
    list(
      body = jsonlite::fromJSON(httr::content(resp, "text")),
      response = resp
    ),
      class = "postUnvalidate"
    )
  } else {
    structure(
    list(
      body = singleton,
      response = resp
    ),
      class = "postSuccess"
    )
  }
}
