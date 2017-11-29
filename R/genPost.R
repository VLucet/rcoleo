#' Fonction générique pour envoyer de l'information vers l'API de Coléo
#'
#' @param endpoint Point d'entrée pour le retrait des données.
#' @param singleton Données à injecter à l'intérieur de la base de données.
#' @return
#' @examples
#' data(cells)
#' post("/cells",cells[[1]])
#' # Note: Cette fonction ne prend que des singletons
#' @export

post <- function (endpoint,singleton, ...) {

  if(!exists("endpoint")){
    stop("Le point d'accès au données est manquant (ex. /cells)")
  }

  url <- httr::modify_url(
     rcoleo.env$dev$server,
     path=paste0(rcoleo.env$base,endpoint)
  )

  resp <- httr::POST(url,
    body = jsonlite::toJSON(singleton,auto_unbox=TRUE),
    config = httr::add_headers(
      "Content-type" = "application/json",
      "Authorization" = paste('Bearer',rcoleo.env$bearer)),
      ...)


  if(httr::http_error(resp)){
    structure(
    list(
      body = jsonlite::fromJSON(httr::content(test$response, "text")),
      response = resp
    ),
      class = "postError"
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
