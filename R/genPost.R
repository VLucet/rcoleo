#' Fonction générique pour envoyer de l'information vers l'API de Coléo
#'
#' @param endpoint Point d'entrée pour le retrait des données.
#' @param singleton Données à injecter à l'intérieur de la base de données.
#' @return
#' @examples
#' # TODO: Mettre un exemple de schema de données dans un RDS
#' get(endpoints$cells)
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


  if(resp$status_code == 201){

    structure(
    list(
      body = singleton
      path = url,
      response = resp
    ),
      class = "postSuccess"
    )

  } else {

    structure(
    list(
      body = jsonlite::fromJSON(httr::content(test$response, "text")),
      path = url,
      response = resp
    ),
      class = "postError"
    )

  }
}
