#' Fonction générique pour envoyer de l'information vers l'API de Coléo
#'
#' @param endpoint Point d'entrée pour le retrait des données.
#' @param singleton Données à injecter à l'intérieur de la base de données.
#' @return
#' Retourne une objet de type `list` contenant les réponses de l'API. Chaque niveau de la liste correspond à la réponse de l'API. Pour chacun des appels POST sur l'API, la classe retourné est `postSuccess` ou `postError`. Une réponse de classe `postSuccess` ou `postError` est une liste à deux niveaux composée du `body`, et de la réponse [httr::response-class]. Si le réponse est de classe `postSucces`, le `body` contiendra les données injectées sinon il contiendra le message d'erreur retourné par l'API.
#' @export

post_gen <- function(endpoint, singleton, ...) {

  if (!exists("endpoint")) {
    stop("Le point d'accès aux données est manquant (ex. /cells)")
  }

  url <- httr::modify_url(rce$server, path = paste0(rce$base, endpoint))

  resp <- httr::POST(url, body = jsonlite::toJSON(singleton, auto_unbox = TRUE),
    config = httr::add_headers(`Content-type` = "application/json", Authorization = paste("Bearer",
      rce$bearer)), ...)

  if (resp$status == 401) {

    structure(list(body = httr::http_status(resp), response = resp), class = "postError")

  } else if (resp$status == 400) {

    structure(list(body = jsonlite::fromJSON(httr::content(resp, "text")), response = resp),
      class = "postError")

  } else if (resp$status == 201) {

    structure(list(body = singleton, response = resp), class = "postSuccess")

  } else if (resp$status == 500) {

    structure(list(body = jsonlite::fromJSON(httr::content(resp, "text")), response = resp),
      class = "postError")

  }
}
