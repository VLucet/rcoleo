#' Fonction générique pour retirer de l'information depuis l'API de Coléo
#'
#' @param endpoint Point d'entrée pour le retrait des données. Un point d'entrée peut être vu comme une table de la base de données. Les points d'entrées sont listé dans l'environnement `rce` du paquet rcoleo.
#' @param ... Arguments des fonctions supérieur ou arguments de la fonction generique `httr::GET`
#' @return
#' Retourne une `list` contenant les réponses de l'API. Chaque niveau de la liste correspond à une page. Pour chacun des appels sur l'API (page), la classe retourné est `getSuccess` ou `getError`. Une réponse de classe `GetSuccess` est une liste à deux niveaux composé du contenu (`content`), et la réponse (objet de classe `response`, paquet `httr`)
#' @examples
#' resp <- get_gen(rce$endpoints$cells)
#' length(resp) # Nombre de pages retourné par l'appel.
#' str(resp[[1]])
#' class(resp[[1]])
#' @export

get_gen <- function(endpoint, ...) {

  stopifnot(exists("endpoint"))

  url <- httr::modify_url(rce$server, path = paste0(rce$base, endpoint))

  # Premier appel pour avoir le nombre de page
  resp <- httr::GET(url, config = httr::add_headers(`Content-type` = "application/json",
    Authorization = paste("Bearer", rce$bearer)),rce$ua, ... )

  # Denombrement du nombre de page
  rg <- as.numeric(stringr::str_extract_all(httr::headers(resp)["content-range"],
    simplify=TRUE,
    "\\(?[0-9,.]+\\)?"))

  limit <- rg[2]+1
  pages <- ifelse((rg[3] %% limit) == 0, round(rg[3] / limit)-1, round(rg[3] / limit))
  responses <- list()

  for (page in 0:pages) {

    # Ajouter les pages à la requête
    query$page <- page

    resp <- httr::GET(url, config = httr::add_headers(`Content-type` = "application/json",
      Authorization = paste("Bearer", rce$bearer)), rce$ua, ...)

    if (httr::http_type(resp) != "application/json") {
      stop("L'API n'a pas retourné un JSON", call. = FALSE)
    }

    parsed <- jsonlite::fromJSON(httr::content(resp, type = "text"), flatten = TRUE)

    if (httr::http_error(resp)) {
      stop(sprintf("La requête sur l'API a échouée: [%s]\n%s", status_code(resp),
        parsed$message), call. = FALSE)
    }

    responses[[page + 1]] <- structure(list(content = parsed, response = resp),
      class = "getSuccess")

  }

 # Si une seule page pas besoin de le transmettre en list
  return(responses)

}
