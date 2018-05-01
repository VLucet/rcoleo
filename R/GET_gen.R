#' Fonction générique pour retirer de l'information depuis l'API de Coléo
#'
#' @param endpoint Point d'entrée pour le retrait des données. Un point d'entrée peut être vu comme une table de la base de données.
#' @param ... Arguments de la fonction generique \link[httr]{GET}
#' @return
#' Retourne une objet de type \code{list} contenant les réponses de l'API. Chaque niveau de la liste correspond à une page. Pour chacun des appels sur l'API (page), la classe retourné est \code{getSuccess} ou \code{getError}. Une réponse de classe \code{getSuccess} est une liste à deux niveaux composé du contenu (\code{body}), et la réponse (objet de classe \code{response}, paquet \code{httr}). Une réponse de classe \code{getError}.
#' @details
#' Les points d'accès de l'API sont énuméré dans l'environment de coléo, voir \code{print(rce$endpoints)}
#' @examples
#' resp <- get_gen(rce$endpoints$cells)
#' length(resp) # Nombre de pages retourné par l'appel sur le point d'accès de l'API.
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

  if(!exists("query")) query <- list()
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
      message(sprintf("La requête sur l'API a échouée: [%s]\n%s", status_code(resp),
        parsed$message), call. = FALSE)

      responses[[page + 1]] <- structure(list(body = NULL, response = resp),
          class = "getError")

    } else {
      responses[[page + 1]] <- structure(list(body = parsed, response = resp),
        class = "getSuccess")
    }

  }

 # Si une seule page pas besoin de le transmettre en list
  return(responses)

}
