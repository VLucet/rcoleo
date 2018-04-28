#' Fonction générique pour retirer de l'information depuis l'API de Coléo
#'
#' @param endpoint Point d'entrée pour le retrait des données. Un point d'entrée peut être vu comme une table de la base de données
#' @return
#' @examples
#' get_gen(endpoints$cells)
#' @export

get_gen <- function(endpoint, ...) {

  stopifnot(exists("endpoint"))

  url <- httr::modify_url(rce$server, path = paste0(rce$base, endpoint))

  # Premier appel pour avoir le nombre de page
  resp <- httr::GET(url, config = httr::add_headers(`Content-type` = "application/json",
    Authorization = paste("Bearer", rce$bearer)),query=list(page=0), rce$ua)

  # Denombrement du nombre de page
  rg <- as.numeric(stringr::str_extract_all(httr::headers(resp)["content-range"],
    simplify=TRUE,
    "\\(?[0-9,.]+\\)?"))

  limit <- rg[2]+1
  pages <- ifelse((rg[3] %% limit) == 0, round(rg[3] / limit)-1, round(rg[3] / limit))
  responses <- list()

  for (page in 0:pages) {

    resp <- httr::GET(url, config = httr::add_headers(`Content-type` = "application/json",
      Authorization = paste("Bearer", rce$bearer)), rce$ua, query=list(page = page))

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

  return(responses)

}
