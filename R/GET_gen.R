#' Fonction générique pour retirer de l'information depuis l'API de Coléo
#'
#' @param endpoint `character` désignant le point d'entrée pour le retrait des données. Un point d'entrée peut être vu comme une table de la base de données.
#' @param query `list` de paramètres à passer avec l'appel sur le endpoint.
#' @param flatten `logical` aplatir automatiquement un data.frame imbriqués dans un seul `data.frame` (obsolete si l'objet retourné n'est pas un data.frame)
#' @param output `character` choix du type d'objet retourné: `data.frame`, `list`, `json`
#' @param token  `character` jeton d'accès pour authentification auprès de l'API
#' @param ... httr options; arguments de la fonction `httr::GET()`
#' @return
#' Retourne un objet de type `list` contenant les réponses de l'API. Chaque niveau de la liste correspond à une page. Pour chacun des appels sur l'API (page), la classe retourné est `getSuccess` ou `getError`. Une réponse de classe `getSuccess` est une liste à deux niveaux composé du contenu (`body`), et la réponse [httr::response]. Une réponse de classe `getError` dispose de la même structure mais ne contiendra pas de body, seulement la réponse de l'API.
#' @details
#' Les points d'accès de l'API sont énuméré dans l'environment de coléo, voir `print(endpoints)`
#' @examples
#' \dontrun{
#' resp <- get_gen("/cells")
#' length(resp) # Nombre de pages retourné par l'appel sur le point d'accès de l'API.
#' str(resp[[1]])
#' class(resp[[1]])
#' } 
#' @export

get_gen <- function(endpoint = NULL, query = NULL, flatten = TRUE, output = 'data.frame', token = bearer(),...) {

  stopifnot(exists("endpoint"))

  url <- httr::modify_url(server(), path = paste0(base(), endpoint))

  # On remplace les NAs dans l'objet query avec des NULLs
  if(!is.null(query)) query[which(is.na(query) | query == "NA")] <- NULL

  # Premier appel pour avoir le nombre de page
  resp <- httr::GET(url, config = httr::add_headers(`Content-type` = "application/json",
    Authorization = paste("Bearer", token)),ua, query = query, ...)
  
  # On prépare la liste pour le renvoi de la fonction
  responses <- list()

  # On couvre le code d'erreur 401 (Unauthorized)
  if(httr::status_code(resp) == 401){
    stop(httr::content(resp)$message)
  }

  # Denombrement du nombre de page
  rg <- as.numeric(stringr::str_extract_all(httr::headers(resp)["content-range"],
    simplify=TRUE,
    "\\(?[0-9,.]+\\)?"))

  # Préparation de l'itérateur
  limit <- 100
  pages <- ceiling(rg[3] / limit) - 1
  if(pages < 0) pages <- 0


  # Boucle sur les pages
  for (page in 0:pages) {
    # Ajouter les pages à la requête
    query$page <- page

    resp <- httr::GET(url, config = httr::add_headers(`Content-type` = "application/json",
      Authorization = paste("Bearer", token)), ua, query = query, ...)

    if (httr::http_type(resp) != "application/json") {
      stop("L'API n'a pas retourné un JSON", call. = FALSE)
    }

    # On spécifie le type de sortie
    if(output == 'json'){
      body <- httr::content(resp, type = "text", encoding = "UTF-8")
    } else if(output == 'list') {
      body <- jsonlite::fromJSON(httr::content(resp, type = "text", encoding = "UTF-8"), simplify = FALSE)
    } else if(output == 'data.frame') {
      body <- tibble::as.tibble(jsonlite::fromJSON(httr::content(resp, type = "text", encoding = "UTF-8"), flatten = TRUE, simplifyDataFrame = TRUE))
    }

    # On regarde la longueur du jeu de données renvoyer pour faire les tests logiques
    if(is.data.frame(body)) n_matches <- nrow(body)
    if(is.list(body)) n_matches <- length(body)

    if (httr::http_error(resp)) {
      message(sprintf("La requête sur l'API a échouée: [%s]\n%s", httr::status_code(resp),
        body$message), call. = FALSE)

      responses[[page + 1]] <- structure(list(body = NULL, response = resp),
          class = "getError")

    } else {

      responses[[page + 1]] <- structure(list(body = body, response = resp),
        class = "getSuccess")

    }

    attr(responses[[page + 1]]$body, "n_records") <- n_matches

  }

  return(responses)

}
