#' Effectuer un diagnotic rapide de l'ensemble des appels sur l'API
#'
#' @param responses liste de réponse des fonctions de GET/POST
#' @export

diagnostic <- function(responses=responses){

  status <- list()

  for(resp in seq_len(length(responses))){
    for(q in seq_len(length(resp))){
        status[[length(status)+1]] <- as.data.frame(httr::http_status(responses[[resp]][[q]]$response))
    }
  }

  plyr::rbind.fill(status)

}
