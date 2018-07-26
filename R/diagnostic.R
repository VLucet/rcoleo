#' Effectuer un diagnotic rapide de l'ensemble des appels sur l'API
#'
#' @param responses liste de réponse des fonctions de GET/POST
#' @export

diagnostic <- function(responses=responses){
  do.call(plyr::rbind.fill,lapply(responses,function(x) as.data.frame(httr::http_status(x$response))))
}
