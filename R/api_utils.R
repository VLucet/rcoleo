#' Effectuer un diagnotic rapide de l'ensemble des appels sur l'API
#'
#' @param responses liste de réponse des fonctions de GET/POST
#' @export

diagnostic <- function(responses=responses){
  do.call(plyr::rbind.fill,lapply(responses,function(x) as.data.frame(httr::http_status(x$response))))
}

#' Transformer la liste de réponses en data.frame
#'
#' @param responses liste de réponse des fonctions de GET/POST
#' @export

as.data.frame.coleoGetResp <- function(responses=responses){

  stopifnot(is.coleoGet(responses))

  # Loop over query
  all_body <- lapply(responses, function(query){
    query <- responses[[1]]
    # if query contains multipages
    if(length(query) > 1){
      return(do.call(plyr::rbind.fill,lapply(query, function(page) return(page$body))))
    } else {
      return(query[[1]]$body)
    }
  })

  return(as.data.frame(do.call(plyr::rbind.fill, all_body)))

}

#' S3 Validators

is.coleoGet <- function(x) inherits(x,'coleoGetResp')
is.coleoPost <- function(x) inherits(x,'coleoPostResp')
