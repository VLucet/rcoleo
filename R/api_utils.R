#' Transformer la liste de réponses en data.frame
#'
#' @param responses liste de réponse des fonctions de GET/POST
#' @export

as.data.frame.coleoGetResp <- function(responses=responses){

  stopifnot(is.coleoGetResp(responses))

  # Loop over query
  all_body <- lapply(responses, function(query){
    # if query contains multipages
    if(length(query) > 1){
      return(do.call(
        plyr::rbind.fill,
        lapply(query, function(page){
         return(page$body)
        })
      ))
    } else {
      if(length(query[[1]]$body) == 0){
        return(NULL)
      } else {
        return(query[[1]]$body)
      }
    }
  })

  return(as.data.frame(do.call(plyr::rbind.fill, all_body)))
}

#' Test sur la classe `coleoGetResp` (Objet S3)
#' @param x Objet à tester
#' @export
is.coleoGetResp <- function(x) inherits(x,'coleoGetResp')

#' Test sur la classe `coleoPostResp` (Objet S3)
#' @param x Objet à tester
#' @export
is.coleoPostResp <- function(x) inherits(x,'coleoPostResp')
