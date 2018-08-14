#' Fonction retournant les cellules dans un objet de classe spatiale (sf)
#'
#' @inheritParams get_cells
#' @param ... arguments de la fonction `httr::GET()`
#' @seealso [get_cells()]
#' @export

sf_cells <- function(cell_code = NULL, name = NULL, ...){
  return(cl_to_sf(get_cells(...)))
}

#' Fonction retournant les sites dans un objet de classe spatiale (sf)
#'
#' @inheritParams get_sites
#' @param ... arguments de la fonction `httr::GET()`
#' @seealso [get_sites()]
#' @export
sf_sites <- function(site_code = NULL, ...){
  return(cl_to_sf(get_sites(...)))
}

#' Fonction retournant les repères utilisé dans le cadre d'une campagne dans un objet de classe spatiale (sf)
#'
#' @inheritParams get_landmarks
#' @param ... arguments de la fonction `httr::GET()`
#' @seealso [get_landmarks()]
#' @export
sf_landmarks <- function(site_code = NULL, opened_at = NULL, closed_at = NULL, type = NULL, ...){
  return(cl_to_sf(get_landmarks(...)))
}
