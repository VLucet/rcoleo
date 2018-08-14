#' Fonction générique pour convertir les sorties de l'API en objet de classe spatiale (sf)
#'
#' @param responses `list` objet retourné par les fonctions GET du package coléo (ex. get_sites()), voir @details
#' @return
#' Retourne un objet de type `list` contenant les réponses de l'API. Chaque niveau de la liste correspond à une page. Pour chacun des appels sur l'API (page), la classe retourné est `getSuccess` ou `getError`. Une réponse de classe `getSuccess` est une liste à deux niveaux composé du contenu (`body`), et la réponse [httr::response]. Une réponse de classe `getError` dispose de la même structure mais ne contiendra pas de body, seulement la réponse de l'API.
#' @examples
#' resp <- get_sites()
#' plot(cl_to_sf(resp))
#' @details L'objet d'entrée doit contenir les colonnes `geom.type` & `geom.coordinates`
#' @seelso [cells_sf()], [sites_cf()]
#' @export

cl_to_sf.coleoGet <- function(responses = responses) {

  # Check for getSuccess classes
  stopifnot(all(lapply(unlist(responses,recursive=FALSE),class) == "getSuccess"))

  # get features
  features <- lapply(responses, function(request){
    lapply(request, function(page){
      features <- apply(page$body, 1, function(feature){
        return(list(type="Feature", geometry=list(type=feature$geom.type,coordinates=feature$geom.coordinates)))
      })
      names(features) <- NULL
      return(features)
    })
  })

  features <- unlist(unlist(features,recursive=FALSE),recursive=FALSE)

  # get Data
  geom_s <- sf::read_sf(
    jsonlite::toJSON(
      list(
        type="FeatureCollection",
        features=features),
        auto_unbox=TRUE
    )
  )

  geom_df <- lapply(responses, function(request){
    all_pages <- lapply(request, function(page){
      dplyr::select(page$body, -dplyr::one_of("geom.type","geom.coordinates"))
    })
    return(do.call(rbind,all_pages))
  })

  geom_df <- do.call(rbind,geom_df)

  # Data binding
  geom_sdf <- sf::st_sf(dplyr::bind_cols(geom_df,geom_s))

  return(geom_sdf)
}

#' Fonction retournant les cellules dans un objet de classe spatiale (sf)
#'
#' @inheritParams get_cells
#' @param ... arguments de la fonction `httr::GET()` et `rcoleo::get_cells()`
#' @seealso [get_cells()]

cells_sf <- function(cell_code = NULL, name = NULL, ...){
  return(cl_to_sf(get_cells(...)))
}

#' Fonction retournant les sites dans un objet de classe spatiale (sf)
#'
#' @inheritParams get_sites
#' @param ... arguments de la fonction `httr::GET()`
#' @seealso [get_sites()]
sites_sf <- function(site_code = NULL, ...){
  return(cl_to_sf(get_sites(...)))
}

#' Fonction retournant les repères utilisé dans le cadre d'une campagne dans un objet de classe spatiale (sf)
#'
#' @inheritParams get_landmarks
#' @param ... arguments de la fonction `httr::GET()` et `rcoleo::get_sites()`
#' @seealso [get_landmarks()]
landmarks_sf <- function(site_code = NULL, opened_at = NULL, closed_at = NULL, type = NULL, ...){
  return(cl_to_sf(get_landmarks(...)))
}
