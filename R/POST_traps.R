#' Publication d'un piège dans l'API de coléo
#'
#' Cette fonction applique la méthode POST sur le point d'entrées `traps` de l'API de Coleo
#'
#' @inheritParams post_cells
#' @export

post_traps <- function(data, ...) {

  # Preparation de l'objet de sortie
  responses <- list()
  class(responses) <- "coleoPostResp"

  endpoint <- endpoints()$traps

  for (i in 1:length(data)) {
    # On retourne l'id unique pour la campagne à laquelle est rattaché les trappes
    # Le unlist c'est pour les pages, mais je sais que la réponse contient une seule page (match sur un code)
    campaign_id <- as.data.frame(get_campaigns(site_code=data[[i]]$site_code,opened_at=data[[i]]$opened_at,closed_at=data[[i]]$closed_at,type="insectes_sol"))$id
    stopifnot(length(campaign_id) == 1)

    campaign_id <- data[[i]]$campaign_id

    responses[[i]] <- post_gen(endpoint, data[[i]], ...)
  }

  return(responses)
}
