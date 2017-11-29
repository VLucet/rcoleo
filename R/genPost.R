


post <- function (endpoint,singleton) {

  url <- httr::modify_url(server, path = paste0("/api/v0",endpoint))
  response <- httr::POST(url, body = jsonlite::toJSON(singleton,auto_unbox=TRUE), config = httr::add_headers("Content-type" = "application/json", "Authorization" = paste('Bearer',bearer)))

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "github_api"
  )

}
