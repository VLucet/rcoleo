% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/POST_campaigns.R
\name{post_campaigns}
\alias{post_campaigns}
\title{Publication d'une campagne sur la base de données de Coléo}
\usage{
post_campaigns(data, ...)
}
\arguments{
\item{data}{une liste ou chacun des niveaux corresponds aux données attribuées au ednpoint.}

\item{...}{httr options; arguments de la fonction \code{httr::POST()}}
}
\description{
Cette fonction applique la méthode POST sur le point d'entrées \code{campaigns} de l'API de Coleo
}
