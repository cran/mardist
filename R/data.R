#' Maritime network
#'
#' An igraph edge list that describes the global maritime network.
#' The object is a data frame with columns `cluster`, `lag_cluster`,
#' and `distance` (in metres).
#'
#' @format A data.frame with N rows and 3 columns.
#' @source JRC
#' @docType data
#' @name network
NULL

#' Cluster coordinates
#'
#' Latitude and longitude of the maritime clusters used in the network.
#'
#' @format A data.frame with columns `cluster`, `latitude`, `longitude`.
#' @docType data
#' @name cluster_coordinates
NULL
