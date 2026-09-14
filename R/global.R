#' @importFrom utils globalVariables
NULL

utils::globalVariables(
  c(
    "distance", # column created / read by find_closest_cluster &
    "lat", "lon", "lat_1", "lon_1", # columns used inside find_route
    "V", "cluster", "lon_1", "lat_1" # any other column names you reference
  )
)
