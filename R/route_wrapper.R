#' Map a single maritime route (convenient short name)
#'
#' Wrapper that forwards to `map_route`.  It exists only so that the
#' examples can use the short name `route_map`.
#'
#' @param lon1 longitude of the origin (decimal degrees)
#' @param lat1 latitude of the origin (decimal degrees)
#' @param lon2 longitude of the destination (decimal degrees)
#' @param lat2 latitude of the destination (decimal degrees)
#' @return A `leaflet` map object.
#' @export
route_map <- function(lon1, lat1, lon2, lat2) {
  ## `map_route` expects a list: the first element is the route data.table,
  ## the second element is the nautical mile estimate (which we do not need
  ## for the visualisation).  We therefore call `find_route` first.
  rr <- find_route(
    graph = gg,
    V1 = find_closest_cluster(lon1, lat1),
    V2 = find_closest_cluster(lon2, lat2),
    extra_distance = 0L
  )
  ## `rr[[1]]` is the data.table that `map_route` works with.
  #map_route(list(rr[[1]], NA))
  map_route(list(rr[[1]],rr[[2]]))
}
