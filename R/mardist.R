utils::data("network", "cluster_coordinates", package = "mardist", envir = parent.env(environment()))


### map limits

lon_max <- 179.9999
lon_min <- (-179.9999)
lat_max <- 89.9999
lat_min <- (-89.9999)

#' Haversine great circle distance
#'
#' Computes the great circle distance between two points on the Earth
#' surface using the Haversine formula.  The result is returned in the
#' same linear unit as the radius r that you supply (the default is the
#' WGS 84 Earth radius in metres).
#'
#' @param lat_from numeric. Latitude of the *from* point (decimal degrees).
#' @param lon_from numeric. Longitude of the *from* point (decimal degrees).
#' @param lat_to   numeric. Latitude of the *to* point (decimal degrees).
#' @param lon_to   numeric. Longitude of the *to* point (decimal degrees).
#' @param r        numeric. Radius of the sphere used for the calculation.
#'                 The default value 6378137  is the WGS84 equatorial radius in
#'                 metres.
#' @return The distance between the two points, expressed in the same unit
#'         as r (metres by default).
#' @examples
#' ## Distance between Shanghai (121.48 E, 31.2198 N) and
#' ## Goa (79.85 E, 6.95 N) the value is given in metres.
#' dtHaversine(31.2198, 121.48, 6.95, 79.85)
#' @export
dtHaversine <- function(lat_from, lon_from, lat_to, lon_to, r = 6378137) {
  radians <- as.numeric(pi / 180)
  lat_to <- as.numeric(lat_to) * radians
  lat_from <- as.numeric(lat_from) * radians
  lon_to <- as.numeric(lon_to) * radians
  lon_from <- lon_from * radians
  dLat <- (lat_to - lat_from)
  dLon <- (lon_to - lon_from)
  a <- (sin(dLat / 2)^2) + (cos(lat_from) * cos(lat_to)) * (sin(dLon / 2)^2)
  return(2 * atan2(sqrt(a), sqrt(1 - a)) * r)
}


### Shortest route

find_route <- function(graph, V1, V2, extra_distance) {
  aa <- igraph::shortest_paths(
    graph,
    from = V1,
    to = V2,
    mode = "all",
    weights = igraph::E(graph)$distance,
    output = "vpath",
    predecessors = FALSE,
    inbound.edges = FALSE,
    algorithm = "automatic"
  )


  route <- data.table::as.data.table(cbind(V = as.vector(aa$vpath[[1]])))
  route <- dplyr::left_join(route, dt, by = "V")
  if (nrow(route) > 1) {
    route$lon_1[2:nrow(route)] <- route$lon[1:(nrow(route) - 1)]
    route$lat_1[2:nrow(route)] <- route$lat[1:(nrow(route) - 1)]


    route[, distance := dtHaversine(lat, lon, lat_1, lon_1)]
    route$distance <- round(route$distance / 1852, 1) # convert into nautical miles
    distance <- 10 * round((sum(route$distance, na.rm = T) + extra_distance) / 10, 0)

    if (distance < 30) {distance <- 30}
  } else {distance <- 30}

  return(list(route, distance))
}


map_route <- function(x) {
  route <- x[[1]]
  # antimeridian
  route <- route[!is.na(route$lon_1), ]
  route$lon[route$lon_1 > 100 & route$lon < (-100)] <- 179
  route$lon[route$lon_1 < (-100) & route$lon > 100] <- (-179)
  #

  route_map <- leaflet::leaflet(route) %>%
    addTiles()

  for (i in 1:nrow(route)) {
    route_map <- route_map %>%
      leaflet::addPolylines(
        lng = c(route$lon[i], route$lon_1[i]),
        lat = c(route$lat[i], route$lat_1[i],
          weight = 1
        )
      )
  }

  route_map <- route_map %>%
    leaflet::addCircleMarkers(lng = route$lon[1], lat = route$lat[1], popup = FALSE) %>%
    leaflet::addCircleMarkers(lng = route$lon[nrow(route)], lat = route$lat[nrow(route)], color = "Red", popup = FALSE)

  return(route_map)
}


### find closest graph node

find_closest_cluster <- function(lon, lat) {
  set_points <- dt
  lat_1 <- lat
  lon_1 <- lon

  set_points[, distance := dtHaversine(lat, lon, lat_1, lon_1) / 1000 / 1.852] ### nn$distance in nautical miles  # dtHaversine comes from source("~/emotos/code/dtHaversine_function.R")
  closest <- set_points$V[set_points$distance == min(set_points$distance)][1]
  return(closest)
}


network <- data.table::as.data.table(network) ### from build_network.r


cluster_coordinates <- data.table::as.data.table(cluster_coordinates, strings.as.factors = FALSE)

nn <- as.data.frame(cbind(cluster = network$cluster, lag_cluster = network$lag_cluster))
gg <- igraph::graph_from_data_frame(nn, directed = FALSE)
igraph::E(gg)$distance <- network$distance

dt <- as.data.frame(cbind(V = c(1:length(igraph::V(gg))), cluster = names(igraph::V(gg))))
dt$cluster <- as.integer(dt$cluster)
dt$V <- as.integer(dt$V)
dt <- dplyr::left_join(dt, cluster_coordinates, by = "cluster")
dt <- data.table::as.data.table(dt)


#' Distance calculation
#'
#' Calculates the maritime distance between two geographic points.
#'
#' @param lon1 numeric. Longitude of the origin (decimal degrees).
#' @param lat1 numeric. Latitude  of the origin (decimal degrees).
#' @param lon2 numeric. Longitude of the destination (decimal degrees).
#' @param lat2 numeric. Latitude  of the destination (decimal degrees).
#' @return The distance in nautical miles (numeric).
#' @examples
#' distance_route(121.48, 31.2198, 79.85, 6.95)
#' @export
distance_route <- function(lon1, lat1, lon2, lat2) {
  point_1 <- find_closest_cluster(lon1, lat1)
  point_2 <- find_closest_cluster(lon2, lat2)

  # if (point_1[1]==point_2[1]){sh_route=30} else {
  distance_1 <- as.numeric(dtHaversine(dt$lat[dt$V == point_1], dt$lon[dt$V == point_1], lat1, lon1) / 1000 / 1.852)
  distance_2 <- as.numeric(dtHaversine(dt$lat[dt$V == point_2], dt$lon[dt$V == point_2], lat2, lon2) / 1000 / 1.852)
  extra_distance <- distance_1 + distance_2
  sh_route <- as.integer(find_route(gg, point_1, point_2, extra_distance)[[2]]) # }
  return(sh_route)
}


#' Map several maritime routes from a common origin
#'
#' Given a single origin (`lon1`, `lat1`) and a data frame / data.table
#' (`clist`) that contains destination coordinates, the function draws a
#' **leaflet** map with a separate shortest route line for each destination.
#'
#' @param lon1 numeric. Longitude of the origin (decimal degrees).
#' @param lat1 numeric. Latitude  of the origin (decimal degrees).
#' @param clist data.frame or data.table. Must contain columns
#'   `lon` and `lat`.  Additional columns are ignored.
#' @return A `leaflet` map object displaying all routes.
#' @examples
#' destinations <- data.frame(lon = c(80, 85), lat = c(7, 10))
#' multi_route_map(121.48, 31.2198, destinations)
#' @export
multi_route_map <- function(lon1, lat1, clist) {
  suppressWarnings({
    # addTiles()
    route_map <- leaflet::leaflet()

    for (k in 1:nrow(clist)) {
      point_1 <- find_closest_cluster(lon1, lat1)
      point_2 <- find_closest_cluster(clist$lon[k], clist$lat[k])

      distance_1 <- as.numeric(dtHaversine(dt$lat[dt$V == point_1], dt$lon[dt$V == point_1], lat1, lon1) / 1000 / 1.852)
      distance_2 <- as.numeric(dtHaversine(dt$lat[dt$V == point_2], dt$lon[dt$V == point_2], clist$lat[k], clist$lon[k]) / 1000 / 1.852)
      extra_distance <- distance_1 + distance_2
      sh_route <- find_route(gg, point_1, point_2, extra_distance)[[1]]

      ### plot the map

      route <- sh_route
      route <- route[!is.na(route$lon_1), ]
      route$lon[route$lon_1 > 100 & route$lon < (-100)] <- 179
      route$lon[route$lon_1 < (-100) & route$lon > 100] <- (-179)


      for (i in 1:nrow(route)) {
        route_map <- route_map %>%
          addPolylines(
            lng = c(route$lon[i], route$lon_1[i]),
            lat = c(route$lat[i], route$lat_1[i],
              weight = 1
            )
          )
      }

      route_map <- route_map %>%
        addTiles() %>%
        addCircleMarkers(lng = route$lon[1], lat = route$lat[1], popup = FALSE) %>%
        addCircleMarkers(lng = route$lon[nrow(route)], lat = route$lat[nrow(route)], color = "Red", popup = FALSE)
    }
    return(route_map)
  })
}


#' Map the full maritime network between paired points
#'
#' Given a data frame / data.table (`clist`) that contains a collection of
#' origin destination pairs, the function draws a **leaflet** map with a
#' separate shortest route line for each pair.  The function is useful for
#' visualising an entire network of maritime connections (e.g. trade
#' routes, shipping lanes, etc.).
#'
#' @param clist A data.frame or data.table that must contain **four**
#'   columns with the exact names `longitude.x`, `latitude.x`,
#'   `longitude.y` and `latitude.y`.  The ``*.x`` columns represent the
#'   origins, the ``*.y`` columns represent the destinations.  Any additional
#'   columns are ignored.
#'
#' @return An object of class **`leaflet`** that displays all of the
#'   shortest maritime routes contained in `clist`.
#'
#' @examples
#' ## Two dummy routes:
#' ##    from (lon=120, lat=30) to (lon=80,  lat=7)
#' ##    from (lon=122, lat=31) to (lon=85,  lat=10)
#' routes <- data.frame(
#'   longitude.x = c(120, 122),
#'   latitude.x  = c(30, 31),
#'   longitude.y = c(80, 85),
#'   latitude.y  = c(7, 10)
#' )
#' multi_route_map_network(routes)
#'
#' @export
multi_route_map_network <- function(clist) {
  suppressWarnings({
    route_map <- leaflet()

    for (k in 1:nrow(clist)) {
      point_1 <- find_closest_cluster(clist$longitude.x[k], clist$latitude.x[k])
      point_2 <- find_closest_cluster(clist$longitude.y[k], clist$latitude.y[k])
      if (point_1 == point_2) {} else {
        route <- find_route(gg, point_1, point_2, 0)[[1]]
        route <- route[!is.na(route$lon_1), ]
        route$lon[route$lon_1 > 100 & route$lon < (-100)] <- 179
        route$lon[route$lon_1 < (-100) & route$lon > 100] <- (-179)


        for (i in 1:nrow(route)) {
          route_map <- route_map %>%
            addPolylines(
              lng = c(route$lon[i], route$lon_1[i]),
              lat = c(route$lat[i], route$lat_1[i],
                weight = 0.1
              )
            )
        }
      } # if point 1==point2
    }
    route_map <- route_map %>%
      addTiles()

    return(route_map)
  })
}



#' Map a continues route that passes through specific points
#'
#' Given a data frame / data.table (`clist`) that contains a list of
#' coordinates, the function draws a **leaflet** map with the
#' shortest route line that connects the points in thelist.  The function is useful for
#' visualising a full route that passes from specific points (e.g. all vessel port calls
#' during a certain period).
#'
#' @param clist A data.frame or data.table that must contain **two**
#'   columns with the exact names `longitude` and `latitude`.  Any additional
#'   columns are ignored.
#'
#' @return An object of class **`leaflet`** that displays all of the
#'   shortest maritime routes contained in `clist`.
#'
#' @examples
#' ## A multi-port route:
#' routes <- data.frame(cbind(
#' longitude=c(120,122,90,60,30,25,0,8),
#' latitude=c(30,31,40,32,40,34,34,55)))
#' multi_point_route(routes)
#'
#' @export
multi_point_route <- function(clist) {
  suppressWarnings({
    route_map <- leaflet()

    for (k in 1:(nrow(clist)-1)) {
      point_1 <- find_closest_cluster(clist$longitude[k], clist$latitude[k])
      point_2 <- find_closest_cluster(clist$longitude[k+1], clist$latitude[k+1])
      if (point_1 == point_2) {} else {
        route <- find_route(gg, point_1, point_2, 0)[[1]]
        route <- route[!is.na(route$lon_1), ]
        route$lon[route$lon_1 > 100 & route$lon < (-100)] <- 179
        route$lon[route$lon_1 < (-100) & route$lon > 100] <- (-179)


        for (i in 1:nrow(route)) {
          route_map <- route_map %>%
            addPolylines(
              lng = c(route$lon[i], route$lon_1[i]),
              lat = c(route$lat[i], route$lat_1[i],
                      weight = 0.1
              )
            )
        }
      } # if point 1==point2
    }
    route_map <- route_map %>%
      addTiles()

    return(route_map)
  })
}
