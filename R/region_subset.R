#' Function generates a region subset of matrix values based on a subset of coordinates within the original region (using nearest spatial neighbour if coordinates differ).
#'
#' \code{region_subset} generates a region subset of matrix values based on a subset of coordinates within the original region (using nearest spatial neighbour if coordinates differ).
#'
#' @examples 
#' coordinates <- data.frame(x = rep(seq(-178.02, -178.06, -0.01), 5),
#'                           y = rep(seq(19.02, 19.06, 0.01), each = 5))
#' values <- matrix(seq(1, 25, 1))
#' subset <- data.frame(x = rep(seq(-178, -178.04, -0.005), 7), 
#'                      y = rep(seq(19.03, 19.06, 0.005), each = 9))
#' region_subset(coordinates, values, subset) # nearest neighbor interpolation
#'
#' @param orig_coords Data frame (or matrix) of original/full region of X-Y coordinates (WGS84) in longitude (degrees West) and latitude (degrees North).
#' @param orig_matrix Matrix of original values with rows corresponding to the original/full region coordinates.
#' @param subset_coords Data frame (or matrix) of X-Y subset region coordinates (WGS84) in longitude (degrees West) and latitude (degrees North).
#' @return A matrix of values corresponding to the subset region coordinates (using nearest spatial neighbor if original and subset coordinates differ).
#' @importFrom sf st_geometry
#' @importFrom sf st_as_sf
#' @importFrom sf st_join
#' @importFrom sf st_nearest_feature
#' @export region_subset

region_subset = function(orig_coords = NULL, orig_matrix = NULL, subset_coords = NULL) {

  # Handle the inputs as possible file paths
  if (is.character(orig_coords) && file.exists(orig_coords)) {
    if (length(grep(".CSV", toupper(orig_coords), fixed = TRUE))) {
      orig_coords <- read.csv(file = orig_coords)
    } else if (length(grep(".RDS", toupper(orig_coords), fixed = TRUE))) {
      orig_coords <- readRDS(file = orig_coords)
    } else {
      orig_coords <- read.table(file = orig_coords)
    }
  }
  if (is.character(orig_matrix) && file.exists(orig_matrix)) {
    if (length(grep(".CSV", toupper(orig_matrix), fixed = TRUE))) {
      orig_matrix <- read.csv(file = orig_matrix)
    } else if (length(grep(".RDS", toupper(orig_matrix), fixed = TRUE))) {
      orig_matrix <- readRDS(file = orig_matrix)
    } else {
      orig_matrix <- read.table(file = orig_matrix)
    }
  }
  if (is.character(subset_coords) && file.exists(subset_coords)) {
    if (length(grep(".CSV", toupper(subset_coords), fixed = TRUE))) {
      subset_coords <- read.csv(file = subset_coords)
    } else if (length(grep(".RDS", toupper(subset_coords), fixed = TRUE))) {
      subset_coords <- readRDS(file = subset_coords)
    } else {
      subset_coords <- read.table(file = subset_coords)
    }
  }

  # Check presence and force types and names and add ID's
  missing = c()
  if (!is.null(orig_coords)) {
    orig_coords <- as.data.frame(orig_coords)[, 1:2]
    names(orig_coords) <- c("x", "y")
    orig_coords$orig_id <- 1:nrow(orig_coords)
  } else {
    missing <- c(missing, "orig_coords")
  }
  if (!is.null(orig_matrix)) {
    orig_matrix <- as.matrix(orig_matrix)
  } else {
    missing <- c(missing, "orig_matrix")
  }
  if (!is.null(subset_coords)) {
    subset_coords <- as.data.frame(subset_coords)[, 1:2]
    names(subset_coords) <- c("x", "y")
    subset_coords$subset_id <- 1:nrow(subset_coords)
  } else {
    missing <- c(missing, "subset_coords")
  }
  if (length(missing)) {
    return(sprintf("Function region_subset requires valid parameters: %s", paste(c("a","b","c"), collapse = ", ")))
  }

  # Ensure both sets of coordinates have longitudes between -180 and 180
  orig_coords$x <- with(orig_coords, ifelse(x < 0, x + 360, x))
  orig_coords$x <- with(orig_coords, ifelse(x > 180, x - 360, x))
  subset_coords$x <- with(subset_coords, ifelse(x < 0, x + 360, x))
  subset_coords$x <- with(subset_coords, ifelse(x > 180, x - 360, x))

  # Wrap coordinates in sf spatial data frames
  orig_coords <- sf::st_as_sf(orig_coords, coords = c("x", "y"), crs = 4326)
  ret <- do.call(rbind, sf::st_geometry(orig_coords))
  colnames(ret) <- c("x", "y")
  orig_coords <- cbind(orig_coords, ret)
  subset_coords <- sf::st_as_sf(subset_coords, coords = c("x", "y"), crs = 4326)

  # Join coordinates by matching to nearest spatial neighbour
  joined_ids <- as.data.frame(sf::st_join(x = subset_coords, y = orig_coords, join = sf::st_nearest_feature))[, 1:2]

  # Return the subset of the original matrix corresponding to the joined ID's
  return(orig_matrix[joined_ids$orig_id, ])
}
