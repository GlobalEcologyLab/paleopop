#' Bison vignette Siberia raster
#'
#' A \code{raster} dataset defining the grid cells of the Siberia study region in a temporally dynamic manner for the
#' bison example vignette.
#'
#' @format A \emph{raster::RasterStack} object:
#' \describe{
#'   \item{dimensions}{21 rows by 180 columns by 1001 layers}
#'   \item{resolution}{2 by 2 degree grid cells}
#'   \item{crs}{WGS84 latitude longitude}
#'   \item{extent}{longitude -180 to 180 degrees; latitude 42 to 84 degrees}
#'   \item{values}{region defined by 913 cells with value of 1, surrounded by non-region \code{NA} values}
#' }
#' @source TBA
"siberia_raster"

#' Bison vignette habitat suitability raster
#'
#' A \emph{raster} dataset defining estimated habitat suitability values for each grid
#' cells of the Siberian study region of the bison example vignette.
#'
#' @format A \emph{raster::RasterStack} object:
#' \describe{
#'   \item{dimensions}{21 rows by 180 columns by 1001 layers}
#'   \item{resolution}{2 by 2 degree grid cells}
#'   \item{extent}{longitude -180 to 180 degrees; latitude 42 to 84 degrees}
#'   \item{values}{Estimated habitat suitability values of 0 to 1}
#' }
#' @source TBA
"bison_hs_raster"