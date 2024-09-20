#' @keywords internal
"_PACKAGE"
#' paleopop: Ensemble population modeling and simulation on paleo time scales
#'
#' The \code{paleopop} package is an extension of the \code{\link[poems:poems]{poems}} framework of \code{\link[R6:R6Class]{R6}} classes, which 
#' simulate populations on a dynamic landscape and validate the results via pattern-oriented modeling. \code{paleopop} adds
#' functionality for modeling populations over paleo time scales.
#' 
#' \itemize{The new functions and R6 classes added by \code{paleopop} to the \code{poems} framework are:
#'   \item \code{\link{paleopop_simulator}} function: Analogous to the \code{\link[poems:population_simulator]{population_simulator}} 
#'     function in \code{poems}, this is the engine of simulation in \code{paleopop}, handling input parameters,
#'     simulating over long time scales, and outputting up to six different types of results.
#'   \item \code{\link{PaleoRegion}} class: Inherited from \code{\link[poems:Region]{Region}}, this class defines a 
#'   geographic region that changes over time, creating a temporal mask that defines which cells are occupiable at a
#'   time step.
#'   \item \code{\link{region_subset}} function: a utility function for subsetting regions defined by coordinates. 
#'   \item \code{\link{PaleoPopModel}} class: Inherited from
#'     \code{\link[poems:SimulationModel]{SimulationModel}}, this class encapsulates the input parameters
#'     utilized by the \code{\link{paleopop_simulator}}.
#'   \item \code{\link{PaleoPopResults}} class: Inherited from
#'     \code{\link[poems:SimulationResults]{SimulationResults}}, this class encapsulates the results generated
#'     by the \code{\link{paleopop_simulator}}, as well as dynamically generating
#'     additional derived results.
#' }
#' 
#' @name paleopop
#' @importFrom utils read.csv read.table
NULL