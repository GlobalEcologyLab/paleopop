#' R6 Class Representing a Nested Container for Dispersal Model Attributes
#'
#' @description
#' R6 class representing a nested container for generative model attributes that are
#' maintained when new model clones are created. The container maintains input and
#' output attribute names, file and function templates, and dispersal model attributes
#' that need to be maintained when a new clone object is generated for a sample
#' simulation.
#'
#' @importFrom R6 R6Class
#' @include GenerativeTemplate.R
#' @export DispersalTemplate

DispersalTemplate <- R6Class("DispersalTemplate",
  inherit = GenerativeTemplate,
  public = list(

    ## Attributes ##

    ## Methods ##

    # Inherited methods (from GenerativeTemplate) #
    # initialize()

  ), # end public

  private = list(

    ## Attributes ##
    # .description        [inherited]
    # .inputs             [inherited]
    # .outputs            [inherited]
    # .file_templates     [inherited]
    # .function_templates [inherited]
    .barrier_sea_ice_model = NULL,
    .coordinates = NULL,
    .distance_classes = NULL,
    .distance_data = NULL,
    .dispersal_function_data = NULL,
    .decimals = NULL,
    .proportion_multiplier = NULL

  ), # end private

  # Active binding accessors for private attributes (above) #
  active = list(

    # description [inherited]

    # inputs [inherited]

    # outputs [inherited]

    # file_templates [inherited]

    # function_templates [inherited]

    #' @field barrier_sea_ice_model A BarrierSeaIceModel object for dispersal distance multiplier data.
    barrier_sea_ice_model = function(value) {
      if (missing(value)) {
        private$.barrier_sea_ice_model
      } else {
        private$.barrier_sea_ice_model <- value
      }
    },
    
    #' @field coordinates Data frame (or matrix) of X-Y population coordinates (WGS84) in longitude (degrees West) and latitude (degrees North).
    coordinates = function(value) {
      if (missing(value)) {
        private$.coordinates
      } else {
        private$.coordinates <- value
      }
    },

    #' @field distance_classes Vector of distance interval boundaries (in km) for calculating discrete dispersal rates.
    distance_classes = function(value) {
      if (missing(value)) {
        private$.distance_classes
      } else {
        private$.distance_classes <- value
      }
    },

    #' @field distance_data Data frame of distance classes including indices for the construction of compact matrices (columns: target_pop, source_pop, compact_row, distance_class).
    distance_data = function(value) {
      if (missing(value)) {
        private$.distance_data
      } else {
        private$.distance_data <- value
      }
    },

    #' @field dispersal_function_data Data frame of discrete dispersal function values. Optional first column may provide distance intervals (non-inclusive lower bounds).
    dispersal_function_data = function(value) {
      if (missing(value)) {
        private$.dispersal_function_data
      } else {
        private$.dispersal_function_data <- value
      }
    },

    #' @field decimals Optional number of decimal places applied to the calculated dispersal rates (default: NULL = no rounding).
    decimals = function(value) {
      if (missing(value)) {
        private$.decimals
      } else {
        private$.decimals <- value
      }
    },

    #' @field proportion_multiplier Optional constant multiplier to be applied to the dispersal function \emph{p} parameter. May be used to represent the fraction of the population capable of migrating.
    proportion_multiplier = function(value) {
      if (missing(value)) {
        private$.proportion_multiplier
      } else {
        private$.proportion_multiplier <- value
      }
    }

  ) # end active
)
