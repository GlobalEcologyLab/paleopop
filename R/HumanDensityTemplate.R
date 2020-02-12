#' R6 Class Representing a Nested Container for Human Density Model Attributes
#'
#' @description
#' R6 class representing a nested container for human density model attributes that are
#' maintained when new model clones are created. The container maintains input and
#' output attribute names, file and function templates, and human density model
#' attributes that need to be maintained when a new clone object is generated for a
#' sample simulation.
#'
#' @importFrom R6 R6Class
#' @include GenerativeTemplate.R
#' @export HumanDensityTemplate

HumanDensityTemplate <- R6Class("HumanDensityTemplate",
  inherit = GenerativeTemplate,
  public = list(

    ## Attributes ##

    ## Methods ##

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method sets default values.
    initialize = function() { # # set default standard deviation and temporal correlation
      self$sd_number <- 1
      self$temporal_correlation <- 1
      super$initialize()
    }

  ), # end public

  private = list(

    ## Attributes ##
    # .description        [inherited]
    # .inputs             [inherited]
    # .outputs            [inherited]
    # .file_templates     [inherited]
    # .function_templates [inherited]
    .carrying_capacity_mean = NULL,
    .carrying_capacity_sd = NULL,
    .human_occupancy_mask = NULL,
    .lower_threshold = NULL,
    .mean_upper = NULL,
    .max_upper = NULL,
    .sd_number = NULL,
    .distrib_data = NULL,
    .uses_correlations = FALSE,
    .correlation_model = NULL,
    .temporal_correlation = NULL,
    .decimals = NULL
  ), # end private

  # Active binding accessors for private attributes (above) #
  active = list(

    # description [inherited]

    # inputs [inherited]

    # outputs [inherited]

    # file_templates [inherited]

    # function_templates [inherited]

    #' @field carrying_capacity_mean Matrix (or data frame) of human carrying capacity time-series data means (simulation cells by duration).
    carrying_capacity_mean = function(value) {
      if (missing(value)) {
        private$.carrying_capacity_mean
      } else {
        private$.carrying_capacity_mean <- value
      }
    },

    #' @field carrying_capacity_sd Matrix (or data frame) of human carrying capacity time-series data standard deviations (simulation cells by duration).
    carrying_capacity_sd = function(value) {
      if (missing(value)) {
        private$.carrying_capacity_sd
      } else {
        private$.carrying_capacity_sd <- value
      }
    },

    #' @field human_occupancy_mask Optional binary mask (matrix or data frame) for human carrying capacity time-series data (simulation cells by duration).
    human_occupancy_mask = function(value) {
      if (missing(value)) {
        private$.human_occupancy_mask
      } else {
        private$.human_occupancy_mask <- value
      }
    },

    #' @field lower_threshold Optional lower threshold for human occupancy (lower values are set to zero).
    lower_threshold = function(value) {
      if (missing(value)) {
        private$.lower_threshold
      } else {
        private$.lower_threshold <- value
      }
    },

    #' @field mean_upper Optional upper limit for sample distribution mean carrying capacities.
    mean_upper = function(value) {
      if (missing(value)) {
        private$.mean_upper
      } else {
        private$.mean_upper <- value
      }
    },

    #' @field max_upper Optional upper limit for sample distribution maximum carrying capacities.
    max_upper = function(value) {
      if (missing(value)) {
        private$.max_upper
      } else {
        private$.max_upper <- value
      }
    },

    #' @field sd_number Number of standard deviations from the sample distribution mean to the lower and upper limits (default = 1).
    sd_number = function(value) {
      if (missing(value)) {
        private$.sd_number
      } else {
        private$.sd_number <- value
      }
    },

    #' @field distrib_data Calculated sample distribution minimum, maximum and mean carrying capacity time-series data, combined, scaled, and zero sets eliminated for generation efficiency.
    distrib_data = function(value) {
      if (missing(value)) {
        private$.distrib_data
      } else {
        private$.distrib_data <- value
      }
    },

    #' @field uses_correlations A boolean to indicate that a correlation model is used for generating correlated random deviates.
    uses_correlations = function(value) {
      if (missing(value)) {
        private$.uses_correlations
      } else {
        private$.uses_correlations <- value
      }
    },
    
    #' @field correlation_model A correlation model for generating correlated random deviates (type of \emph{CorrelationModel} or an inherited class).
    correlation_model = function(value) {
      if (missing(value)) {
        private$.correlation_model
      } else {
        private$.correlation_model <- value
      }
    },
    
    #' @field temporal_correlation Absolute correlation coefficient between simulation time steps for all grid cells (0-1; default = 1).
    temporal_correlation = function(value) {
      if (missing(value)) {
        private$.temporal_correlation
      } else {
        private$.temporal_correlation <- value
      }
    },
    
    #' @field decimals Number of decimal places applied to the calculated human densities (default: NULL = no rounding).
    decimals = function(value) {
      if (missing(value)) {
        private$.decimals
      } else {
        private$.decimals <- value
      }
    }

  ) # end active
)
