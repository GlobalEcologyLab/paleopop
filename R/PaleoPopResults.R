#' R6 class representing paleopop simulator results.
#'
#' @description
#' \code{\link[R6:R6Class]{R6}} class for encapsulating and dynamically generating
#' spatially-explicit \code{\link{paleopop_simulator}} results, as well as optional
#' re-generated \code{\link{Generator}} for niche carrying capacity and/or human
#' density.
#' 
#' @examples 
#' library(raster)
#' library(poems)
#' # Ring Island example region
#' coordinates <- data.frame(x = rep(seq(-178.02, -178.06, -0.01), 5),
#'                           y = rep(seq(19.02, 19.06, 0.01), each = 5))
#' template_raster <- Region$new(coordinates = coordinates)$region_raster # full extent
#' sealevel_raster <- template_raster
#' template_raster[][c(7:9, 12:14, 17:19)] <- NA # make Ring Island
#' sealevel_raster[][c(7:9, 12:14, 17:18)] <- NA
#' raster_stack <- raster::stack(x = append(replicate(9, template_raster), sealevel_raster))
#' region <- PaleoRegion$new(template_raster = raster_stack)
#' 
#' # Model template
#' model_template <- PaleoPopModel$new(
#' region = region,
#' time_steps = 10,
#' years_per_step = 12, # years per generational time-step
#' standard_deviation = 0.1,
#' growth_rate_max = 0.6,
#' harvest = FALSE,
#' populations = region$region_cells,
#' initial_abundance = seq(9000, 0, -1000),
#' transition_rate = 1.0,
#' carrying_capacity = rep(1000, 17),
#' dispersal = (!diag(nrow = 17, ncol = 17))*0.05,
#' density_dependence = "logistic",
#' dispersal_target_k = 10,
#' occupancy_threshold = 1, 
#' abundance_threshold = 10,
#' results_selection = c("abundance")
#' )
#' 
#' # Simulations
#' results <- paleopop_simulator(model_template)
#' 
#' # Results
#' results_model <- PaleoPopResults$new(results = results, region = region, time_steps = 10)
#' results_model$extirpation # cells where the population goes to zero are marked 1
#' results_model$occupancy # indicates with 0 and 1 which cells are occupied at each time step
#' results_model$ema # expected minimum abundance
#'
#' @importFrom R6 R6Class
#' @importFrom poems Region
#' @importFrom poems SimulationResults
#' @export PaleoPopResults

PaleoPopResults <- R6Class("PaleoPopResults",
  inherit = SimulationResults,
  public = list(

    ## Attributes ##

    # object_generator [inherited]

    #' @field attached A list of dynamically attached attributes (name-value pairs).
    attached = list()

    ## Methods ##

    # Inherited methods (from GenericClass, GenericModel, SpatialModel & SimulationResults) #
    #   initialize(results = NULL, parent = NULL, ...)
    #   new_clone(...)
    #   get_attribute_names(all = FALSE)
    #   get_attributes(params = NULL, remove_burn_in = TRUE)
    #   get_attribute(param)
    #   get_attribute_aliases(params = NULL)
    #   set_attributes(params = list(), ...)

    # Overwritten/overridden methods #

    # New methods (see active attributes) #

  ), # end public

  private = list(

    ## Attributes ##

    # Model attributes #
    .model_attributes = c("coordinates", "time_steps", "burn_in_steps", "occupancy_mask", "trend_interval",
                          "abundance", "abundance_trend", "ema", "extirpation", "extinction_location", "harvested",
                          "occupancy", "carrying_capacity", "human_density"),
    # .region              [inherited]
    # .time_steps          [inherited]
    # .burn_in_steps       [inherited]
    # .occupancy_mask      [inherited]
    .trend_interval = NULL,
    .abundance = NULL,
    .abundance_trend = NULL,
    .ema = NULL,
    .extirpation = NULL,
    .extinction_location = NULL,
    .harvested = NULL,
    .occupancy = NULL,
    .carrying_capacity = NULL,
    .human_density = NULL,

    # Attributes accessible via model get/set methods #
    .active_attributes = c("coordinates", "time_steps", "burn_in_steps", "occupancy_mask", "trend_interval",
                           "abundance", "abundance_trend", "ema", "extirpation", "extinction_location", "harvested",
                           "occupancy", "carrying_capacity", "human_density")

    # Model reference attributes #
    # .all                 [inherited]
    # .parent              [inherited]
    # .default             [inherited]

    # Dynamic attributes #
    # .attribute_aliases   [inherited]

    # Errors and warnings #
    # .error_messages      [inherited]
    # .warning_messages    [inherited]

  ), # end private

  # Active binding accessors for private attributes (above)
  active = list(

    # Model attributes accessors #

    #' @field model_attributes A vector of model attribute names.
    model_attributes = function(value) { # inherited
      if (missing(value)) {
        super$model_attributes
      } else {
        super$model_attributes <- value
      }
    },

    #' @field region A \code{\link{Region}} (or inherited class) object specifying the study region.
    region = function(value) { # inherited
      if (missing(value)) {
        super$region
      } else {
        super$region <- value
      }
    },

    #' @field coordinates Data frame (or matrix) of X-Y population coordinates (WGS84) in longitude (degrees West) and latitude (degrees North).
    coordinates = function(value) { # use non-raster region
      if (missing(value)) {
        self$region$coordinates
      } else {
        if (!is.null(self$region)) {
          self$region$coordinates <- value
        } else { # create a region with coordinates
          self$region <- Region$new(coordinates = value, use_raster = FALSE)
        }
      }
    },

    #' @field time_steps Number of simulation time steps.
    time_steps = function(value) { # inherited
      if (missing(value)) {
        super$time_steps
      } else {
        super$time_steps <- value
      }
    },

    #' @field burn_in_steps Optional number of initial 'burn-in' time steps to be ignored.
    burn_in_steps = function(value) { # inherited
      if (missing(value)) {
        super$burn_in_steps
      } else {
        super$burn_in_steps <- value
      }
    },

    #' @field occupancy_mask Optional binary mask array (matrix), data frame, or raster (stack) for each cell at each time-step of the simulation including burn-in.
    occupancy_mask = function(value) { # inherited
      if (missing(value)) {
        super$occupancy_mask
      } else {
        super$occupancy_mask <- value
      }
    },

    #' @field trend_interval Optional time-step range (indices) for trend calculations (assumes indices begin after the burn-in when utilized).
    trend_interval = function(value) {
      if (missing(value)) {
        if (!is.null(self$parent)) {
          self$parent$trend_interval
        } else {
          private$.trend_interval
        }
      } else {
        private$.trend_interval <- value
      }
    },

    #' @field abundance Matrix of population abundance across simulation time-steps (\emph{populations} rows by \emph{duration} columns).
    abundance = function(value) {
      if (missing(value)) {
        if (is.null(private$.abundance) && !is.null(self$parent) && !is.null(self$parent$abundance)) {
          # Calculate from individual populations
          private$.abundance <- .colSums(self$parent$abundance, m = nrow(self$parent$abundance),
                                         n = ncol(self$parent$abundance), na.rm = TRUE)
        }
        if (!is.null(self$parent)) { # all populations
          private$.abundance
        } else { # individual populations
          if (!is.null(private$.abundance)) {
            if (!is.null(self$burn_in_steps)) {
              duration_indices <- (self$burn_in_steps + 1):ncol(private$.abundance)
            } else {
              duration_indices <- 1:ncol(private$.abundance)
            }
            if (!is.null(self$occupancy_mask)) {
              (private$.abundance*self$occupancy_mask)[, duration_indices]
            } else {
              private$.abundance[, duration_indices]
            }
          } else {
            private$.abundance
          }
        }
      } else {
        private$.abundance <- value
      }
    },

    #' @field abundance_trend Trend or average Sen's \code{\link[trend:sens.slope]{slope}} of total abundance (optionally across a time-step interval).
    abundance_trend = function(value) {
      if (missing(value)) {
        if (is.null(private$.abundance_trend) && !is.null(self$parent) && !is.null(self$abundance)) {
          if (is.numeric(self$trend_interval) && min(self$trend_interval) >= 1 && max(self$trend_interval) <= length(self$abundance)) {
            private$.abundance_trend <- as.numeric(trend::sens.slope(self$abundance[self$trend_interval])$estimates)
          } else {
            private$.abundance_trend <- as.numeric(trend::sens.slope(self$abundance)$estimates)
          }
        }
        private$.abundance_trend
      } else {
        private$.abundance_trend <- value
      }
    },

    #' @field ema Matrix of population expected minimum abundance (EMA) across simulation time-steps (\emph{populations} rows by \emph{duration} columns).
    ema = function(value) {
      if (missing(value)) {
        if (is.null(private$.ema) && !is.null(self$abundance)) {
          # Calculate via abundance
          if (!is.null(self$parent)) { # all populations
            ema <- array(self$abundance[1], length(self$abundance))
            for (i in 2:length(self$abundance)) {
              ema[i] <- min(self$abundance[i], ema[i - 1])
            }
          } else { # individual populations
            ema <- array(self$abundance[, 1], dim(self$abundance))
            for (i in 2:ncol(self$abundance)) {
              ema[, i] <- pmin(self$abundance[, i], ema[, i - 1])
            }
          }
          private$.ema <- ema
        }
        private$.ema
      } else {
        private$.ema <- value
      }
    },

    #' @field extirpation Array of population extirpation times.
    extirpation = function(value) {
      if (missing(value)) {
        if (is.null(private$.extirpation) && !is.null(self$abundance)) {
          # Calculate via abundance
          if (!is.null(self$parent)) { # all populations
            if (0 %in% self$abundance) {
              extirpation <- min(which(self$abundance == 0), na.rm = TRUE)
            } else {
              extirpation <- NA
            }
          } else { # individual populations
            extirpation <- array(NA, nrow(self$abundance))
            extirpation[which(self$abundance[, 1] == 0)] <- 1
            for (i in 2:ncol(self$abundance)) {
              extirpation <- pmin(extirpation, rep(i, nrow(self$abundance)), na.rm = TRUE)
              extirpation[which(as.logical(self$abundance[, i]))] <- NA
            }
          }
          private$.extirpation <- extirpation
        }
        private$.extirpation
      } else {
        private$.extirpation <- value
      }
    },

    #' @field extinction_location The weighted centroid of cells occupied in the time-step prior to the extirpation of all populations (if occurred).
    extinction_location = function(value) {
      if (missing(value)) {
        if (is.null(private$.extinction_location) && !is.null(self$parent) &&
            !is.null(self$parent$coordinates) && is.numeric(self$extirpation)) {
          if (self$extirpation > 1) {
            last_pop_indices <- which(as.logical(self$parent$abundance[, self$extirpation - 1]))
            if (length(last_pop_indices) > 1) {
              abundance_weights <- matrix(rep(self$parent$abundance[last_pop_indices, self$extirpation - 1], 2), ncol = 2)
              extinction_location <- .colSums(as.matrix(self$parent$coordinates[last_pop_indices,])*abundance_weights, m = length(last_pop_indices), n = 2)/.colSums(abundance_weights, m = length(last_pop_indices), n = 2)
            } else {
              extinction_location <- as.numeric(self$parent$coordinates[last_pop_indices,])
            }
            names(extinction_location) <- c("x", "y")
            private$.extinction_location <- extinction_location
          } else {
            private$.extinction_location <- NA
          }
        }
        private$.extinction_location
      } else {
        private$.extinction_location <- value
      }
    },

    #' @field harvested Matrix of the number of animals harvested from each population at each time-step (\emph{populations} rows by \emph{duration} columns).
    harvested = function(value) {
      if (missing(value)) {
        if (is.null(private$.harvested) && !is.null(self$parent) && !is.null(self$parent$harvested)) {
          # Calculate from individual populations
          private$.harvested <- .colSums(self$parent$harvested, m = nrow(self$parent$harvested),
                                         n = ncol(self$parent$harvested), na.rm = TRUE)
        }
        if (!is.null(self$parent)) { # all populations
          private$.harvested
        } else { # individual populations
          if (!is.null(private$.harvested)) {
            if (!is.null(self$burn_in_steps)) {
              duration_indices <- (self$burn_in_steps + 1):ncol(private$.harvested)
            } else {
              duration_indices <- 1:ncol(private$.harvested)
            }
            if (!is.null(self$occupancy_mask)) {
              (private$.harvested*self$occupancy_mask)[, duration_indices]
            } else {
              private$.harvested[, duration_indices]
            }
          } else {
            private$.harvested
          }
        }
      } else {
        private$.harvested <- value
      }
    },

    #' @field occupancy Array of the number of populations occupied at each time-step.
    occupancy = function(value) {
      if (missing(value)) {
        if (is.null(private$.occupancy) && !is.null(self$abundance)) {
          # Calculate via abundance
          if (!is.null(self$parent)) { # all populations
            private$.occupancy <- .colSums(as.logical(self$parent$abundance), m = nrow(self$parent$abundance),
                                           n = ncol(self$parent$abundance), na.rm = TRUE)
          } else { # individual populations
            private$.occupancy <- +(self$abundance > 0)
          }
        }
        private$.occupancy
      } else {
        private$.occupancy <- value
      }
    },

    #' @field carrying_capacity Optional matrix of simulation input carrying capacity to be combined with results (\emph{populations} rows by \emph{duration} columns).
    carrying_capacity = function(value) {
      if (missing(value)) {
        if (is.null(private$.carrying_capacity) && !is.null(self$parent) && !is.null(self$parent$carrying_capacity)) {
          # Calculate from individual populations
          private$.carrying_capacity <- .colSums(self$parent$carrying_capacity, m = nrow(self$parent$carrying_capacity),
                                                   n = ncol(self$parent$carrying_capacity), na.rm = TRUE)
        }
        if (!is.null(self$parent)) { # all populations
          private$.carrying_capacity
        } else { # individual populations
          if (!is.null(private$.carrying_capacity)) {
            if (!is.null(self$burn_in_steps)) {
              duration_indices <- (self$burn_in_steps + 1):ncol(private$.carrying_capacity)
            } else {
              duration_indices <- 1:ncol(private$.carrying_capacity)
            }
            if (!is.null(self$occupancy_mask)) {
              (private$.carrying_capacity*self$occupancy_mask)[, duration_indices]
            } else {
              private$.carrying_capacity[, duration_indices]
            }
          } else {
            private$.carrying_capacity
          }
        }
      } else {
        private$.carrying_capacity <- value
      }
    },

    #' @field human_density Optional matrix of simulation input human density to be combined with results (\emph{populations} rows by \emph{duration} columns).
    human_density = function(value) {
      if (missing(value)) {
        if (is.null(private$.human_density) && !is.null(self$parent) && !is.null(self$parent$human_density)) {
          # Calculate from individual populations
          private$.human_density <- .colSums(self$parent$human_density, m = nrow(self$parent$human_density),
                                               n = ncol(self$parent$human_density), na.rm = TRUE)
        }
        if (!is.null(self$parent)) { # all populations
          private$.human_density
        } else { # individual populations
          if (!is.null(private$.human_density)) {
            if (!is.null(self$burn_in_steps)) {
              duration_indices <- (self$burn_in_steps + 1):ncol(private$.human_density)
            } else {
              duration_indices <- 1:ncol(private$.human_density)
            }
            if (!is.null(self$occupancy_mask)) {
              (private$.human_density*self$occupancy_mask)[, duration_indices]
            } else {
              private$.human_density[, duration_indices]
            }
          } else {
            private$.human_density
          }
        }
      } else {
        private$.human_density <- value
      }
    },

    # Model reference attribute accessors #

    #' @field all Nested simulation results for all cells.
    all = function(value) { # inherited
      if (missing(value)) {
        super$all
      } else {
        super$all <- value
      }
    },

    #' @field parent Parent simulation results for individual cells.
    parent = function(value) { # inherited
      if (missing(value)) {
        super$parent
      } else {
        super$parent <- value
      }
    },

    #' @field default Default value/attribute utilized when applying primitive metric functions (e.g. max) to the results.
    default = function(value) { # inherited
      if (missing(value)) {
        super$default
      } else {
        super$default <- value
      }
    },

    # Dynamic attribute accessors #

    #' @field attribute_aliases A list of alternative alias names for model attributes (form: \code{alias = "attribute"}) to be used with the set and get attributes methods.
    attribute_aliases = function(value) { # inherited
      if (missing(value)) {
        super$attribute_aliases
      } else {
        super$attribute_aliases <- value
      }
    },

    # Errors and warnings accessors #

    #' @field error_messages A vector of error messages encountered when setting model attributes.
    error_messages = function(value) { # inherited
      if (missing(value)) {
        super$error_messages
      } else {
        super$error_messages <- value
      }
    },

    #' @field warning_messages A vector of warning messages encountered when setting model attributes.
    warning_messages = function(value) { # inherited
      if (missing(value)) {
        super$warning_messages
      } else {
        super$warning_messages <- value
      }
    }

  ) # end active
)
