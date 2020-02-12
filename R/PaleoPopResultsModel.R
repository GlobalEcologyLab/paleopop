#' R6 Class Representing a Results Model for the Default Paleopop Simulator.
#'
#' @description
#' An R6 class to represent a generic (abstract) model for encapsulating and dynamically
#' generating \emph{paleopop} simulation results associated with the default paleopop 
#' simulator function, as well as optional re-generated niche carrying capacities and/or
#' human densities.
#'
#' @importFrom R6 R6Class
#' @include GenericResultsModel.R
#' @export PaleoPopResultsModel

PaleoPopResultsModel <- R6Class("PaleoPopResultsModel",
  inherit = GenericResultsModel,
  public = list(

    ## Attributes ##
    
    # Model attributes #
    
    #' @field model_attributes An array of public model attribute names - another stored as private.
    model_attributes = c(), # private only (see below)
    
    ## Methods ##
    
    # Inherited methods (from GenericClass, GenericModel & GenericResultsModel) #
    #   initialize(results = NULL, ...)
    #   read_from_rds(path)
    #   save_to_rds(path)
    #   get_attribute_names(all = FALSE)
    #   get_attributes(params)
    #   set_attributes((params = list(), ...))

    # Overwritten/overridden methods #

    #' @description
    #' Creates a new (re-initialized) object of the current (inherited) object class with optionally passed parameters.
    #' @param ... Parameters passed via the inherited class constructor (defined in initialize and run via new).
    #' @return New object of the inherited class.
    new_clone = function(...) {
      if ("parent" %in% names(list(...))) {
        super$new_clone(...)
      } else {
        super$new_clone(coordinates = private$.coordinates,
                        trend_interval = private$.trend_interval, ...)
      }
    }

    # New methods (see active attributes) #

  ), # end public

  private = list(

    ## Attributes ##
    
    # .attribute_aliases [inherited]
    
    # Model reference attributes #
    # .all                 [inherited]
    # .parent              [inherited]
    # .default             [inherited]
    
    # Model attributes #
    .model_attributes = c("coordinates", "abundance", "abundance_trend", "ema", "extirpation", "extinction_location",
                          "harvested", "occupancy", "occupancy_mask", "burn_in_duration", "carrying_capacities",
                          "human_densities", "trend_interval"),
    .coordinates = NULL,
    .abundance = NULL,
    .abundance_trend = NULL,
    .ema = NULL,
    .extirpation = NULL,
    .extinction_location = NULL,
    .harvested = NULL,
    .occupancy = NULL,
    # .occupancy_mask      [inherited]
    # .burn_in_duration    [inherited]
    # .carrying_capacities [inherited]
    # .human_densities     [inherited]
    .trend_interval = NULL,

    # Attributes accessible via model get/set methods #
    .active_attributes = c("coordinates", "abundance", "abundance_trend", "ema", "extirpation", "extinction_location",
                           "harvested", "occupancy", "occupancy_mask", "burn_in_duration", "carrying_capacities",
                           "human_densities", "trend_interval")
    
    # Errors and warnings
    # .error_messages      [inherited]
    # .warning_messages    [inherited]

  ), # end private

  # Active binding accessors for private attributes (above)
  active = list(

    # attribute_aliases [inherited]
    
    # Model reference attributes #
    
    # all [inherited]
    
    # parent [inherited]
    
    #' @field default Default value(s) utilized when applying primitive metric functions (e.g. max) to the results.
    default = function(value) {
      if (missing(value)) {
        if (is.null(private$.default)) {
          self$all$abundance
        } else {
          private$.default
        }
      } else {
        private$.default <- value
      }
    },
    
    
    #' @field coordinates Data frame (or matrix) of X-Y population coordinates (WGS84) in longitude (degrees West) and latitude (degrees North).
    coordinates = function(value) {
      if (missing(value)) {
        private$.coordinates
      } else {
        if (is.character(value) && file.exists(value)) {
          if (length(grep(".CSV", toupper(value), fixed = TRUE))) {
            private$.coordinates <- read.csv(file = value)[, 1:2]
          } else if (length(grep(".RDS", toupper(value), fixed = TRUE))) {
            private$.coordinates <- readRDS(file = value)
          } else {
            private$.coordinates <- read.table(file = value)[, 1:2]
          }
        } else {
          if (!is.null(value)) {
            private$.coordinates <- as.data.frame(value)
          } else {
            private$.coordinates <- value
          }
        }
        if (!is.null(value)) {
          names(private$.coordinates) <- c("x", "y")
        }
      }
    },

    # Model attributes #
    #   Dynamically generate results via other attributes wherever possible

    #' @field abundance Matrix of population abundance across simulation time-steps (population rows by duration columns).
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
            if (!is.null(self$burn_in_duration)) {
              duration_indices <- (self$burn_in_duration + 1):ncol(private$.abundance)
            } else {
              duration_indices <- 1:ncol(private$.abundance)
            }
            if (!is.null(self$occupancy_mask)) {
              private$.abundance[, duration_indices]*self$occupancy_mask
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

    #' @field abundance_trend Trend or average (Sen's) slope of total abundance (optionally across a time-step interval).
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

    #' @field ema Matrix of population expected minimum abundance (EMA) across simulation time-steps (population rows by duration columns).
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

    #' @field harvested Matrix of the number of animals harvested from each population at each time-step (population rows by duration columns).
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
            if (!is.null(self$burn_in_duration)) {
              duration_indices <- (self$burn_in_duration + 1):ncol(private$.harvested)
            } else {
              duration_indices <- 1:ncol(private$.harvested)
            }
            if (!is.null(self$occupancy_mask)) {
              private$.harvested[, duration_indices]*self$occupancy_mask
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

    #' @field occupancy_mask Optional binary mask matrix for each population at each time-step (population rows by duration columns).
    # Overwritten/overridden to maintain flexibility on resolving duration burn-in inclusion
    occupancy_mask = function(value) {
      if (missing(value)) {
        if (!is.null(self$parent)) { # all populations
          private$.occupancy_mask
        } else { # individual populations
          if (!is.null(private$.occupancy_mask) && !is.null(self$burn_in_duration)) {
            # Resolve duration using abundance, harvested, carrying_capacities, or human_densities when available
            if ((!is.null(private$.abundance) && ncol(private$.occupancy_mask) == ncol(private$.abundance)) ||
                (!is.null(private$.harvested) && ncol(private$.occupancy_mask) == ncol(private$.harvested)) ||
                (!is.null(private$.carrying_capacities) && ncol(private$.occupancy_mask) == ncol(private$.carrying_capacities)) ||
                (!is.null(private$.human_densities) && ncol(private$.occupancy_mask) == ncol(private$.human_densities))) {
              duration_indices <- (self$burn_in_duration + 1):ncol(private$.occupancy_mask)
            } else { # Assume occupancy mask excludes burn-in
              duration_indices <- 1:ncol(private$.occupancy_mask)
            }
            private$.occupancy_mask[, duration_indices]
          } else {
            private$.occupancy_mask
          }
        }
      } else {
        if (!is.null(value)) {
          private$.occupancy_mask <- as.matrix(value)
        } else {
          private$.occupancy_mask <- value
        }
      }
    },

    # burn_in_duration [inherited]

    # carrying_capacities [inherited]

    # human_densities [inherited]

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
    }

    # Errors and warnings #

    # error_messages [inherited]

    # warning_messages [inherited]

  ) # end active
)
