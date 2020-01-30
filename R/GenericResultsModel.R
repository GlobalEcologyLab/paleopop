#' R6 Class Representing a Generic Results Model.
#'
#' @description
#' An R6 class to represent a generic (abstract) model for encapsulating and dynamically
#' generating simulation results simulation results, as well as optional re-generated 
#' niche carrying capacities and/or human densities.
#'
#' @importFrom R6 R6Class
#' @include GenericModel.R
#' @export GenericResultsModel

GenericResultsModel <- R6Class("GenericResultsModel",
  inherit = GenericModel,
  public = list(

    ## Attributes ##
    
    # Model attributes #
    
    #' @field model_attributes An array of public model attribute names - another stored as private.
    model_attributes = c(), # private only (see below)
    
    ## Methods ##
    
    # Inherited methods (from GenericClass & GenericModel) #
    #   read_from_rds(path)
    #   save_to_rds(path)
    #   get_attributes(params)
    #   set_attributes((params = list(), ...))

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method sets attributes from a results list or file, and sets object attributes individually and/or from a list.
    #' @param results A list containing results or a file path to simulation results.
    #' @param parent Parent simulation results model for individual populations (used when nesting a results model clone for all populations).
    #' @param ... Parameters passed via a \emph{params} list or individually.
    initialize = function(results = NULL, parent = NULL, ...) {
      if (!is.null(parent)) {
        self$parent <- parent
      } else {
        self$all <- self$new_clone(parent = self)
      }
      if (!is.null(results)) {
        if ((is.character(results) && file.exists(results) &&
             length(grep(".RDS", toupper(results), fixed = TRUE)))) {
          results = readRDS(results)
        } else  if (is.character(results)) {
          stop(paste("Could not read results from", results), call. = FALSE)
        }
        if (is.list(results)) {
          if ("all" %in% names(results)) {
            self$all$set_attributes(params = results$all)
            results$all <- NULL
          }
          super$initialize(params = results, ...)
        } else {
          stop(paste("Could not read results from type/class", class(results)[1]), call. = FALSE)
        }
      } else {
        super$initialize(...)
      }
    },

    #' @description
    #' Creates a new (re-initialized) object of the current (inherited) object class with optionally passed parameters.
    #' @param ... Parameters passed via the inherited class constructor (defined in initialize and run via new).
    #' @return New object of the inherited class.
    new_clone = function(...) {
      super$new_clone(occupancy_mask = private$.occupancy_mask,
                      burn_in_duration = private$.burn_in_duration, ...)
    },

    #' @description
    #' Returns an array of all attribute names including public and private model attributes, as well as attached attributes, error and warning messages.
    #' @param all Boolean to indicate if a nested list for all populations (when present) should be also listed (default is FALSE).
    #' @return Array of all attribute names with optional inclusion of attrribute names of nested results for all populations.
    get_attribute_names = function(all = FALSE) {
      if (all && !is.null(self$all) && length(self$all$get_attribute_names())) {
        return(c(super$get_attribute_names(), paste0("all$", self$all$get_attribute_names())))
      } else {
        return(super$get_attribute_names())
      }
    }

    # New methods (see active attributes) #

  ), # end public

  private = list(

    ## Attributes ##
    
    # .attribute_aliases [inherited]
    
    # Model reference attributes #
    .all = NULL,
    .parent = NULL,
    .default = NULL,
    
    # Model attributes #
    .model_attributes = c("occupancy_mask", "burn_in_duration", "carrying_capacities", "human_densities"),
    .occupancy_mask = NULL,
    .burn_in_duration = NULL,
    .carrying_capacities = NULL,
    .human_densities = NULL,

    # Attributes accessible via model get/set methods #
    .active_attributes = c("occupancy_mask", "burn_in_duration", "carrying_capacities", "human_densities")
    
    # Errors and warnings #
    # .error_messages    [inherited]
    # .warning_messages  [inherited]
    
  ), # end private

  # Active binding accessors for private attributes (above) #
  active = list(

    # attribute_aliases [inherited]
    
    # Model reference attributes #
    
    #' @field all Nested simulation results model for all populations.
    all = function(value) {
      if (missing(value)) {
        private$.all
      } else {
        private$.all <- value
      }
    },
    
    #' @field parent Parent simulation results model for individual populations.
    parent = function(value) {
      if (missing(value)) {
        private$.parent
      } else {
        private$.parent <- value
      }
    },
    
    #' @field default Default value(s) utilized when applying primitive metric functions (e.g. max) to the results.
    default = function(value) {
      if (missing(value)) {
        private$.default
      } else {
        private$.default <- value
      }
    },
    
    # Model attributes #
    #   Inherited class will dynamically generate results via other attributes wherever possible

    #' @field occupancy_mask Optional binary mask matrix for each population at each time-step (population rows by duration columns).
    occupancy_mask = function(value) {
      if (missing(value)) {
        if (!is.null(self$parent)) { # all populations
          private$.occupancy_mask
        } else { # individual populations
          if (!is.null(private$.occupancy_mask) && !is.null(self$burn_in_duration)) {
            # Resolve duration using carrying_capacities or human_densities when available
            if ((!is.null(private$.carrying_capacities) && ncol(private$.occupancy_mask) == ncol(private$.carrying_capacities)) ||
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

    #' @field burn_in_duration Optional number of initial 'burn-in' time steps to be ignored.
    burn_in_duration = function(value) {
      if (missing(value)) {
        private$.burn_in_duration
      } else {
        private$.burn_in_duration <- value
      }
    },

    #' @field carrying_capacities Optional matrix of simulation input carrying capacities to be combined with results ($populations rows by $duration columns).
    carrying_capacities = function(value) {
      if (missing(value)) {
        if (is.null(private$.carrying_capacities) && !is.null(self$parent) && !is.null(self$parent$carrying_capacities)) {
          # Calculate from individual populations
          private$.carrying_capacities <- .colSums(self$parent$carrying_capacities, m = nrow(self$parent$carrying_capacities),
                                                   n = ncol(self$parent$carrying_capacities), na.rm = TRUE)
        }
        if (!is.null(self$parent)) { # all populations
          private$.carrying_capacities
        } else { # individual populations
          if (!is.null(private$.carrying_capacities)) {
            if (!is.null(self$burn_in_duration)) {
              duration_indices <- (self$burn_in_duration + 1):ncol(private$.carrying_capacities)
            } else {
              duration_indices <- 1:ncol(private$.carrying_capacities)
            }
            if (!is.null(self$occupancy_mask)) {
              private$.carrying_capacities[, duration_indices]*self$occupancy_mask
            } else {
              private$.carrying_capacities[, duration_indices]
            }
          } else {
            private$.carrying_capacities
          }
        }
      } else {
        private$.carrying_capacities <- value
      }
    },

    #' @field human_densities Optional matrix of simulation input human densities to be combined with results ($populations rows by $duration columns).
    human_densities = function(value) {
      if (missing(value)) {
        if (is.null(private$.human_densities) && !is.null(self$parent) && !is.null(self$parent$human_densities)) {
          # Calculate from individual populations
          private$.human_densities <- .colSums(self$parent$human_densities, m = nrow(self$parent$human_densities),
                                               n = ncol(self$parent$human_densities), na.rm = TRUE)
        }
        if (!is.null(self$parent)) { # all populations
          private$.human_densities
        } else { # individual populations
          if (!is.null(private$.human_densities)) {
            if (!is.null(self$burn_in_duration)) {
              duration_indices <- (self$burn_in_duration + 1):ncol(private$.human_densities)
            } else {
              duration_indices <- 1:ncol(private$.human_densities)
            }
            if (!is.null(self$occupancy_mask)) {
              private$.human_densities[, duration_indices]*self$occupancy_mask
            } else {
              private$.human_densities[, duration_indices]
            }
          } else {
            private$.human_densities
          }
        }
      } else {
        private$.human_densities <- value
      }
    }

    # Errors and warnings #
    
    # error_messages    [inherited]
    
    # warning_messages  [inherited]
    
  ) # end active
)
