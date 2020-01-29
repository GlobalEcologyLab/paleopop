#' R6 Class Representing a Population Model for the Default Paleopop Simulator
#'
#' @description
#' R6 class representing a spatially-explicit demographic-based population model with
#' attributes utilized by the default paleopop simulator function.
#'
#' @importFrom R6 R6Class
#' @include PopulationModel.R
#' @export PaleoPopModel

PaleoPopModel <- R6Class("PaleoPopModel",
  inherit = PopulationModel,
  public = list(

    ## Attributes ##

    # Model attributes #

    #' @field model_attributes A vector of public model attribute names - another stored as private.
    model_attributes = c(), # private only (see below)

    ## Methods ##

    # Inherited methods (from GenericClass, GenericModel & PopulationModel)
    #   new_clone(...)
    #   read_from_rds(path)
    #   save_to_rds(path)
    #   get_attribute_names()
    #   get_attributes(params = NULL)
    #   set_attributes(params = list(), ...)
    #   set_sample_attributes(params = list(), ...)
    #   get_template()
    #   set_template(template)

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method sets template model and sets given attributes individually and/or from a list.
    #' @param template Template population model containing fixed (non-sampled) attributes.
    #' @param ... Parameters passed via a \emph{params} list or individually.
    initialize = function(template = NULL, ...) {
      if (is.null(template) && !("results_selection" %in% names(list(...)))) {
        self$results_selection <- c("abundance")
      }
      super$initialize(template = template, ...)
      if (is.null(self$template_model)) {
        self$attribute_aliases <- c(self$attribute_aliases, list(density = "harvest_max_n"))
      }
    },

    #' @description
    #' Returns a boolean to indicate if (optionally selected or all) model attributes (such as dimensions) are consistent.
    #' @param params Optional array of parameter/attribute names.
    #' @return Boolean to indicate consistency of selected/all attributes.
    is_consistent = function(params = NULL) {
      if (is.null(params)) { # all model attributes
        return(self$is_consistent(private$.model_attributes))
      } else { # listed attributes
        params <- c(params)
        consistent_array <- array(FALSE,length(params))
        for (i in 1:length(params)) {
          param <- params[i]
          param_value <- eval(parse(text=sprintf("self$%s",param)))
          if (is.null(param_value)) { # ignore incomplete attributes
            consistent_array[i] <- TRUE
          } else {
            # Population attributes:
            if (param %in% c("coordinates")) {
              consistent_array[i] <- ((is.data.frame(self$coordinates) || is.matrix(self$coordinates)) &&
                                        nrow(param_value) == self$populations && ncol(param_value) == 2)
            } else if (param %in% c("initial_abundances")) {
              consistent_array[i] <- (length(param_value) == self$populations)
            } else if (param %in% c("carrying_capacities")) {
              consistent_array[i] <- (is.matrix(param_value) && nrow(param_value) == self$populations && ncol(param_value) == self$duration)
            # Dispersal and correlation:
            } else if (param %in% c("dispersal_data")) {
              consistent_array[i] <- (is.data.frame(param_value) && ncol(param_value) == 5 &&
                                        min(param_value[, 1:4]) >= 1 && max(param_value[, 1:4]) <= self$populations)
            } else if (param %in% c("compact_decomposition")) {
              consistent_array[i] <- (is.list(param_value) && "matrix" %in% names(param_value) && "map" %in% names(param_value) &&
                                        is.matrix(param_value$matrix) && ncol(param_value$matrix) == self$populations &&
                                        is.matrix(param_value$map) && ncol(param_value$map) == self$populations &&
                                        nrow(param_value$matrix) == nrow(param_value$map) &&
                                        min(param_value$map, na.rm = TRUE) >= 1 &&
                                        max(param_value$map, na.rm = TRUE) <= self$populations)
            # Human impact (harvest) attribute:
            } else if (param %in% c("human_densities")) {
              consistent_array[i] <- ((self$populations == 1 && length(param_value) == self$duration) ||
                                        (is.matrix(param_value) && nrow(param_value) == self$populations && ncol(param_value) == self$duration))
            } else { # other attributes are independent
              consistent_array[i] <- TRUE
            }
          }
        }
        return(all(consistent_array))
      }
    },

    #' @description
    #' Returns a boolean to indicate if all attributes necessary to simulate the model have been set and are consistent.
    #' @return Boolean to indicate model completeness (and consistency).
    is_complete = function() {
      return(
        # General model attributes:
        (is.numeric(self$duration) && self$duration > 0) &&
        (is.numeric(self$years_per_step) && self$years_per_step > 0) &&
        # Transition rate and standard deviation:
        (is.numeric(self$transition_rate) && self$transition_rate >= 0) &&
        (is.numeric(self$standard_deviation) && self$standard_deviation >= 0) &&
        # Population attributes:
        (is.numeric(self$populations) && self$populations > 0) &&
        (is.null(self$coordinates) || (is.numeric(self$coordinates$x) && is.numeric(self$coordinates$y) && self$is_consistent("coordinates"))) &&
        (is.numeric(self$initial_abundances) && all(self$initial_abundances >= 0) && self$is_consistent("initial_abundances")) &&
        (is.numeric(self$growth_rate_max) && self$growth_rate_max >= 0) &&
        (is.null(self$local_threshold) || (is.numeric(self$local_threshold) && self$local_threshold >= 0)) &&
        (is.null(self$occupancy_threshold) || (is.numeric(self$occupancy_threshold) && self$occupancy_threshold >= 0)) &&
        (is.null(self$dispersal_target_k_threshold) || (is.numeric(self$dispersal_target_k_threshold) && self$dispersal_target_k_threshold >= 0)) &&
        (is.numeric(self$carrying_capacities) && all(self$carrying_capacities >= 0) && self$is_consistent("carrying_capacities")) &&
        # Dispersal and correlation attributes:
        (is.null(self$dispersal_data) || (is.numeric(as.matrix(self$dispersal_data)) && all(self$dispersal_data >= 0) && self$is_consistent("dispersal_data"))) &&
        (is.null(self$compact_decomposition) || (is.list(self$compact_decomposition) && "matrix" %in% names(self$compact_decomposition) &&
                                                   "map" %in% names(self$compact_decomposition) && is.numeric(self$compact_decomposition$matrix) &&
                                                   all(self$compact_decomposition$matrix >= 0) && self$is_consistent("compact_decomposition"))) &&
          # Human impact (harvest) attributes:
        is.logical(self$harvest) &&
        (!self$harvest || is.numeric(self$harvest_max)) &&
        (!self$harvest || is.numeric(self$harvest_g)) &&
        (!self$harvest || is.numeric(self$harvest_z)) &&
        (!self$harvest || is.numeric(self$harvest_max_n)) &&
        (!self$harvest || (is.numeric(self$human_densities) && self$is_consistent("human_densities"))) &&
        # Results required selection:
        (is.character(self$results_selection) &&
           all(self$results_selection %in% c("abundance", "ema", "extirpation", "harvested", "occupancy")) &&
           any(c("abundance", "ema", "extirpation", "harvested", "occupancy") %in% self$results_selection))
      )
    },

    #' @description
    #' Returns a list of booleans for each parameter to indicate which attributes necessary to simulate the model have been set and are consistent.
    #' @return List of booleans for each parameter to indicate to indicate completeness (and consistency).
    list_completeness = function() {
      complete_list <- list()
      for (param in private$.model_attributes) {
        param_value <- eval(parse(text=sprintf("self$%s",param)))
        # General model attributes:
        if (param %in% c("duration", "years_per_step")) {
          complete_list[[param]] <- (is.numeric(param_value) && param_value > 0)
        # Transition rate and standard deviation:
        } else if (param %in% c("transition_rate", "standard_deviation")) {
          complete_list[[param]] <- (is.numeric(param_value) && param_value >= 0)
        # Population attributes:
        } else if (param %in% c("populations")) {
          complete_list[[param]] <- (is.numeric(param_value) && param_value > 0)
        } else if (param %in% c("coordinates")) {
          complete_list[[param]] <- (is.null(param_value) || ((is.numeric(param_value$x) && is.numeric(param_value$y) && self$is_consistent(param))))
        } else if (param %in% c("initial_abundances", "carrying_capacities")) {
          complete_list[[param]] <- (is.numeric(param_value) && all(param_value >= 0) && self$is_consistent(param))
        } else if (param %in% c("growth_rate_max")) {
          complete_list[[param]] <- (is.numeric(param_value) && param_value >= 0)
        } else if (param %in% c("local_threshold", "occupancy_threshold", "dispersal_target_k_threshold")) {
          complete_list[[param]] <- (is.null(param_value) || (is.numeric(param_value) && param_value >= 0))
        # Dispersal and correlation attributes:
        } else if (param %in% c("dispersal_data")) {
          complete_list[[param]] <- (is.null(param_value) || (is.numeric(as.matrix(param_value)) && all(param_value >= 0) && self$is_consistent(param)))
        } else if (param %in% c("compact_decomposition")) {
          complete_list[[param]] <- (is.null(param_value) || (is.list(param_value) && "matrix" %in% names(param_value) && "map" %in% names(param_value) &&
                                       is.numeric(param_value$matrix) && all(param_value$matrix >= 0) && self$is_consistent(param)))
        # Human impact (harvest) attributes:
        } else if (param %in% c("harvest")) {
          complete_list[[param]] <- is.logical(param_value)
        } else if (param %in% c("harvest_max", "harvest_g", "harvest_z", "harvest_max_n")) {
          complete_list[[param]] <- is.numeric(param_value)
        } else if (param %in% c("human_densities")) {
          complete_list[[param]] <- (is.numeric(param_value) && self$is_consistent(param))
        # Results required selection:
        } else if (param %in% c("results_selection")) {
          complete_list[[param]] <- (is.character(param_value) &&
                                       all(param_value %in% c("abundance", "ema", "extirpation", "harvested", "occupancy")) &&
                                       any(c("abundance", "ema", "extirpation", "harvested", "occupancy") %in% param_value))
        }
      }
      return(complete_list)
    },

    #' @description
    #' Returns a list of attributes necessary to simulate the model that are incomplete.
    #' @return List of incomplete attributes which prevent the model simulation.
    incomplete_attributes = function() {
      incomplete <- c()
      complete_list <- self$list_completeness()
      for (param in names(complete_list)) {
        if (!complete_list[[param]]) {
          if (param %in% c("harvest_max", "harvest_g", "harvest_z", "harvest_max_n", "human_densities")) {
            is_complete <- (complete_list[["harvest"]] && !self$harvest)
          } else {
            is_complete <- FALSE
          }
          if (!is_complete) {
            incomplete <- append(incomplete, param)
          }
        }
      }
      return(incomplete)
    }

  ), # end public

  private = list(

    ## Attributes ##

    # .attribute_aliases [inherited]

    # Model attributes #
    .model_attributes = c("duration", "years_per_step", "random_seed", "transition_rate", "standard_deviation",
                          "populations", "coordinates", "initial_abundances", "growth_rate_max", "local_threshold",
                          "occupancy_threshold", "dispersal_target_k_threshold", "carrying_capacities",
                          "dispersal_data", "compact_decomposition", "harvest", "harvest_max", "harvest_g",
                          "harvest_z", "harvest_max_n", "human_densities", "results_selection"),
    # .duration                      [inherited]
    .years_per_step = NULL,
    .random_seed = NULL,
    .transition_rate = NULL,
    .standard_deviation = NULL,
    # Population settings:
    # .populations                   [inherited]
    # .coordinates                   [inherited]
    # .initial_abundances            [inherited]
    .growth_rate_max = NULL,
    .local_threshold = NULL,
    .occupancy_threshold = NULL,
    .dispersal_target_k_threshold = NULL,
    # .carrying_capacities           [inherited]
    .dispersal_data = NULL,
    .compact_decomposition = NULL,
    .harvest = NULL,
    .harvest_max = NULL,
    .harvest_g = NULL,
    .harvest_z = NULL,
    .harvest_max_n = NULL,
    # .human_densities               [inherited]
    .results_selection = NULL,

    # Attributes accessible via model get/set methods #
    .active_attributes = c("duration", "years_per_step", "random_seed", "transition_rate", "standard_deviation",
                           "populations", "coordinates", "initial_abundances", "growth_rate_max", "local_threshold",
                           "occupancy_threshold", "dispersal_target_k_threshold", "carrying_capacities",
                           "dispersal_data", "compact_decomposition", "harvest", "harvest_max", "harvest_g",
                           "harvest_z", "harvest_max_n", "human_densities", "results_selection")

    # Template model for fixed (non-sampled) attributes for shallow cloning
    # .template_model                [inherited]

    # Vector of sample attributes (names)
    # .sample_attributes             [inherited]

  ), # end private

  # Active binding accessors for private model attributes (above) #
  active = list(

    # attribute_aliases [inherited]

    # duration [inherited]

    #' @field years_per_step Number of years per time step.
    years_per_step = function(value) {
      if (is.null(self$template_model) || "years_per_step" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.years_per_step
        } else {
          private$.years_per_step <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$years_per_step
        } else {
          self$template_model$years_per_step <- value
        }
      }
    },

    #' @field random_seed Number to seed the random number generation for stochasticity.
    random_seed = function(value) {
      if (is.null(self$template_model) || "random_seed" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.random_seed
        } else {
          private$.random_seed <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$random_seed
        } else {
          self$template_model$random_seed <- value
        }
      }
    },

    #' @field transition_rate Rate of transition (or fecundity) between generations.
    transition_rate = function(value) {
      if (is.null(self$template_model) || "transition_rate" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.transition_rate
        } else {
          private$.transition_rate <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$transition_rate
        } else {
          self$template_model$transition_rate <- value
        }
      }
    },

    #' @field standard_deviation Standard deviation applied to transition rates.
    standard_deviation = function(value) {
      if (is.null(self$template_model) || "standard_deviation" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.standard_deviation
        } else {
          private$.standard_deviation <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$standard_deviation
        } else {
          self$template_model$standard_deviation <- value
        }
      }
    },

    # populations [inherited]

    # coordinates [inherited]

    # initial_abundances [inherited]

    #' @field growth_rate_max Maximum growth rate.
    growth_rate_max = function(value) {
      if (is.null(self$template_model) || "growth_rate_max" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.growth_rate_max
        } else {
          private$.growth_rate_max <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$growth_rate_max
        } else {
          self$template_model$growth_rate_max <- value
        }
      }
    },

    #' @field local_threshold Abundance threshold (that needs to be exceeded) for each population to persist.
    local_threshold = function(value) {
      if (is.null(self$template_model) || "local_threshold" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.local_threshold
        } else {
          private$.local_threshold <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$local_threshold
        } else {
          self$template_model$local_threshold <- value
        }
      }
    },

    #' @field occupancy_threshold Threshold for the number of populations occupied (that needs to be exceeded) for all populations to persist.
    occupancy_threshold = function(value) {
      if (is.null(self$template_model) || "occupancy_threshold" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.occupancy_threshold
        } else {
          private$.occupancy_threshold <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$occupancy_threshold
        } else {
          self$template_model$occupancy_threshold <- value
        }
      }
    },

    #' @field dispersal_target_k_threshold Target population carrying capacity threshold for density dependent dispersal.
    dispersal_target_k_threshold = function(value) {
      if (is.null(self$template_model) || "dispersal_target_k_threshold" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.dispersal_target_k_threshold
        } else {
          private$.dispersal_target_k_threshold <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$dispersal_target_k_threshold
        } else {
          self$template_model$dispersal_target_k_threshold <- value
        }
      }
    },

    # carrying_capacities [inherited]

    #' @field dispersal_data Data frame of non-zero dispersal rates and indices for the construction of compact matrices (columns: target_pop, source_pop, emigrant_row, immigrant_row, dispersal_rate).
    dispersal_data = function(value) {
      if (is.null(self$template_model) || "dispersal_data" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.dispersal_data
        } else {
          private$.dispersal_data <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$dispersal_data
        } else {
          self$template_model$dispersal_data <- value
        }
      }
    },

    #' @field compact_decomposition List containing a pre-calculated compact transposed (Cholesky) decomposition matrix and a corresponding map of population indices for environmental correlation (list names: matrix, map).
    compact_decomposition = function(value) {
      if (is.null(self$template_model) || "compact_decomposition" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.compact_decomposition
        } else {
          private$.compact_decomposition <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$compact_decomposition
        } else {
          self$template_model$compact_decomposition <- value
        }
      }
    },

    #' @field harvest Boolean for utilizing harvesting.
    harvest = function(value) {
      if (is.null(self$template_model) || "harvest" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.harvest
        } else {
          private$.harvest <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$harvest
        } else {
          self$template_model$harvest <- value
        }
      }
    },

    #' @field harvest_max Proportion harvested per year (annual time scale - not generational).
    harvest_max = function(value) {
      if (is.null(self$template_model) || "harvest_max" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.harvest_max
        } else {
          private$.harvest_max <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$harvest_max
        } else {
          self$template_model$harvest_max <- value
        }
      }
    },

    #' @field harvest_g The "G" parameter in the harvest function.
    harvest_g = function(value) {
      if (is.null(self$template_model) || "harvest_g" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.harvest_g
        } else {
          private$.harvest_g <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$harvest_g
        } else {
          self$template_model$harvest_g <- value
        }
      }
    },

    #' @field harvest_z The "Z" parameter in the harvest function.
    harvest_z = function(value) {
      if (is.null(self$template_model) || "harvest_z" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.harvest_z
        } else {
          private$.harvest_z <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$harvest_z
        } else {
          self$template_model$harvest_z <- value
        }
      }
    },

    #' @field harvest_max_n Maximum density per grid cell.
    harvest_max_n = function(value) {
      if (is.null(self$template_model) || "harvest_max_n" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.harvest_max_n
        } else {
          if (is.numeric(value)) { # rounding due to alias/dual usage
            private$.harvest_max_n <- round(value)
          } else {
            private$.harvest_max_n <- value
          }
        }
      } else {
        if (missing(value)) {
          self$template_model$harvest_max_n
        } else {
          if (is.numeric(value)) {
            self$template_model$harvest_max_n <- round(value)
          } else {
            self$template_model$harvest_max_n <- value
          }
        }
      }
    },

    # human_densities [inherited]

    #' @field results_selection List of results selection from ("abundance", "ema", "extirpation", "harvested", "occupancy").
    results_selection = function(value) {
      if (is.null(self$template_model) || "results_selection" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.results_selection
        } else {
          private$.results_selection <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$results_selection
        } else {
          self$template_model$results_selection <- value
        }
      }
    }

    # sample_attributes [inherited]

  ) # end active
)
