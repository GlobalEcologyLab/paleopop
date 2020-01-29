#' R6 Class Representing a Generic Population Model
#'
#' @description
#' R6 class representing a generic (abstract) spatially-explicit demographic-based
#' population model provided as an example template.
#'
#' @examples
#' # Model for sample attributes nested with a template model for fixed attributes
#' template_model <- PopulationModel$new(params = list(duration = 10, populations = 5),
#'                                       coordinates = array(1:10, c(5, 2)),
#'                                       correlation_matrix = (diag(5)*0.8 + 0.2))
#' nested_model <- PopulationModel$new(template = template_model)
#' nested_model$set_sample_attributes(params = c("initial_abundances",
#'                                               "carrying_capacities",
#'                                               "dispersal_matrix"),
#'                                    human_densities = array((1:50)/50, c(5, 10)))
#' # Set sample attributes with aliases
#' nested_model$set_sample_aliases(abundances = "initial_abundances")
#' nested_model$get_sample_aliases()
#' nested_model$set_sample_attributes(params = list(dispersals = (diag(5)*-0.1 + 0.1),
#'                                                  abundances = seq(10, 30, by = 5)),
#'                                    capacities = matrix(seq(50, 32, by = -2),
#'                                                        nrow = 5, ncol = 10,
#'                                                        byrow = TRUE))
#' nested_model$get_template()$get_attributes()
#' nested_model$get_attributes()
#' # Consistency and completeness
#' nested_model$is_consistent()
#' nested_model$is_complete()
#' nested_model$initial_abundances <- nested_model$initial_abundances[-1]
#' nested_model$is_complete()
#' nested_model$list_completeness()
#' nested_model$incomplete_attributes()
#'
#' @importFrom R6 R6Class
#' @include GenericModel.R
#' @export PopulationModel

PopulationModel <- R6Class("PopulationModel",
  inherit = GenericModel,
  public = list(

    ## Attributes ##

    # Model attributes #

    #' @field model_attributes A vector of public model attribute names - another stored as private.
    model_attributes = c(), # private only (see below)

    ## Methods ##

    # Inherited methods (from GenericClass & GenericModel) #
    #   new_clone(...)
    #   read_from_rds(path)
    #   save_to_rds(path)
    #   set_attributes(params = list(), ...)

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method sets template model and sets given attributes individually and/or from a list.
    #' @param template Template population model containing fixed (non-sampled) attributes.
    #' @param ... Parameters passed via a \emph{params} list or individually.
    initialize = function(template = NULL, ...) {
      if (!is.null(template)) {
        self$template_model <- template
      }
      super$initialize(...)
    },

    #' @description
    #' Returns a list of all attribute names including public and private model attributes, as well as attached attributes (including those from the template model).
    #' @return List of all attribute names.
    get_attribute_names = function() {
      if (is.null(self$template_model)) {
        return(super$get_attribute_names())
      } else {
        return(unique(c(self$template_model$get_attribute_names(), names(self$attached))))
      }
    },

    # New methods #

    #' @description
    #' Sets the names (only - when \emph{params} is a vector) and values (when \emph{params} is a list and/or when name-value pairs are provided) of the sample attributes for the model.
    #' @param params List of parameters/attributes (names and values) or array of names only.
    #' @param ... Parameters/attributes passed individually.
    set_sample_attributes = function(params = list(), ...) {
      if (is.list(params) && length(params)) { # add attributes and values
        for (i in 1:length(params)) { # substitute aliases
          if (names(params)[i] %in% names(self$attribute_aliases))
          {
            names(params)[i] <- self$attribute_aliases[[names(params)[i]]]
          }
        }
        private$.sample_attributes <- unique(c(private$.sample_attributes,names(params)))
        self$set_attributes(params = params)
      } else if (length(params)) { # set array of sample attribute names
        for (i in 1:length(params)) { # substitute aliases
          if (params[i] %in% names(self$attribute_aliases))
          {
            params[i] <- self$attribute_aliases[[params[i]]]
          }
        }
        private$.sample_attributes <- as.vector(params)
        for (param in params) { # attach any attributes not present
          if (!(param %in% private$.model_attributes || param %in% names(self$attached))) {
            self$attached[[param]] <- NA
          }
        }
      }
      indiv_params <- list(...)
      if (length(indiv_params)) {
        for (i in 1:length(indiv_params)) { # substitute aliases
          if (names(indiv_params)[i] %in% names(self$attribute_aliases))
          {
            names(indiv_params)[i] <- self$attribute_aliases[[names(indiv_params)[i]]]
          }
        }
        private$.sample_attributes <- unique(c(private$.sample_attributes,names(indiv_params)))
        self$set_attributes(params = indiv_params)
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
        consistent_array <- array(FALSE, length(params))
        for (i in 1:length(params)) {
          param <- params[i]
          param_value <- eval(parse(text=sprintf("self$%s", param)))
          if (is.null(param_value)) { # ignore incomplete attributes
            consistent_array[i] <- TRUE
          } else {
            if (param %in% c("coordinates") && !is.null(self$populations)) {
              consistent_array[i] <- ((is.data.frame(self$coordinates) || is.matrix(self$coordinates)) &&
                                        nrow(param_value) == self$populations && ncol(param_value) == 2)
            } else if (param %in% c("initial_abundances") && !is.null(self$populations)) {
              consistent_array[i] <- (length(param_value) == self$populations || (is.matrix(param_value) && ncol(param_value) == self$populations))
            } else if (param %in% c("carrying_capacities") && !is.null(self$populations) && !is.null(self$duration)) {
              consistent_array[i] <- (is.matrix(param_value) && nrow(param_value) == self$populations && ncol(param_value) == self$duration)
            } else if (param %in% c("dispersal_matrix", "correlation_matrix") && !is.null(self$populations)) {
              consistent_array[i] <- ((self$populations == 1 && length(param_value) == 1) ||
                                        (is.matrix(param_value) && nrow(param_value) == self$populations && ncol(param_value) == self$populations))
            } else if (param %in% c("human_densities") && !is.null(self$populations) && !is.null(self$duration)) {
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
        (is.numeric(self$duration) && self$duration > 0) &&
        (is.numeric(self$populations) && self$populations > 0) &&
        (is.null(self$coordinates) || (is.numeric(self$coordinates$x) && is.numeric(self$coordinates$y) && self$is_consistent("coordinates"))) &&
        (is.numeric(self$initial_abundances) && all(self$initial_abundances >= 0) && self$is_consistent("initial_abundances")) &&
        (is.numeric(self$carrying_capacities) && all(self$carrying_capacities >= 0) && self$is_consistent("carrying_capacities")) &&
        (is.null(self$dispersal_matrix) || (is.numeric(self$dispersal_matrix) && all(self$dispersal_matrix >= 0) &&
                                              all(self$dispersal_matrix <= 1) && self$is_consistent("dispersal_matrix"))) &&
        (is.null(self$correlation_matrix) || (is.numeric(self$correlation_matrix) && all(self$correlation_matrix >= 0) &&
                                                all(self$correlation_matrix <= 1) && self$is_consistent("correlation_matrix"))) &&
        (is.null(self$human_densities) || (is.numeric(self$human_densities) && all(self$human_densities >= 0) &&
                                             all(self$human_densities <= 1) && self$is_consistent("human_densities")))
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
        if (param %in% c("duration", "populations")) {
          complete_list[[param]] <- (is.numeric(param_value) && param_value > 0)
        } else if (param %in% c("coordinates")) {
          complete_list[[param]] <- (is.null(param_value) || (is.numeric(param_value$x) && is.numeric(param_value$y) && self$is_consistent(param)))
        } else if (param %in% c("initial_abundances", "carrying_capacities")) {
          complete_list[[param]] <- (is.numeric(param_value) && all(param_value >= 0) && self$is_consistent(param))
        } else if (param %in% c("dispersal_matrix", "correlation_matrix", "human_densities")) {
          complete_list[[param]] <- (is.null(param_value) || (is.numeric(param_value) && all(param_value >= 0) &&
                                                                all(param_value <= 1) && self$is_consistent(param)))
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
          incomplete <- append(incomplete, param)
        }
      }
      return(incomplete)
    }

  ), # end public

  private = list(

    ## Attributes ##

    # .attribute_aliases [inherited]

    # Model attributes #
    .model_attributes = c("duration", "populations", "coordinates", "initial_abundances", "carrying_capacities",
                          "dispersal_matrix", "correlation_matrix", "human_densities"),
    .duration = NULL,
    .populations = NULL,
    .coordinates = NULL,
    .initial_abundances = NULL,
    .carrying_capacities = NULL,
    .dispersal_matrix = NULL,
    .correlation_matrix = NULL,
    .human_densities = NULL,

    # Attributes accessible via model get/set methods #
    .active_attributes = c("duration", "populations", "coordinates", "initial_abundances", "carrying_capacities",
                           "dispersal_matrix", "correlation_matrix", "human_densities"),

    # Template model for fixed (non-sampled) attributes for shallow cloning
    .template_model = NULL,

    # Vector of sample attributes (names)
    .sample_attributes = NULL

  ), # end private

  # Active binding accessors for private model attributes (above) #
  active = list(

    #' @field attribute_aliases A list of alternative alias names for model attributes (form: alias = "attribute") to be used with the set and get attributes methods.
    attribute_aliases = function(value) {
      if (is.null(self$template_model)) {
        if (missing(value)) {
          private$.attribute_aliases
        } else {
          private$.attribute_aliases <- value
        }
      } else { # nested
        if (missing(value)) {
          self$template_model$attribute_aliases
        } else {
          self$template_model$attribute_aliases <- value
        }
      }
    },

    #' @field duration Number of simulation time steps.
    duration = function(value) {
      if (is.null(self$template_model) || "duration" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.duration
        } else {
          private$.duration <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$duration
        } else {
          self$template_model$duration <- value
        }
      }
    },

    #' @field populations Number of populations.
    populations = function(value) {
      if (is.null(self$template_model) || "populations" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.populations
        } else {
          private$.populations <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$populations
        } else {
          self$template_model$populations <- value
        }
      }
    },

    #' @field coordinates Data frame (or matrix) of X-Y population coordinates (WGS84) in longitude (degrees West) and latitude (degrees North).
    coordinates = function(value) {
      if (is.null(self$template_model) || "coordinates" %in% self$sample_attributes) {
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
      } else {
        if (missing(value)) {
          self$template_model$coordinates
        } else {
          if (is.character(value) && file.exists(value)) {
            if (length(grep(".CSV", toupper(value), fixed = TRUE))) {
              self$template_model$coordinates <- read.csv(file = value)[, 1:2]
            } else if (length(grep(".RDS", toupper(value), fixed = TRUE))) {
              self$template_model$coordinates <- readRDS(file = value)
            } else {
              self$template_model$coordinates <- read.table(file = value)[, 1:2]
            }
          } else {
            if (!is.null(value)) {
              self$template_model$coordinates <- as.data.frame(value)
            } else {
              self$template_model$coordinates <- value
            }
          }
          if (!is.null(value)) {
            names(self$template_model$coordinates) <- c("x", "y")
          }
        }
      }
    },

    #' @field initial_abundances Array or matrix of initial abundances for each population.
    initial_abundances = function(value) {
      if (is.null(self$template_model) || "initial_abundances" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.initial_abundances
        } else {
          private$.initial_abundances <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$initial_abundances
        } else {
          self$template_model$initial_abundances <- value
        }
      }
    },

    #' @field carrying_capacities Matrix of carrying capacities ($populations rows by $duration columns).
    carrying_capacities = function(value) {
      if (is.null(self$template_model) || "carrying_capacities" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.carrying_capacities
        } else {
          private$.carrying_capacities <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$carrying_capacities
        } else {
          self$template_model$carrying_capacities <- value
        }
      }
    },

    #' @field dispersal_matrix Matrix of dispersal (migration) rates between populations (target $populations rows by source $populations columns).
    dispersal_matrix = function(value) {
      if (is.null(self$template_model) || "dispersal_matrix" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.dispersal_matrix
        } else {
          private$.dispersal_matrix <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$dispersal_matrix
        } else {
          self$template_model$dispersal_matrix <- value
        }
      }
    },

    #' @field correlation_matrix Matrix of environmental correlation between populations ($populations rows by $populations columns).
    correlation_matrix = function(value) {
      if (is.null(self$template_model) || "correlation_matrix" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.correlation_matrix
        } else {
          if ("correlation_matrix" %in% self$sample_attributes && self$populations > 1) { # sample multiplier
            private$.correlation_matrix <- value*self$template_model$correlation_matrix
          } else {
            private$.correlation_matrix <- value
          }
        }
      } else {
        if (missing(value)) {
          self$template_model$correlation_matrix
        } else {
          self$template_model$correlation_matrix <- value
        }
      }
    },

    #' @field human_densities Matrix of human density (fraction) ($populations rows by $duration columns).
    human_densities = function(value) {
      if (is.null(self$template_model) || "human_densities" %in% self$sample_attributes) {
        if (missing(value)) {
          private$.human_densities
        } else {
          private$.human_densities <- value
        }
      } else {
        if (missing(value)) {
          self$template_model$human_densities
        } else {
          self$template_model$human_densities <- value
        }
      }
    },

    #' @field template_model Nested fixed-attribute template model.
    template_model = function(value) {
      if (missing(value)) {
        private$.template_model
      } else {
        if ("PopulationModel" %in% class(value)) {
          private$.template_model <- value
        } else { # construct a model having the same (potentially inherited) class as the template
          eval(parse(text=sprintf("private$.template_model <- %s$new(params = as.list(value))", class(self)[1])))
        }
      }
    },

    #' @field sample_attributes List of sample attribute names (only).
    sample_attributes = function(value) {
      if (missing(value)) {
        private$.sample_attributes
      } else {
        if (is.list(value)) {
          params <- names(value)
        } else {
          params <- c(value)
        }
        self$set_sample_attributes(params = params)
      }
    }

  ) # end active
)
