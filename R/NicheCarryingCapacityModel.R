#' R6 Class Representing a Niche Carrying Capacity Model.
#'
#' @description
#' R6 class functionality for modelling niche carrying capacities within a
#' spatially-explicit population model. Typically the model calculates carrying
#' capacities and initial abundances for each sample simulation using data files
#' generated via a climate niche model. It dynamically generates attributes
#' defined as \emph{outputs} (default: \emph{carrying_capacities} and
#' \emph{initial_abundances}) given sampled \emph{inputs} (default:
#' \emph{niche_breadth}, \emph{niche_cuts} and \emph{niche_cuts}).
#'
#' @importFrom R6 R6Class
#' @include GenerativeModel.R
#' @include NicheCarryingCapacityTemplate.R
#' @export NicheCarryingCapacityModel

NicheCarryingCapacityModel <- R6Class("NicheCarryingCapacityModel",
  inherit = GenerativeModel,
  public = list(

    ## Attributes ##

    # Model attributes #

    #' @field model_attributes A list of public model attributes (name-value pairs) - another stored as private.
    model_attributes = c(), # private only (see below)

    ## Methods ##

    # Inherited methods (from GenericClass, GenericModel & GenerativeModel) #
    #   new_clone(...)
    #   read_from_rds(path)
    #   save_to_rds(path)
    #   get_attribute_names()
    #   get_attributes(params)
    #   set_attributes(params = list(), ...)
    #   generate(input_values = list())
    #   add_file_template(param, path_template, path_params = c(), file_type = "RDS")
    #   add_function_template(param, source_path, call_template, call_params = c())
    #   read_file(param)
    #   run_function(param)
    #   add_generative_requirements(params = list(), ...)
    #   generative_requirements_satisfied()

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method sets the generative template and requirements as well as any attributes passed via a \emph{params} list or individually.
    #' @param generative_template Optional nested object for generative niche carrying capacity model attributes that need to be maintained when a new clone object is generated for a sample simulation.
    #' @param generative_requirements Optional list of attribute names and the template setting (\emph{"file"} or \emph{"function"}) that is required to generate their values (default is \emph{"file"}).
    #' @param attribute_aliases Optional list of extra alias names for model attributes (form: alias = "attribute") to be used with the set and get attributes methods.
    #' @param ... Parameters passed via a \emph{params} list or individually.
    initialize = function(generative_template = NULL, generative_requirements = NULL, attribute_aliases = NULL, ...) {
      if (is.null(generative_template)) { # when new object
        self$generative_template <- NicheCarryingCapacityTemplate$new()
        attribute_aliases <- c(attribute_aliases, # Append default aliases
                               list(breadth = "niche_breadth", cuts = "niche_cuts", max_n = "density"))
        if (!("description" %in% names(list(...)))) {
          self$description <- "Niche carrying capacity"
        }
        if (!("inputs" %in% names(list(...)))) {
          self$inputs <- c("niche_breadth", "niche_cuts", "density")
        }
        if (!("outputs" %in% names(list(...)))) {
          self$outputs <- c("carrying_capacities", "initial_abundances")
        }
      }
      if (is.null(generative_requirements)) {
        generative_requirements <- list(carrying_capacities = "file")
      }
      super$initialize(generative_template = generative_template,
                       generative_requirements = generative_requirements,
                       attribute_aliases = attribute_aliases, ...)
    }

  ), # end public

  private = list(

    ## Attributes ##

    # .attribute_aliases       [inherited]

    # Generative attributes #
    # .generative_template     [inherited]
    # .generative_requirements [inherited]

    # Model attributes #
    .model_attributes = c("niche_breadth", "niche_cuts", "density", "carrying_capacities",
                          "initial_abundances"),
    .niche_breadth = NULL,
    .niche_cuts = NULL,
    .density = NULL,
    .carrying_capacities = NULL,
    .initial_abundances = NULL,

    # Attributes accessible via model get/set methods #
    .active_attributes = c("description", "inputs", "outputs", "file_templates", "function_templates",
                           "niche_breadth", "niche_cuts", "density", "carrying_capacities",
                           "initial_abundances")

    # Errors and warnings #
    # .error_messages    [inherited]
    # .warning_messages  [inherited]

  ), # end private

  # Active binding accessors for private attributes (above and template nested)
  active = list(

    # attribute_aliases [inherited]

    # Generative attribute accessors #

    # generative_template [inherited]

    # generative_requirements [inherited]

    # Model attribute accessors #

    # Nested model attribute accessors #

    # description [inherited]

    # inputs [inherited]

    # outputs [inherited]

    # file_templates [inherited]

    # function_templates [inherited]

    # Local (non-nested) model attribute accessors #

    #' @field niche_breadth Sampled niche model breadth (typically part of the identifier for a climate niche model data file).
    niche_breadth = function(value) {
      if (missing(value)) {
        private$.niche_breadth
      } else {
        private$.niche_breadth <- value
      }
    },

    #' @field niche_cuts Sampled niche model cuts (typically part of the identifier for a climate niche model data file).
    niche_cuts = function(value) {
      if (missing(value)) {
        private$.niche_cuts
      } else {
        private$.niche_cuts <- value
      }
    },

    #' @field density Sampled maximum density of model organism per simulation grid cell.
    density = function(value) {
      if (missing(value)) {
        private$.density
      } else {
        private$.density <- value
      }
    },

    #' @field carrying_capacities Calculated carrying capacity based on niche model parameters and density.
    carrying_capacities = function(value) {
      if (missing(value)) {
        if (is.null(private$.carrying_capacities) && "carrying_capacities" %in% names(self$generative_requirements)) {
          template_type <- self$generative_requirements[["carrying_capacities"]]
          if (template_type == "file") {
            private$.carrying_capacities <- self$read_file("carrying_capacities")
            if (is.null(self$density)) {
              self$warning_messages <- unique(append(self$warning_messages, "Missing density parameter for generating carrying capacity"))
              private$.carrying_capacities <- NULL
            }
            if (!is.null(private$.carrying_capacities)) {
              private$.carrying_capacities <- private$.carrying_capacities*self$density
              nonzero_indices <- which(private$.carrying_capacities > 0)
              private$.carrying_capacities[nonzero_indices] <- round(private$.carrying_capacities[nonzero_indices])
            }
          } else if (template_type == "function") {
            private$.carrying_capacities <- self$run_function("carrying_capacities")
          }
        }
        private$.carrying_capacities
      } else {
        private$.carrying_capacities <- value
      }
    },

    #' @field initial_abundances Calculated initial abundances based on carrying capacity.
    initial_abundances = function(value) {
      if (missing(value)) {
        if (is.null(private$.initial_abundances)) {
          if ("initial_abundances" %in% names(self$generative_requirements)) {
            template_type <- round(self$generative_requirements[["initial_abundances"]])
            if (template_type == "file") {
              private$.initial_abundances <- self$read_file("initial_abundances")
            } else if (template_type == "function") {
              private$.initial_abundances <- self$run_function("initial_abundances")
            }
          } else { # use the first (t=1) column of carrying capacities
            if (!is.null(self$carrying_capacities)) {
              if (length(dim(self$carrying_capacities)) == 2) {
                private$.initial_abundances <- self$carrying_capacities[, 1]
              } else { # vector
                private$.initial_abundances <- self$carrying_capacities
              }
            }
          }
        }
        private$.initial_abundances
      } else {
        private$.initial_abundances <- value
      }
    }

  ) # end active
)
