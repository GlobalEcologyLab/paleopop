#' R6 Class Representing a Generic Model.
#'
#' @description
#' R6 class with generic (abstract) functionality for toolset models, including model
#' attribute get and set methods that resolve attribute scope (public, active, attached),
#' attribute aliases, attribute attachment, and error and warning message attributes.
#'
#' @examples
#' TestInheritedModel <-
#'   R6::R6Class("TestInheritedModel",
#'               inherit = GenericModel,
#'               public = list(model_attributes = c("attr1", "attr2"),
#'                             attr1 = NULL, attr2 = NULL),
#'               private = list(.model_attributes = c("attr3", "attr4"),
#'                              .active_attributes = c("attr3"),
#'                              .attr3 = NULL, .attr4 = NULL),
#'               active = list(attr3 = function(value) {
#'                               if (missing(value)) {
#'                                 private$.attr3
#'                               } else {
#'                                 private$.attr3 <- value
#'                               }
#'                             })
#'              )
#' params <- list(attr1 = 101, attr3 = c("a","b"), dog = array(7,3))
#' inherited_model <- TestInheritedModel$new(params = params,
#'                                           attr2 = 202,
#'                                           attr4 = c("c","d"),
#'                                           cat = array(9,3))
#' inherited_model
#' inherited_model$attached
#' inherited_model$get_attribute_names()
#' inherited_model$get_attributes()
#' inherited_model$get_attributes(c("attr2","attr4","cat"))
#' inherited_model$set_attributes(list(attr1 = 110, attr3 = c("x","y")),
#'                                attr2 = 220,
#'                                attr4 = c("w","x"),
#'                                dog = array(8,3),
#'                                horse = "fast",
#'                                warning_messages = "animals attached")
#' inherited_model
#' inherited_model$attached
#' inherited_model$attribute_aliases <- list(a1 = "attr1", c = "cat", warnings = "warning_messages")
#' inherited_model$get_attributes(c("a1", "c", "warnings"))
#' inherited_model$set_attributes(list(a1 = 111), c = array(3,3), warnings = c("new warning"))
#' inherited_model$get_attributes(c("a1", "c", "warnings"))
#' inherited_model$get_attributes()
#' inherited_model$warning_messages
#'
#' @importFrom R6 R6Class
#' @include GenericClass.R
#' @export GenericModel

GenericModel <- R6Class("GenericModel",
  inherit = GenericClass,
  public = list(

    ## Attributes ##

    # Model attributes #

    #' @field model_attributes A vector of public model attribute names - another stored as private.
    model_attributes = c(), # none in base class

    # Dynamically attached attributes ##

    #' @field attached A list of dynamically attached attributes (name-value pairs).
    attached = list(),

    ## Methods ##

    # Inherited methods (from GenericClass) #
    #   read_from_rds(path)
    #   save_to_rds(path)

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method sets given attributes individually and/or from a list.
    #' @param attribute_aliases A list of alternative alias names for model attributes (form: alias = "attribute") to be used with the set and get attributes methods.
    #' @param params Parameters passed via a list.
    #' @param ... Parameters passed individually.
    initialize = function(attribute_aliases = NULL, params = list(), ...) {
      if (!is.null(attribute_aliases)) {
        self$attribute_aliases <- attribute_aliases
      }
      self$set_attributes(params = params, ...)
      super$initialize(...)
    },

    #' @description
    #' Creates a new (re-initialized) object of the current (inherited) object class with optionally passed parameters.
    #' @param ... Parameters passed via the inherited class constructor (defined in initialize and run via new).
    #' @return New object of the inherited class.
    new_clone = function(...) {
      return(super$new_clone(attribute_aliases = self$attribute_aliases, ...))
    },

    # New methods #

    #' @description
    #' Returns an array of all attribute names including public and private model attributes, as well as attached attributes, error and warning messages.
    #' @return Array of all attribute names.
    get_attribute_names = function() {
      return(c(self$model_attributes, private$.model_attributes, names(self$attached), "error_messages", "warning_messages"))
    },

    #' @description
    #' Returns a list of values for selected attributes or attribute aliases (when array of parameter names provided) or all attributes (when no params).
    #' @param params Array of attribute names to return (all when NULL).
    #' @return List of selected or all attributes values.
    get_attributes = function(params = NULL) {
      attribute_list <- list()
      if (is.null(params)) {
        params <- self$get_attribute_names()
      }
      for (param in params) {
        if (param %in% names(self$attribute_aliases)) {
          attribute <- self$attribute_aliases[[param]]
        } else {
          attribute <- param
        }
        if (attribute %in% self$model_attributes || attribute %in% private$.active_attributes) {
          eval(parse(text=sprintf("attribute_list$%s <- self$%s", param, attribute)))
        } else if (attribute %in% private$.model_attributes) {
          eval(parse(text=sprintf("attribute_list$%s <- private$.%s", param, attribute)))
        } else if (attribute %in% c("attribute_aliases", "error_messages", "warning_messages")) {
          eval(parse(text=sprintf("attribute_list$%s <- self$%s", param, attribute)))
        } else if (attribute %in% names(self$attached)) {
          eval(parse(text=sprintf("attribute_list$%s <- self$attached$%s", param, attribute)))
        } else {
          eval(parse(text=sprintf("attribute_list$%s <- NA", param)))
        }
      }
      return(attribute_list)
    },

    #' @description
    #' Sets given attributes (optionally via alias names) individually and/or from a list.
    #' @param params List of parameters/attributes.
    #' @param ... Parameters/attributes passed individually.
    set_attributes = function(params = list(), ...) {
      params <- c(list(...), params) # prioritise individual parameters
      for (param in names(params)) {
        if (param %in% names(self$attribute_aliases)) {
          attribute <- self$attribute_aliases[[param]]
        } else {
          attribute <- param
        }
        if (attribute %in% self$model_attributes || attribute %in% private$.active_attributes) {
          eval(parse(text=sprintf("self$%s <- params$%s", attribute, param)))
        } else if (attribute %in% private$.model_attributes) {
          eval(parse(text=sprintf("private$.%s <- params$%s", attribute, param)))
        } else if (attribute %in% c("attribute_aliases", "error_messages", "warning_messages")) {
          eval(parse(text=sprintf("self$%s <- params$%s", attribute, param)))
        } else if (attribute %in% c("object_generator")) {
          # ignore - handled in generic class
        } else { # attach
          eval(parse(text=sprintf("self$attached$%s <- params$%s", attribute, param)))
        }
      }
    },

    #' @description
    #' Returns an array of attribute names and aliases for specified or all attributes.
    #' @param params Array of attribute names for names/aliases to return (all when NULL).
    #' @return Array of selected or all attribute names and aliases.
    get_attribute_aliases = function(params = NULL) {
      if (is.null(params)) {
        params <- self$get_attribute_names()
      } else {
        params <- params[which(params %in% self$get_attribute_names())]
      }
      return(c(params, names(self$attribute_aliases[which(unlist(self$attribute_aliases) %in% params)])))
    }

  ), # end public

  private = list(

    ## Attributes ##

    .attribute_aliases = list(),

    # Model attributes #
    .model_attributes = c(), # none in base class

    # Attributes accessible via model get/set methods #
    .active_attributes = c(),

    # Errors and warnings #
    .error_messages = NULL,
    .warning_messages = NULL

  ), # end private

  # Active binding accessors for private attributes (above) #
  active = list(

    #' @field attribute_aliases A list of alternative alias names for model attributes (form: alias = "attribute") to be used with the set and get attributes methods.
    attribute_aliases = function(value) {
      if (missing(value)) {
        private$.attribute_aliases
      } else {
        private$.attribute_aliases <- value
      }
    },

    #' @field error_messages A vector of error messages encountered when setting model attributes.
    error_messages = function(value) {
      if (missing(value)) {
        private$.error_messages
      } else {
        private$.error_messages <- value
      }
    },

    #' @field warning_messages A vector of warning messages encountered when setting model attributes.
    warning_messages = function(value) {
      if (missing(value)) {
        private$.warning_messages
      } else {
        private$.warning_messages <- value
      }
    }

  ) # end active
)
