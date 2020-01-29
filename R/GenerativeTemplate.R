#' R6 Class Representing a Nested Container for Generative Model Attributes
#'
#' @description
#' R6 class representing a nested container for generative model attributes that are
#' maintained when new model clones are created. The container maintains input and
#' output attribute names, file and function templates, and any inherited class model
#' attributes that need to be maintained when cloning.
#'
#' @importFrom R6 R6Class
#' @export GenerativeTemplate

GenerativeTemplate <- R6Class("GenerativeTemplate",
  public = list(

    ## Attributes ##

    ## Methods ##

    #' @description
    #' Initialization method initializes the generative templates.
    initialize = function() {
      self$file_templates <- list()
      self$function_templates <- list()
    }

  ), # end public

  private = list(

    ## Attributes ##
    .description = NULL,
    .inputs = NULL,
    .outputs = NULL,
    .file_templates = NULL,
    .function_templates = NULL

  ), # end private

  # Active binding accessors for private attributes (above) #
  active = list(

    #' @field description A brief description of what the generative model generates.
    description = function(value) {
      if (missing(value)) {
        private$.description
      } else {
        private$.description <- value
      }
    },

    #' @field inputs An array of input attribute names for the generative model.
    inputs = function(value) {
      if (missing(value)) {
        private$.inputs
      } else {
        private$.inputs <- value
      }
    },

    #' @field outputs An array of output attribute names for the generative model.
    outputs = function(value) {
      if (missing(value)) {
        private$.outputs
      } else {
        private$.outputs <- value
      }
    },

    #' @field file_templates A list of file template attributes.
    file_templates = function(value) {
      if (missing(value)) {
        private$.file_templates
      } else {
        private$.file_templates <- value
      }
    },

    #' @field function_templates A list of function template attributes.
    function_templates = function(value) {
      if (missing(value)) {
        private$.function_templates
      } else {
        private$.function_templates <- value
      }
    }

  ) # end active
)
