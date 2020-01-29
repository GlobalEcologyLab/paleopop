#' R6 Class Representing a Nested Container for Niche Carrying Capacity Model Attributes
#'
#' @description
#' R6 class representing a nested container for generative model attributes that are
#' maintained when new model clones are created. The container maintains input and
#' output attribute names, file and function templates, and niche carrying capacity
#' model attributes that need to be maintained when a new clone object is generated
#' for a sample simulation.
#'
#' @importFrom R6 R6Class
#' @include GenerativeTemplate.R
#' @export NicheCarryingCapacityTemplate

NicheCarryingCapacityTemplate <- R6Class("NicheCarryingCapacityTemplate",
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
    .niche_occupancy_mask = NULL

  ), # end private

  # Active binding accessors for private attributes (above) #
  active = list(

    # description [inherited]

    # inputs [inherited]

    # outputs [inherited]

    # file_templates [inherited]

    # function_templates [inherited]

    #' @field niche_occupancy_mask Optional binary mask (matrix or data frame) for niche carrying capacity time-series data (simulation cells by duration).
    niche_occupancy_mask = function(value) {
      if (missing(value)) {
        private$.niche_occupancy_mask
      } else {
        private$.niche_occupancy_mask <- value
      }
    }

  ) # end active
)
