#' R6 Class Representing a Barrier Sea Ice Model.
#'
#' @description
#' R6 class functionality for modelling sea, ice and other barriers to dispersal within
#' a spatially-explicit population model. The barrier model calculates distance
#' multipliers to modify distance-based dispersal rates for simulated migrations in
#' a spatio-temporal frictional landscape. The frictional landscape is defined via
#' conductance values, the inverse of friction, which ranges from zero (barrier) to one
#' (no friction) with values in-between representing some friction. For example, a 
#' conductance value of 1/5 = 0.2 represents a landscape in which simulated animals move
#' 5 times slower than a non-friction landscape. In this example the resultant distance
#' multiplier would be 5, thus reducing the effective dispersal range.
#'
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom R6 R6Class
#' @include GenericModel.R
#' @export BarrierSeaIceModel

BarrierSeaIceModel <- R6Class("BarrierSeaIceModel",
  inherit = GenericModel,
  public = list(

    ## Attributes ##
    
    # Model attributes #
    
    #' @field model_attributes A list of public model attributes (name-value pairs) - another stored as private.
    model_attributes = c(), # private only (see below)
    
    ## Methods ##
    
    # Inherited methods (from GenericClass & GenericModel) #
    #   initialize(attribute_aliases = NULL, params = list(), ...)
    #   new_clone(...)
    #   read_from_rds(path)
    #   save_to_rds(path)
    #   get_attribute_names()
    #   get_attributes(params)
    #   set_attributes(params = list(), ...)
    
    # New methods #
    
    # 
    #' @description
    #' Calculates and returns spatio-temporal dispersal distance multipliers for each in-range migration.
    #' @param dispersal_indices Two-column matrix representing the target and source coordinate index for each in-range migration.
    #' @param ... Parameters passed via a \emph{params} list or individually.
    #' @return Temporal list of dispersal distance multiplier arrays with values for each in-range migration.
    calculate_distance_multipliers = function(dispersal_indices = NULL, ...) {

      # Set attributes
      if (length(list(...))) {
        self$set_attributes(...)
      }
      
      # Ensure coordinates and the barrier/sea-ice matrix are set
      if (is.null(self$coordinates) || is.null(self$barrier_sea_ice_matrix)) {
        stop("Distance multipliers calculation requires coordinates and the barrier/sea-ice matrix to be set first", call. = FALSE)
      }
      
      # Ensure dispersal indices are correctly set and are consistent with coordinates
      if (is.null(dispersal_indices) || !all(is.integer(dispersal_indices)) || !all(dispersal_indices >= 1) ||
          !is.matrix(dispersal_indices) || ncol(dispersal_indices) != 2 ||
          nrow(dispersal_indices) > nrow(self$coordinates)^2 ||
          max(dispersal_indices) > nrow(self$coordinates)) {
        stop("Dispersal indices must be a two-column matrix representing the target and source coordinate index for each in-range migration", call. = FALSE)
      }
      
      tryCatch({

        suppressWarnings({
          # Calculate raster, transition matrix, then least cost distances for no barrier/sea-ice (no friction)
          WGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
          no_friction_rast <- raster::rasterFromXYZ(cbind(self$coordinates, array(1, nrow(self$coordinates))), crs = WGS84)
          no_friction_transitions <- gdistance::transition(no_friction_rast, transitionFunction = mean, directions = self$transition_directions)
          no_friction_transitions <- gdistance::geoCorrection(no_friction_transitions, type = "c", scl = TRUE, multpl = FALSE)
          no_friction_cost_matrix <- as.matrix(gdistance::costDistance(no_friction_transitions, as.matrix(self$coordinates)))
          no_friction_costs <- no_friction_cost_matrix[dispersal_indices]
          no_friction_transitions <- NULL; no_friction_cost_matrix <- NULL # release from memory
        })

        # Calculate the (within range) distance multipliers for each time step in parallel
        doParallel::registerDoParallel(cores = self$parallel_cores)
        self <- self # Ensure that this object consistently becomes available within each parallel thread
        distance_multipliers <- foreach(i = 1:ncol(self$barrier_sea_ice_matrix), .errorhandling = c("stop")) %dopar% {
          suppressWarnings({
            # Calculate raster, transition matrix, then least cost distances for barrier/sea-ice for time step
            WGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
            conductance_rast <- raster::rasterFromXYZ(cbind(self$coordinates, self$barrier_sea_ice_matrix[,i]), crs = WGS84)
            conductance_transitions <- gdistance::transition(conductance_rast, transitionFunction = mean, directions = self$transition_directions)
            conductance_transitions <- gdistance::geoCorrection(conductance_transitions, type = "c", scl = TRUE, multpl = FALSE)
            conductance_cost_matrix <- as.matrix(gdistance::costDistance(conductance_transitions, as.matrix(self$coordinates)))
            conductance_costs <- conductance_cost_matrix[dispersal_indices]
            conductance_transitions <- NULL; conductance_cost_matrix <- NULL # release from memory
          })
          conductance_costs/no_friction_costs # return to parallel rbind
        }

      },
      error = function(e){
        self$error_messages <- unique(c(self$error_messages, gsub("\n", "", as.character(e), fixed = TRUE)))
      })
      doParallel::stopImplicitCluster()

      if (!is.null(self$error_messages)) {
        error_messages <- self$error_messages; self$error_messages <- NULL
        stop(paste(c("Encountered in calculating distance multipliers:", error_messages), collapse = "\n"), call. = FALSE)
      } else {
        return(distance_multipliers)
      }
    }

  ), # end public

  private = list(

    ## Attributes ##
    
    # .attribute_aliases [inherited]
    
    # Model attributes #
    .model_attributes = c("parallel_cores", "transition_directions", "coordinates", "barrier_sea_ice_matrix"),
    .parallel_cores = 1,
    .transition_directions = 8,
    .coordinates = NULL,
    .barrier_sea_ice_matrix = NULL,
    
    # Attributes accessible via model get/set methods #
    .active_attributes = c("parallel_cores", "transition_directions", "coordinates", "barrier_sea_ice_matrix")
    
    # Errors and warnings #
    # .error_messages    [inherited]
    # .warning_messages  [inherited]
    
  ), # end private

  # Active binding accessors for private attributes (above)
  active = list(

    # attribute_aliases [inherited]
    
    #' @field parallel_cores Number of cores for running the simulations in parallel.
    parallel_cores = function(value) {
      if (missing(value)) {
        private$.parallel_cores
      } else {
        private$.parallel_cores <- value
      }
    },

    #' @field transition_directions Number of transition directions or neighbours in which cells are connected: usually 4, 8 (default), or 16 (see gdistance package).
    transition_directions = function(value) {
      if (missing(value)) {
        private$.transition_directions
      } else {
        private$.transition_directions <- value
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
          if (!is.null(private$.barrier_sea_ice_matrix) && nrow(private$.barrier_sea_ice_matrix) != nrow(private$.coordinates)) {
            stop("Coordinates must be consistent with barrier/sea-ice matrix dimensions", call. = FALSE)
            private$.coordinates <- NULL
          }
        }
      }
    },

    #' @field barrier_sea_ice_matrix Matrix of conductance (inverse friction) values (range: 0 = barrier; 0 < some friction < 1; 1 = no friction) for each grid cell (rows) at each simulation time step (columns).
    barrier_sea_ice_matrix = function(value) {
      if (missing(value)) {
        private$.barrier_sea_ice_matrix
      } else {
        if (is.character(value) && file.exists(value)) {
          if (length(grep(".CSV", toupper(value), fixed = TRUE))) {
            private$.barrier_sea_ice_matrix <- as.matrix(read.csv(file = value))
          } else if (length(grep(".RDS", toupper(value), fixed = TRUE))) {
            private$.barrier_sea_ice_matrix <- as.matrix(readRDS(file = value))
          } else {
            private$.barrier_sea_ice_matrix <- as.matrix(read.table(file = value))
          }
        } else {
          if (!is.null(value)) {
            private$.barrier_sea_ice_matrix <- as.matrix(value)
          } else {
            private$.barrier_sea_ice_matrix <- value
          }
        }
        if (!is.null(value)) {
          if (!is.null(private$.coordinates) && nrow(private$.barrier_sea_ice_matrix) != nrow(private$.coordinates)) {
            stop("Barrier/sea-ice matrix dimensions must be consistent with coordinates", call. = FALSE)
            private$.barrier_sea_ice_matrix <- NULL
          }
        }
      }
    }

    # error_messages    [inherited]
    
    # warning_messages  [inherited]
    
  ) # end active
)
