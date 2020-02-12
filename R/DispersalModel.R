#' R6 Class Representing a Dispersal Model.
#'
#' @description
#' R6 class functionality for modelling dispersals within a spatially-explicit
#' population model. The model calculates dispersal rates between population model
#' cells using a distance-based function: \emph{p*exp(-distance/b)} for
#' \emph{distance <= r} (otherwise zero), where \emph{p} (proportion), \emph{b}
#' (breadth or average distance) and \emph{r} (range or maximum distance) are
#' configurable model attributes. The dispersal rates are adjusted to limit
#' emigration from each cell to \emph{p}. The model also generates data for
#' constructing compacted dispersal matrices. It dynamically generates attributes
#' defined as \emph{outputs} (default: \emph{dispersal_data}) given sampled
#' \emph{inputs} (default: \emph{dispersal_proportion} and
#' \emph{dispersal_max_distance}).
#'
#'
#' @importFrom R6 R6Class
#' @include GenerativeModel.R
#' @include DispersalTemplate.R
#' @export DispersalModel

DispersalModel <- R6Class("DispersalModel",
  inherit = GenerativeModel,
  public = list(

    ## Attributes ##

    # Model attributes #

    #' @field model_attributes A vector of public model attribute names - another stored as private.
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

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method sets the generative template and requirements, optionally the barrier/sea-ice model, as well as any attributes passed via a \emph{params} list or individually.
    #' @param generative_template Optional nested object for generative dispersal model attributes that need to be maintained when a new clone object is generated for a sample simulation.
    #' @param generative_requirements Optional list of attribute names and the template setting (\emph{"file"} or \emph{"function"}) that is required to generate their values (otherwise default functionality is used).
    #' @param barrier_sea_ice_model Optional \emph{BarrierSeaIceModel} (or inherited class) object for dispersal distance multiplier data.
    #' @param attribute_aliases Optional list of extra alias names for model attributes (form: alias = "attribute") to be used with the set and get attributes methods.
    #' @param ... Parameters passed via a \emph{params} list or individually.
    initialize = function(generative_template = NULL,
                          generative_requirements = NULL,
                          barrier_sea_ice_model = NULL,
                          attribute_aliases = NULL, ...) {
      if (is.null(generative_template)) { # when new object
        self$generative_template <- DispersalTemplate$new()
        attribute_aliases <- c(attribute_aliases, # Append default aliases
                               list(proportion = "dispersal_proportion", dispersal_p = "dispersal_proportion", p = "dispersal_proportion",
                                    breadth = "dispersal_breadth", dispersal_b = "dispersal_breadth", b = "dispersal_breadth",
                                    max_distance = "dispersal_max_distance", dispersal_r = "dispersal_max_distance", r = "dispersal_max_distance"))
        if (!("description" %in% names(list(...)))) {
          self$description <- "Dispersal"
        }
        if (!("inputs" %in% names(list(...)))) {
          self$inputs <- c("dispersal_proportion", "dispersal_max_distance")
        }
        if (!("outputs" %in% names(list(...)))) {
          self$outputs <- c("dispersal_data")
        }
      }
      if (is.null(generative_requirements)) {
        generative_requirements <- list(dispersal_data = "default")
      }
      super$initialize(generative_template = generative_template,
                       generative_requirements = generative_requirements,
                       attribute_aliases = attribute_aliases, ...)
      if (!is.null(barrier_sea_ice_model)) {
        self$barrier_sea_ice_model <- barrier_sea_ice_model
      }
    },

    #' @description
    #' Returns a boolean to indicate that all the default, file and/or function template settings that are required for attribute generation are present.
    #' @return Boolean to indicate that the required settings for attribute generation are present.
    generative_requirements_satisfied = function() {
      satisfied <- super$generative_requirements_satisfied()
      if (is.list(satisfied)) {
        dispersal_matrix_default <- ("dispersal_matrix" %in% names(satisfied) && self$generative_requirements$dispersal_matrix == "default")
        if (dispersal_matrix_default) {
          satisfied$dispersal_matrix <- (!is.null(self$coordinates) && !is.null(self$distance_classes) &&
                                           !is.null(self$dispersal_function_data) && !is.null(self$distance_data))
        }
        dispersal_data_default <- ("dispersal_data" %in% names(satisfied) && self$generative_requirements$dispersal_data == "default")
        if (dispersal_data_default) {
          satisfied$dispersal_data <- (!is.null(self$coordinates) && !is.null(self$distance_classes) &&
                                         !is.null(self$dispersal_function_data))
          if (!is.null(self$barrier_sea_ice_model)) {
            satisfied$dispersal_data <- (satisfied$dispersal_data &&
                                           !is.null(self$barrier_sea_ice_model$barrier_sea_ice_matrix))
          }
          satisfied$dispersal_data <- (satisfied$dispersal_data && !is.null(self$distance_data))
        }
        # Add any attributes that are missing (for error message)
        if (dispersal_matrix_default || dispersal_data_default) {
          if (is.null(self$coordinates)) {
            satisfied$coordinates <- FALSE
          }
          if (is.null(self$distance_classes)) {
            satisfied$distance_classes <- FALSE
          }
          if (is.null(self$dispersal_function_data)) {
            satisfied$dispersal_function_data <- FALSE
          }
        }
        if (dispersal_data_default && !is.null(self$barrier_sea_ice_model)) {
          if (is.null(self$barrier_sea_ice_model$barrier_sea_ice_matrix)) {
            satisfied$barrier_sea_ice_matrix <- FALSE
          }
        }
        if (dispersal_matrix_default || dispersal_data_default) {
          if (is.null(self$distance_data)) {
            satisfied$distance_data <- FALSE
          }
        }
      }
      return(satisfied)
    },

    # New methods #

    #' @description
    #' Sets the distance classes to a sequence of values from minimum to maximum in steps of interval size.
    #' @param minimum Minimum or first distance class sequence value (default = 1).
    #' @param maximum Maximum or last distance class value (default = 10).
    #' @param interval Interval or distance class sequence step size  (default = (maximum - minimum)/10).
    set_distance_classes = function(minimum = 1, maximum = 10, interval = (maximum - minimum)/10) {
      self$distance_classes <- seq(minimum, maximum, interval)
    },

    #' @description
    #' Returns a matrix with the calculated distance between each pair of population cells (via \emph{coordinates}).
    #' @return Matrix with distances between population cells (\emph{population} rows by \emph{population} columns).
    calculate_distance_matrix = function() {
      if (!is.null(self$coordinates)) {
        return(geosphere::distm(self$coordinates, self$coordinates, fun=geosphere::distGeo)/1000)
      } else {
        stop("Distance matrix calculation requires coordinates to be set first", call. = FALSE)
      }
    },

    #' @description
    #' Calculates the distance class for within-range populations using the set/provided distance classes. Also calculates indices for constructing compact matrices.
    #' @param distance_matrix Optional pre-calculated matrix with distances between population cells (population rows by population columns).
    #' @param ... Parameters passed via a \emph{params} list or individually.
    calculate_distance_data = function(distance_matrix = NULL, ...) {

      # Set attributes
      self$distance_data <- NULL
      if (length(list(...))) {
        self$set_attributes(...)
      }

      # Ensure coordinates and distance classes are set
      if (is.null(self$coordinates) || is.null(self$distance_classes)) {
        stop("Distance data calculation requires coordinates and distance classes to be set first", call. = FALSE)
      }

      # Ensure pre-calculated distance matrix is consistent with coordinates
      if (!is.null(distance_matrix) && !is.null(self$coordinates)) {
        if (!is.matrix(distance_matrix) || nrow(distance_matrix) != nrow(self$coordinates) || ncol(distance_matrix) != nrow(self$coordinates)) {
          stop("Distance matrix dimensions must be consistent with coordinates", call. = FALSE)
        }
      }

      # Calculate distance matrix: populations by populations matrix of pairwise distances between grid centroids (km)
      if (is.null(distance_matrix)) {
        distance_matrix <- self$calculate_distance_matrix()
      }

      # Calculate the indices of distances within the maximum dispersal range
      distance_data <- which(distance_matrix > 0 & distance_matrix <= max(self$distance_classes), arr.ind = TRUE)
      distance_data <- distance_data[order(distance_data[, 2], distance_data[, 1]),]
      colnames(distance_data) <- c("target_pop", "source_pop")
      distances_within_range <- distance_matrix[distance_data]
      distance_matrix <- NULL # release from memory

      # Calculate barrier/sea-ice model distance multipliers for the within range indices
      if (!is.null(self$barrier_sea_ice_model)) {
        distance_multipliers <- self$barrier_sea_ice_model$calculate_distance_multipliers(distance_data)
      }

      # Calculate and append indices for constructing compact distance matrices
      distance_data <- data.frame(distance_data)
      populations <- nrow(self$coordinates)
      dispersal_rows <- tabulate(distance_data$source_pop, nbins = populations)
      compact_rows <- max(dispersal_rows)
      compact_matrix <- array(1:compact_rows, c(compact_rows, populations))
      compact_matrix <- compact_matrix*(compact_matrix <= matrix(dispersal_rows, nrow = compact_rows, ncol = populations, byrow = TRUE))
      # Map the row of each compact matrix to the original target population
      distance_data$compact_row <- which(compact_matrix > 0, arr.ind = TRUE, useNames = FALSE)[, 1]

      # Calculate base (no friction) distance classes for distances within the maximum dispersal range
      base_distance_classes <- as.numeric(cut(distances_within_range, breaks = c(1, self$distance_classes)))
      self$distance_data <- list(base = data.frame(distance_data, distance_class = base_distance_classes))
      
      # Calculate the sequential changes in distance class using barrier/sea-ice model distance multipliers
      if (!is.null(self$barrier_sea_ice_model)) {
        current_distance_classes <- base_distance_classes
        sequential_distance_data <- list()
        for (i in 1:length(distance_multipliers)) {
          previous_distance_classes <- current_distance_classes
          current_distance_classes <- as.numeric(cut(distances_within_range*distance_multipliers[[i]], breaks = c(1, self$distance_classes, Inf)))
          changed_indices <- which(current_distance_classes != previous_distance_classes)
          sequential_distance_data[[i]] <- data.frame(distance_data[changed_indices,], distance_class = current_distance_classes[changed_indices])
        }
        distance_multipliers <- NULL # release from memory
        self$distance_data$changes <- sequential_distance_data
      }
      
    },

    #' @description
    #' Calculates a dispersal matrix, or data frame of dispersal rates and compact indices (default), using the conditional dispersal function with dispersal limiting for a simulation sample.
    #' @param type Optional type selector (\emph{"data"} or \emph{"matrix"}) to determine whether to calculate a dispersal matrix or data frame (default).
    #' @return Returns character string message when calculation prerequisites are not met (for simulation logging).
    calculate_dispersals = function(type = "data") {

      # Ensure distance data are calculated
      if (is.null(self$distance_data) || is.null(self$distance_data$base) ||
          (!is.null(self$barrier_sea_ice_model) && is.null(self$distance_data$changes))) {
        return("Dispersal distance data needs to be (re-)calculated before dispersals can be generated")
      }
      
      # Calculate dispersals using distance data and sampled dispersal function parameters
      if (!is.null(self$dispersal_proportion) && !is.null(self$dispersal_breadth) && !is.null(self$dispersal_max_distance)) {

        # Calculate dispersal rates for each distance class (discrete values)
        dispersal_rate_classes <- c(ifelse(self$distance_classes <= self$dispersal_max_distance, self$dispersal_proportion*exp(-1*self$distance_classes/self$dispersal_breadth), 0), 0)

        # Select base (non-friction) data for non-zero dispersal classes
        nonzero_base_data <- self$distance_data$base[which(self$distance_data$base$distance_class <= length(which(dispersal_rate_classes > 0))), ]
        
        # Calculate a compact matrix of dispersals for the base (non-friction) data (original compact indices)
        base_compact_rows <- max(self$distance_data$base$compact_row)
        populations <- nrow(self$coordinates)
        compact_matrix <- array(0, c(base_compact_rows, populations))
        compact_dispersal_indices <- as.matrix(nonzero_base_data[, c("compact_row", "source_pop")])
        compact_matrix[compact_dispersal_indices] <- dispersal_rate_classes[nonzero_base_data$distance_class]

        # Calculate multipliers to set the total proportion migrating from each cell (without friction) to p
        multipliers <- self$dispersal_proportion/.colSums(compact_matrix, m = base_compact_rows, n = populations)
        multipliers[which(!is.finite(multipliers))] <- 1
        if (type == "data" && !is.null(self$barrier_sea_ice_model)) {
          multipliers[which(multipliers > 1)] <- 1 #DISCUSS# always
        }
        
        # Apply multipliers to the base compact dispersal matrix
        compact_matrix <- compact_matrix*matrix(multipliers, nrow = base_compact_rows, ncol = populations, byrow = TRUE)

        # Extract dispersal rates and round when required (then update non-zero base/non-friction dispersal data)
        nonzero_base_data$dispersal_rate <- compact_matrix[compact_dispersal_indices]
        if (!is.null(self$decimals)) {
          nonzero_base_data$dispersal_rate <- round(nonzero_base_data$dispersal_rate, self$decimals)
          nonzero_base_data <- nonzero_base_data[which(nonzero_base_data$dispersal_rate > 0),]
          compact_dispersal_indices <- as.matrix(nonzero_base_data[, c("compact_row", "source_pop")])
        }

        # Calculate indices for constructing further compacted dispersal matrices for emigrants and immigrants
        dispersal_rows <- tabulate(nonzero_base_data$source_pop, nbins = populations)
        dispersal_cols <- tabulate(nonzero_base_data$target_pop, nbins = populations)
        nonzero_compact_rows <- max(dispersal_rows, dispersal_cols)
        compact_emigrant_matrix <- array(1:nonzero_compact_rows, c(nonzero_compact_rows, populations))
        compact_immigrant_matrix <- compact_emigrant_matrix*(compact_emigrant_matrix <= matrix(dispersal_cols, nrow = nonzero_compact_rows, ncol = populations, byrow = TRUE))
        compact_emigrant_matrix <- compact_emigrant_matrix*(compact_emigrant_matrix <= matrix(dispersal_rows, nrow = nonzero_compact_rows, ncol = populations, byrow = TRUE))
        # Map the row of each compact matrix to the original target (for emigrants) or source (for immigrants) populations
        nonzero_base_data$emigrant_row <- which(compact_emigrant_matrix > 0, arr.ind = TRUE, useNames = FALSE)[,1]
        nonzero_base_data$immigrant_row <- which(compact_immigrant_matrix > 0, arr.ind = TRUE, useNames = FALSE)[,1]
        target_sorted_indices <- nonzero_base_data[order(nonzero_base_data$target_pop, nonzero_base_data$source_pop), c("target_pop", "source_pop")]
        nonzero_base_data$immigrant_row <- nonzero_base_data$immigrant_row[order(target_sorted_indices$source_pop, target_sorted_indices$target_pop)]

        # Calculate the sequential changes in dispersals when barrier/sea-ice model is present
        if (type == "data" && !is.null(self$barrier_sea_ice_model)) {
          
          # Calculate the initial dispersal data by applying the first (friction) distance changes to the (compact) base data
          compact_matrix[as.matrix(self$distance_data$changes[[1]][, c("compact_row", "source_pop")])] <-
            dispersal_rate_classes[self$distance_data$changes[[1]]$distance_class]*multipliers[self$distance_data$changes[[1]]$source_pop]
          
          # Construct the dispersal data from the base (non-friction) data for the non-zero base indices (ensures all indices present for changes)
          self$dispersal_data <- list(data.frame(nonzero_base_data[, c("target_pop", "source_pop", "emigrant_row", "immigrant_row")],
                                                 dispersal_rate = compact_matrix[compact_dispersal_indices]))
          compact_matrix <- NULL  # release from memory
          
          # Round when required
          if (!is.null(self$decimals)) {
            self$dispersal_data[[1]]$dispersal_rate <- round(self$dispersal_data[[1]]$dispersal_rate, self$decimals)
          }
          
          # Map the original distance classes and the new emigrant and immigrant row indices via compact matrices
          original_distance_class_map <- emigrant_row_map <- immigrant_row_map <- array(NA, c(base_compact_rows, populations))
          original_distance_class_map[as.matrix(self$distance_data$base[, c("compact_row", "source_pop")])] <- self$distance_data$base$distance_class
          emigrant_row_map[as.matrix(nonzero_base_data[, c("compact_row", "source_pop")])] <- nonzero_base_data$emigrant_row
          immigrant_row_map[as.matrix(nonzero_base_data[, c("compact_row", "source_pop")])] <- nonzero_base_data$immigrant_row
          
          # Calculate subsequent changes in dispersals
          for (i in 2:length(self$distance_data$changes)) {
            
            # Select data for non-zero dispersal classes
            original_distance_classes <- original_distance_class_map[as.matrix(self$distance_data$changes[[i]][, c("compact_row", "source_pop")])]
            nonzero_dispersal_indices <- which(original_distance_classes <= length(which(dispersal_rate_classes > 0)))
            nonzero_change_data <- self$distance_data$changes[[i]][nonzero_dispersal_indices,]
            
            # Calculate/construct dispersal rates using class and multiplier (based on non-friction data)
            self$dispersal_data[[i]] <- data.frame(nonzero_change_data[, c("target_pop", "source_pop")],
                                                   emigrant_row = emigrant_row_map[as.matrix(nonzero_change_data[, c("compact_row", "source_pop")])],
                                                   immigrant_row = immigrant_row_map[as.matrix(nonzero_change_data[, c("compact_row", "source_pop")])],
                                                   dispersal_rate = dispersal_rate_classes[nonzero_change_data$distance_class]*multipliers[nonzero_change_data$source_pop])
            # Round when required
            if (!is.null(self$decimals) && length(self$dispersal_data[[i]]) > 0) {
              self$dispersal_data[[i]]$dispersal_rate <- round(self$dispersal_data[[i]]$dispersal_rate, self$decimals)
            }
            
          }
          
        } else { # matrix or no barrier/sea-ice model
          
          # Set dispersals from non-zero dispersal data
          if (type == "matrix") {
            self$dispersal_matrix <- array(0, c(populations, populations))
            self$dispersal_matrix[as.matrix(nonzero_base_data[, c("target_pop", "source_pop")])] <- nonzero_base_data$dispersal_rate
          } else {
            self$dispersal_data <- list(nonzero_base_data[, c("target_pop", "source_pop", "emigrant_row", "immigrant_row", "dispersal_rate")])
          }
          
        }

      } else {
        return("Dispersal calculation requires sample parameter settings for proportion, breadth & maximum distance (look-up data may be missing)")
      }
    }

  ), # end public

  private = list(

    ## Attributes ##

    # .attribute_aliases       [inherited]

    # Generative attributes #
    # .generative_template     [inherited]
    # .generative_requirements [inherited]
    .barrier_sea_ice_model = NULL,
    
    # Model attributes #
    .model_attributes = c("coordinates", "distance_classes", "distance_data", "dispersal_function_data",
                          "decimals", "proportion_multiplier", "dispersal_proportion", "dispersal_breadth",
                          "dispersal_max_distance", "dispersal_index", "dispersal_matrix", "dispersal_data"),
    .dispersal_proportion = NULL,
    .dispersal_breadth = NULL,
    .dispersal_max_distance = NULL,
    .dispersal_index = NULL,
    .dispersal_matrix = NULL,
    .dispersal_data = NULL,

    # Attributes accessible via model get/set methods #
    .active_attributes = c("description", "inputs", "outputs", "file_templates", "function_templates",
                           "coordinates", "distance_classes", "distance_data", "dispersal_function_data",
                           "decimals", "proportion_multiplier", "dispersal_proportion", "dispersal_breadth",
                           "dispersal_max_distance", "dispersal_index", "dispersal_matrix", "dispersal_data")

    # Errors and warnings #
    # .error_messages    [inherited]
    # .warning_messages  [inherited]

  ), # end private

  # Active binding accessors for private attributes (above and template nested) #
  active = list(

    # attribute_aliases [inherited]

    # Generative attribute accessors #

    # generative_template [inherited]

    # generative_requirements [inherited]

    #' @field barrier_sea_ice_model A \emph{BarrierSeaIceModel} (or inherited class) object for dispersal distance multiplier data.
    barrier_sea_ice_model = function(value) {
      if (missing(value)) {
        self$generative_template$barrier_sea_ice_model
      } else {
        if (!is.null(value) && !("BarrierSeaIceModel" %in% class(value))) {
          stop("Dispersal barrier/sea-ice model must be a BarrierSeaIceModel or inherited class object", call. = FALSE)
        } else if (!is.null(value)) {
          # Protect consistency of existing distance data associated with an existing barrier/sea-ice model
          if (!is.null(self$distance_data) && !is.null(self$barrier_sea_ice_model)) {
            stop("Dispersal model distance data is already associated with the existing barrier/sea-ice model", call. = FALSE)
          # Check coordinates consistency
          } else if (!is.null(value$coordinates) && !is.null(self$generative_template$coordinates) &&
                     (nrow(value$coordinates) != nrow(self$generative_template$coordinates) ||
                      !all(value$coordinates == self$generative_template$coordinates))) {
            stop("Dispersal model coordinates are inconsistent with the barrier/sea-ice model", call. = FALSE)
          } else if (is.null(value$coordinates) && !is.null(self$generative_template$coordinates) &&
                     !is.null(value$barrier_sea_ice_matrix) &&
                     nrow(value$barrier_sea_ice_matrix) != nrow(self$generative_template$coordinates)) {
            stop("Dispersal model coordinates are inconsistent with the barrier/sea-ice matrix dimensions", call. = FALSE)
          } else if (!is.null(self$distance_data)) {
            # Existing distance data will lack temporal changes
            warning("Dispersal model distance data will need to be re-calculated with the barrier/sea-ice model", call. = FALSE)
            self$distance_data <- NULL
          }
          # Copy coordinates appropriately
          if (is.null(value$coordinates) && !is.null(self$generative_template$coordinates)) {
            value$coordinates <- self$generative_template$coordinates
          } else if (!is.null(value$coordinates) && is.null(self$generative_template$coordinates)) {
            self$generative_template$coordinates <- value$coordinates
          }
          self$generative_template$barrier_sea_ice_model <- value
        } else { # set model (to NULL)
          self$generative_template$barrier_sea_ice_model <- value
          if (!is.null(self$distance_data) && "changes" %in% names(self$distance_data)) {
            self$distance_data$changes <- NULL
          }
        }
      }
    },
    
    # Model attribute accessors #

    # Nested model attribute accessors #

    # description [inherited]

    # inputs [inherited]

    # outputs [inherited]

    # file_templates [inherited]

    # function_templates [inherited]

    #' @field coordinates Data frame (or matrix) of X-Y population coordinates (WGS84) in longitude (degrees West) and latitude (degrees North).
    coordinates = function(value) {
      if (missing(value)) {
        self$generative_template$coordinates
      } else {
        if (!is.null(self$distance_data)) {
          stop("Dispersal model distance data is already associated with the existing coordinates", call. = FALSE)
        } else {
          if (is.character(value) && file.exists(value)) {
            if (length(grep(".CSV", toupper(value), fixed = TRUE))) {
              coordinates <- read.csv(file = value)[, 1:2]
            } else if (length(grep(".RDS", toupper(value), fixed = TRUE))) {
              coordinates <- readRDS(file = value)
            } else {
              coordinates <- read.table(file = value)[, 1:2]
            }
          } else {
            if (!is.null(value)) {
              coordinates <- as.data.frame(value)
            } else {
              coordinates <- value
            }
          }
          if (!is.null(value)) {
            names(coordinates) <- c("x", "y")
          }
          self$generative_template$coordinates <- coordinates
        }
      }
    },

    #' @field distance_classes Vector of distance interval boundaries (in km) for calculating discrete dispersal rates.
    distance_classes = function(value) {
      if (missing(value)) {
        self$generative_template$distance_classes
      } else {
        if (!is.null(self$distance_data)) {
          stop("Dispersal model distance data is already associated with the existing distance classes", call. = FALSE)
        } else {
          self$generative_template$distance_classes <- value
        }
      }
    },

    #' @field distance_data Data frame of distance classes including indices for the construction of compact matrices (columns: target_pop, source_pop, compact_row, distance_class).
    distance_data = function(value) {
      if (missing(value)) {
        self$generative_template$distance_data
      } else {
        self$generative_template$distance_data <- value
      }
    },

    #' @field dispersal_function_data Data frame of discrete dispersal function values. Optional first column may provide distance intervals (non-inclusive lower bounds).
    dispersal_function_data = function(value) {
      if (missing(value)) {
        self$generative_template$dispersal_function_data
      } else {
        if (is.character(value) && file.exists(value)) {
          if (length(grep(".CSV", toupper(value), fixed = TRUE))) {
            self$generative_template$dispersal_function_data <- read.csv(file = value)
          } else if (length(grep(".RDS", toupper(value), fixed = TRUE))) {
            self$generative_template$dispersal_function_data <- readRDS(file = value)
          } else {
            self$generative_template$dispersal_function_data <- read.table(file = value)
          }
        } else {
          if (!is.null(value)) {
            self$generative_template$dispersal_function_data <- as.data.frame(value)
          } else {
            self$generative_template$dispersal_function_data <- value
          }
        }
      }
    },

    #' @field decimals Optional number of decimal places applied to the calculated dispersal rates (default: NULL = no rounding).
    decimals = function(value) {
      if (missing(value)) {
        self$generative_template$decimals
      } else {
        self$generative_template$decimals <- value
      }
    },

    #' @field proportion_multiplier Optional constant multiplier to be applied to the dispersal function \emph{p} parameter. May be used to represent the fraction of the population capable of migrating.
    proportion_multiplier = function(value) {
      if (missing(value)) {
        self$generative_template$proportion_multiplier
      } else {
        self$generative_template$proportion_multiplier <- value
      }
    },

    # Local (non-nested) model attribute accessors #

    #' @field dispersal_proportion Dispersal function: \emph{p*exp(-distance/b)} \emph{p} parameter. Represents the proportion and limit of dispersers between model cells.
    dispersal_proportion = function(value) {
      if (missing(value)) {
        if (!is.null(private$.dispersal_proportion) && !is.null(self$proportion_multiplier)) {
          private$.dispersal_proportion*self$proportion_multiplier
        } else {
          private$.dispersal_proportion
        }
      } else {
        private$.dispersal_proportion <- value
      }
    },

    #' @field dispersal_breadth Dispersal function: \emph{p*exp(-distance/b)} \emph{b} parameter. Represents the breadth of the dispersal between model cells. Typically estimated via average migration distance.
    dispersal_breadth = function(value) {
      if (missing(value)) {
        if (!is.null(self$dispersal_function_data) && !is.null(self$dispersal_index) &&
            any(self$get_attribute_aliases("dispersal_breadth") %in% names(self$dispersal_function_data))) {
          # Use function look-up data
          function_data_column <- which(names(self$dispersal_function_data) %in% self$get_attribute_aliases("dispersal_breadth"))
          self$dispersal_function_data[[function_data_column]][self$dispersal_index]
        } else {
          private$.dispersal_breadth
        }
      } else {
        private$.dispersal_breadth <- value
      }
    },

    #' @field dispersal_max_distance Dispersal maximum distance or range (\emph{r}) parameter limits the use of the dispersal function: \emph{p*exp(-distance/b)}. The function is utilized when \emph{distance <= r} otherwise the dispersal rate is set to zero.
    dispersal_max_distance = function(value) {
      if (missing(value)) {
        if (!is.null(self$dispersal_function_data) && !is.null(self$dispersal_index) &&
            any(names(self$dispersal_function_data) %in% self$get_attribute_aliases("dispersal_max_distance"))) {
          # Use function look-up data
          function_data_column <- which(names(self$dispersal_function_data) %in% self$get_attribute_aliases("dispersal_max_distance"))
          self$dispersal_function_data[[function_data_column]][self$dispersal_index]
        } else {
          private$.dispersal_max_distance
        }
      } else {
        private$.dispersal_max_distance <- value
        if (!is.null(self$dispersal_function_data) && is.null(self$dispersal_index) && is.numeric(value)) {
          # Use maximum distance to derive function data index
          if (!(names(self$dispersal_function_data)[1] %in% self$get_attribute_aliases(c("dispersal_proportion", "dispersal_breadth", "dispersal_max_distance")))) {
            # Assume first column provides distance intervals (non-inclusive lower bounds)
            self$dispersal_index <- as.numeric(cut(value, breaks = c(self$dispersal_function_data[, 1], Inf)))
          } else if (any(names(self$dispersal_function_data) %in% self$get_attribute_aliases("dispersal_max_distance"))) {
            # Use max_distance column as distance intervals (non-inclusive lower bounds)
            function_data_column <- which(names(self$dispersal_function_data) %in% self$get_attribute_aliases("dispersal_max_distance"))
            self$dispersal_index <- as.numeric(cut(value, breaks = c(self$dispersal_function_data[[function_data_column]], Inf)))
          }
        }
      }
    },

    #' @field dispersal_index Sampled index for the dispersal function data frame (to look-up dispersal function parameters).
    dispersal_index = function(value) {
      if (missing(value)) {
        private$.dispersal_index
      } else {
        private$.dispersal_index <- value
        if (is.numeric(value) && !is.null(self$dispersal_function_data)) {
          parameter_values <- as.list(self$dispersal_function_data[value,])
          self$set_attributes(parameter_values)
        }
      }
    },

    #' @field dispersal_matrix Dispersal matrix calculated via dispersal function.
    dispersal_matrix = function(value) {
      if (missing(value)) {
        if (is.null(private$.dispersal_matrix) && "dispersal_matrix" %in% names(self$generative_requirements)) {
          template_type <- self$generative_requirements[["dispersal_matrix"]]
          if (template_type == "file") {
            private$.dispersal_matrix <- self$read_file("dispersal_matrix")
          } else if (template_type == "function") {
            private$.dispersal_matrix <- self$run_function("dispersal_matrix")
          } else if (template_type == "default") { # use internal function
            message <- self$calculate_dispersals(type = "matrix")
            if (is.character(message)) {
              self$warning_messages <- unique(append(self$warning_messages, message))
            }
          }
        }
        private$.dispersal_matrix
      } else {
        private$.dispersal_matrix <- value
      }
    },

    #' @field dispersal_data Data frame of non-zero dispersal rates including indices for the construction of compact matrices (columns: target_pop, source_pop, emigrant_row, immigrant_row, dispersal_rate).
    dispersal_data = function(value) {
      if (missing(value)) {
        if (is.null(private$.dispersal_data) && "dispersal_data" %in% names(self$generative_requirements)) {
          template_type <- self$generative_requirements[["dispersal_data"]]
          if (template_type == "file") {
            private$.dispersal_data <- self$read_file("dispersal_data")
          } else if (template_type == "function") {
            private$.dispersal_data <- self$run_function("dispersal_data")
          } else if (template_type == "default") { # use internal function
            message <- self$calculate_dispersals(type = "data")
            if (is.character(message)) {
              self$warning_messages <- unique(append(self$warning_messages, message))
            }
          }
        }
        private$.dispersal_data
      } else {
        private$.dispersal_data <- value
      }
    }

    # Errors and warnings #
    
    # error_messages    [inherited]
    
    # warning_messages  [inherited]
    
  ) # end active
)
