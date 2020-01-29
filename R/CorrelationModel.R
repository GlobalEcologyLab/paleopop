#' R6 Class Representing a Correlation Model.
#'
#' @description
#' R6 class functionality for modelling correlations within a spatially-explicit
#' population model. The model calculates correlations between population model
#' cells using a distance-based function: \emph{a*exp(-distance/b)}, where
#' \emph{a} (amplitude) and \emph{b} (breadth) are configurable model
#' attributes. The model also calculates a Cholesky decomposition of the
#' correlation matrix, which is utilized to generate correlated normal deviates.
#'
#' @importFrom R6 R6Class
#' @include GenericModel.R
#' @export CorrelationModel

CorrelationModel <- R6Class("CorrelationModel",
  inherit = GenericModel,
  public = list(

    ## Attributes ##

    # Model attributes #

    #' @field model_attributes A list of public model attributes (name-value pairs) - another stored as private.
    model_attributes = c(), # private only (see below)

    ## Methods ##

    # Inherited methods (from GenericClass & GenericModel) #
    #   new_clone(...)
    #   read_from_rds(path)
    #   save_to_rds(path)
    #   get_attribute_names()
    #   get_attributes(params)
    #   set_attributes(params = list(), ...)

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method sets given attributes individually and/or from a list.
    #' @param compact_only Boolean to indicate that only the compact versions of matrices will be maintained once calculated.
    #' @param attribute_aliases Optional list of extra alias names for model attributes (form: alias = "attribute") to be used with the set and get attributes methods.
    #' @param ... Parameters passed via a \emph{params} list or individually.
    initialize = function(compact_only = TRUE, attribute_aliases = NULL, ...) {
      self$compact_only <- compact_only
      attribute_aliases <- c(attribute_aliases, # Append default aliases
                             list(amplitude = "correlation_amplitude", correlation_a = "correlation_amplitude", a = "correlation_amplitude",
                                  breadth = "correlation_breadth", correlation_b = "correlation_breadth", b = "correlation_breadth"))
      super$initialize( attribute_aliases = attribute_aliases, ...)
    },

    # New methods #

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
    #' Calculates the correlation matrix by applying the distance-based correlation function.
    #' @param distance_matrix Optional pre-calculated matrix with distances between population cells (\emph{population} rows by \emph{population} columns).
    #' @param decimals Optional number of decimal places for correlation values.
    #' @param threshold Optional threshold (minimum value) for correlation values (default 0.0000001).
    #' @param ... Parameters passed via a \emph{params} list or individually.
    calculate_correlations = function(distance_matrix = NULL, decimals = NULL, threshold = 0.0000001, ...) {

      # Set attributes
      self$correlation_matrix <- NULL
      if (length(list(...))) {
        self$set_attributes(...)
      }

      # Ensure coordinates and distance classes are set
      if (is.null(self$coordinates) || is.null(self$correlation_amplitude) || is.null(self$correlation_breadth)) {
        stop("Correlation calculations require coordinates and function parameter settings amplitude and breadth", call. = FALSE)
      }

      # Ensure pre-calculated distance matrix is consistent with coordinates
      if (!is.null(distance_matrix) && !is.null(self$coordinates)) {
        if (!is.matrix(distance_matrix) || nrow(distance_matrix) != nrow(self$coordinates) || ncol(distance_matrix) != nrow(self$coordinates)) {
          stop("Distance matrix dimensions must be consistent with coordinates", call. = FALSE)
        }
      }

      # Populations by population matrix of pairwise distances between grid centroids (km)
      if (is.null(distance_matrix)) {
        distance_matrix <- self$calculate_distance_matrix()
      }

      # Calculate correlation values between each population based on a*exp(-distance/b)
      self$correlation_matrix <- self$correlation_amplitude*exp(-1*distance_matrix/self$correlation_breadth)
      diag(self$correlation_matrix) <- 1

      # Rounding?
      if (!is.null(decimals)) {
        self$correlation_matrix <- round(self$correlation_matrix, as.numeric(decimals))
      } else if (is.numeric(threshold)) {
        self$correlation_matrix[which(self$correlation_matrix < threshold)] <- 0
      }

    },

    #' @description
    #' Calculates the transposed Cholesky decomposition of the correlation matrix.
    #' @param distance_matrix Optional pre-calculated matrix with distances between population cells (population rows by population columns).
    #' @param decimals Optional number of decimal places for correlation values.
    #' @param threshold Optional threshold (minimum value) for correlation values (default 0.0000001).
    #' @param ... Parameters passed via a \emph{params} list or individually.
    calculate_cholesky_decomposition = function(distance_matrix = NULL, decimals = NULL, threshold = 0.0000001, ...) {

      # Set attributes
      self$t_decomposition_matrix <- NULL
      if (length(list(...))) {
        self$set_attributes(...)
      }

      # Calculate correlations when required
      if (is.null(self$correlation_matrix)) {
        self$calculate_correlations(distance_matrix = distance_matrix)
      }

      # Check that the transposed Cholesky decomposition matrix can be calculated
      if (isSymmetric(self$correlation_matrix) && all(eigen(self$correlation_matrix, symmetric = TRUE, only.values = TRUE)$values > 0)) {

        # Calculate the transposed Cholesky decomposition matrix
        self$t_decomposition_matrix <- chol(self$correlation_matrix)
        if (self$compact_only) { # clear the correlation matrix
          self$correlation_matrix <- NULL
        }

        # Rounding?
        if (!is.null(decimals)) {
          self$t_decomposition_matrix <- round(self$t_decomposition_matrix, as.numeric(decimals))
        } else if (is.numeric(threshold)) {
          self$t_decomposition_matrix[which(self$t_decomposition_matrix < threshold)] <- 0
        }

      } else {
        stop("Cholesky decomposition can only be calculated when the correlation matrix is symmetric and has positive eigen values", call. = FALSE)
      }

    },

    #' @description
    #' Compacts the transposed Cholesky decomposition of the correlation matrix into the minimal number of rows, which are mapped to the original matrix.
    #' @param distance_matrix Optional pre-calculated matrix with distances between population cells (population rows by population columns).
    #' @param ... Parameters passed via a \emph{params} list or individually.
    calculate_compact_decomposition = function(distance_matrix = NULL, ...) {

      # Set attributes
      self$t_decomposition_compact_matrix <- NULL
      self$t_decomposition_compact_map <- NULL
      if (length(list(...))) {
        self$set_attributes(...)
      }

      # Calculate Cholesky decomposition when required
      if (is.null(self$t_decomposition_matrix)) {
        self$calculate_cholesky_decomposition(distance_matrix = distance_matrix)
      }

      # Calculate non-zero decomposition data from the decomposition matrix
      t_decomposition_data <- which(self$t_decomposition_matrix != 0, arr.ind = TRUE, useNames = TRUE)
      t_decomposition_data <- as.data.frame(cbind(t_decomposition_data, self$t_decomposition_matrix[t_decomposition_data]))
      if (self$compact_only) { # clear the full decomposition matrix
        self$t_decomposition_matrix <- NULL
      }
      names(t_decomposition_data) <- c("row", "col", "value")
      t_decomposition_data <- t_decomposition_data[order(t_decomposition_data$col, t_decomposition_data$row),]

      # Create a compact transposed decomposition matrix
      populations <- nrow(self$coordinates)
      t_decomposition_nonzero_rows <- tabulate(t_decomposition_data$col, nbins = populations)
      t_decomposition_compact_rows <- max(t_decomposition_nonzero_rows)
      self$t_decomposition_compact_matrix <- array(1:t_decomposition_compact_rows, c(t_decomposition_compact_rows, populations))
      self$t_decomposition_compact_matrix <- self$t_decomposition_compact_matrix*(self$t_decomposition_compact_matrix <= matrix(t_decomposition_nonzero_rows, nrow = t_decomposition_compact_rows, ncol = populations, byrow = TRUE))
      t_decomposition_compact_indices <- which(self$t_decomposition_compact_matrix != 0)
      self$t_decomposition_compact_matrix[t_decomposition_compact_indices] <- t_decomposition_data$value

      # Create a map to the original row populations
      self$t_decomposition_compact_map <- array(NA, c(t_decomposition_compact_rows, populations))
      self$t_decomposition_compact_map[t_decomposition_compact_indices] <- t_decomposition_data$row

    },

    #' @description
    #' Returns a compact transposed Cholesky decomposition of the correlation and a corresponding map of population indices in a list with names: matrix, map.
    #' @param distance_matrix Optional pre-calculated matrix with distances between population cells (population rows by population columns).
    #' @param ... Parameters passed via a \emph{params} list or individually.
    #' @return List containing a compact Cholesky decomposition matrix and a corresponding map of population indices (for the compacted rows).
    get_compact_decomposition = function(distance_matrix = NULL, ...) {

      # Set attributes
      if (length(list(...))) {
        self$set_attributes(...)
      }

      # Calculate compact decomposition when required
      if (!is.null(distance_matrix) || is.null(self$t_decomposition_compact_matrix) || is.null(self$t_decomposition_compact_map)) {
        self$calculate_compact_decomposition(distance_matrix = distance_matrix)
      }

      # Pack the decomposition compact matrix and map into a list
      return(list(matrix = self$t_decomposition_compact_matrix, map = self$t_decomposition_compact_map))

    },

    #' @description
    #' Generates and returns correlated normal deviates from the correlation model, utilizing the optional random seed.
    #' @param random_seed Optional seed for the random generation of correlated deviates.
    #' @return Array/vector of correlated normal deviates.
    generate_correlated_normal_deviates = function(random_seed = NULL) {

      # Ensure compact correlation decomposition is calculated
      if (is.null(self$t_decomposition_compact_matrix) || is.null(self$t_decomposition_compact_map)) {
        stop("The compact correlation decomposition needs to be calculated before correlated normal deviates can be generated", call. = FALSE)
      }

      # Resolve dimensions
      populations <- nrow(self$coordinates)
      compact_rows <- nrow(self$t_decomposition_compact_matrix)

      # Set random seed when present
      if (!is.null(random_seed)) {
        set.seed(random_seed)
      }

      # Generate spatially correlated deviates
      return(.colSums(self$t_decomposition_compact_matrix*rnorm(populations)[self$t_decomposition_compact_map],
                      m = compact_rows, n = populations, na.rm=TRUE))
    }

  ), # end public

  private = list(

    ## Attributes ##

    # .attribute_aliases [inherited]

    # Model attributes #
    .model_attributes = c("coordinates", "correlation_amplitude", "correlation_breadth", "correlation_matrix",
                          "t_decomposition_matrix", "compact_only", "t_decomposition_compact_matrix",
                          "t_decomposition_compact_map"),
    .coordinates = NULL,
    .correlation_amplitude = NULL,
    .correlation_breadth = NULL,
    .correlation_matrix = NULL,
    .t_decomposition_matrix = NULL,
    .compact_only = NULL,
    .t_decomposition_compact_matrix = NULL,
    .t_decomposition_compact_map = NULL,

    # Attributes accessible via model get/set methods #
    .active_attributes = c("coordinates", "correlation_amplitude", "correlation_breadth", "correlation_matrix",
                           "t_decomposition_matrix", "compact_only", "t_decomposition_compact_matrix",
                           "t_decomposition_compact_map")

    # Errors and warnings #
    # .error_messages    [inherited]
    # .warning_messages  [inherited]

  ), # end private

  # Active binding accessors for private attributes (above) #
  active = list(

    # attribute_aliases [inherited]

    #' @field coordinates Data frame (or matrix) of X-Y population coordinates (WGS84) in longitude (degrees West) and latitude (degrees North).
    coordinates = function(value) {
      if (missing(value)) {
        private$.coordinates
      } else {
        if (!is.null(self$correlation_matrix) || !is.null(self$t_decomposition_matrix) ||
            !is.null(self$t_decomposition_compact_matrix) || !is.null(self$t_decomposition_compact_map)) {
          stop("Calculated correlations/decompositions are already associated with the existing coordinates", call. = FALSE)
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
      }
    },

    #' @field correlation_amplitude Correlation function: \emph{a*exp(-distance/b)} \emph{a} parameter. Represents the amplitude or maximum magnitude of correlation values between model cells.
    correlation_amplitude = function(value) {
      if (missing(value)) {
        private$.correlation_amplitude
      } else {
        if (!is.null(self$correlation_matrix) || !is.null(self$t_decomposition_matrix) ||
            !is.null(self$t_decomposition_compact_matrix) || !is.null(self$t_decomposition_compact_map)) {
          stop("Calculated correlations/decompositions are already associated with the existing correlation parameters", call. = FALSE)
        } else if (!is.null(value) && is.numeric(value) && (value > 1 || value < 0)) {
          stop("Correlation function parameter amplitude must be between 0 and 1 inclusively", call. = FALSE)
        } else if (!is.null(value) && !is.numeric(value)) {
          stop("Correlation function parameter amplitude must be numeric", call. = FALSE)
        } else {
          private$.correlation_amplitude <- value
        }
      }
    },

    #' @field correlation_breadth Correlation function: \emph{a*exp(-distance/b)} \emph{b} parameter. Represents the breadth of the correlation between model cells. Typically estimated via average distance between correlated population cells.
    correlation_breadth = function(value) {
      if (missing(value)) {
        private$.correlation_breadth
      } else {
        if (!is.null(self$correlation_matrix) || !is.null(self$t_decomposition_matrix) ||
            !is.null(self$t_decomposition_compact_matrix) || !is.null(self$t_decomposition_compact_map)) {
          stop("Calculated correlations/decompositions are already associated with the existing correlation parameters", call. = FALSE)
        } else if (!is.null(value) && is.numeric(value) && value <= 0) {
          stop("Correlation function parameter breadth must be positive/non-zero", call. = FALSE)
        } else if (!is.null(value) && !is.numeric(value)) {
          stop("Correlation function parameter breadth must be numeric", call. = FALSE)
        } else {
          private$.correlation_breadth <- value
        }
      }
    },

    #' @field correlation_matrix Correlation matrix calculated via correlation function: \emph{a*exp(-distance/b)}.
    correlation_matrix = function(value) {
      if (missing(value)) {
        private$.correlation_matrix
      } else {
        private$.correlation_matrix <- value
      }
    },

    #' @field t_decomposition_matrix The transposed Cholesky decomposition of the correlation matrix.
    t_decomposition_matrix = function(value) {
      if (missing(value)) {
        private$.t_decomposition_matrix
      } else {
        private$.t_decomposition_matrix <- value
      }
    },

    #' @field compact_only Boolean to indicate that only the compact versions of matrices will be maintained once calculated.
    compact_only = function(value) {
      if (missing(value)) {
        private$.compact_only
      } else {
        private$.compact_only <- value
      }
    },

    #' @field t_decomposition_compact_matrix A compact (rows) version of the transposed Cholesky decomposition of the correlation matrix.
    t_decomposition_compact_matrix = function(value) {
      if (missing(value)) {
        private$.t_decomposition_compact_matrix
      } else {
        private$.t_decomposition_compact_matrix <- value
      }
    },

    #' @field t_decomposition_compact_map A map of the original row populations for the compact transposed decomposition matrix.
    t_decomposition_compact_map = function(value) {
      if (missing(value)) {
        private$.t_decomposition_compact_map
      } else {
        private$.t_decomposition_compact_map <- value
      }
    }

  ) # end active
)
