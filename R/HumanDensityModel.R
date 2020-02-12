#' R6 Class Representing a Human Density Model.
#'
#' @description
#' R6 class functionality for modelling human densities within a spatially-explicit
#' population model. The model generates samples of human densities as a fraction of
#' maximum capacity via a triangular distribution based on configured human density
#' means and standard deviations for each simulation cell, as well as optional limits.
#' It dynamically generates attributes defined as \emph{outputs} (default:
#' \emph{human_densities}) given sampled \emph{inputs} (default:
#' \emph{human_density_sample}).
#'
#' @importFrom R6 R6Class
#' @include GenerativeModel.R
#' @include HumanDensityTemplate.R
#' @export HumanDensityModel

HumanDensityModel <- R6Class("HumanDensityModel",
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

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method sets the generative template and requirements as well as any attributes passed via a \emph{params} list or individually.
    #' @param generative_template Optional nested object for generative human density model attributes that need to be maintained when a new clone object is generated for a sample simulation.
    #' @param generative_requirements Optional list of attribute names and the template setting (\emph{"file"} or \emph{"function"}) that is required to generate their values (otherwise default functionality is used).
    #' @param attribute_aliases Optional list of extra alias names for model attributes (form: alias = "attribute") to be used with the set and get attributes methods.
    #' @param ... Parameters passed via a \emph{params} list or individually.
    initialize = function(generative_template = NULL, generative_requirements = NULL, attribute_aliases = NULL, ...) {
      if (is.null(generative_template)) { # when new object
        self$generative_template <- HumanDensityTemplate$new()
        attribute_aliases <- c(attribute_aliases, # Append default aliases
                               list(hd_sample = "human_density_sample", hd_p = "human_density_sample"))
        if (!("description" %in% names(list(...)))) {
          self$description <- "Human density"
        }
        if (!("inputs" %in% names(list(...)))) {
          self$inputs <- c("human_density_sample")
        }
        if (!("outputs" %in% names(list(...)))) {
          self$outputs <- c("human_densities")
        }
      }
      if (is.null(generative_requirements)) {
        generative_requirements <- list(human_densities = "default")
      }
      super$initialize(generative_template = generative_template,
                       generative_requirements = generative_requirements,
                       attribute_aliases = attribute_aliases, ...)
    },

    #' @description
    #' Returns a boolean to indicate that all the default, file and/or function template settings that are required for attribute generation are present.
    #' @return Boolean to indicate that the required settings for attribute generation are present.
    generative_requirements_satisfied = function() {
      satisfied <- super$generative_requirements_satisfied()
      if (is.list(satisfied) && "human_densities" %in% names(satisfied) &&
          self$generative_requirements$human_densities == "default") {
        satisfied$human_densities <- (!is.null(self$carrying_capacity_mean) &&
                                        !is.null(self$carrying_capacity_sd) &&
                                        (!self$uses_correlations || !is.null(self$correlation_model)))
        # Add any attributes that are missing (for error message)
        if (is.null(self$carrying_capacity_mean)) {
          satisfied$carrying_capacity_mean <- FALSE
        }
        if (is.null(self$carrying_capacity_sd)) {
          satisfied$carrying_capacity_sd <- FALSE
        }
        if (self$uses_correlations && is.null(self$correlation_model)) {
          satisfied$correlation_model <- FALSE
        }
      }
      return(satisfied)
    },

    # New methods #

    #' @description
    #' Calculates the minimum, mean and maximum human density values for each simulation grid and time step based on the input mean and standard deviation data.
    #' @param ... Parameters passed via a \emph{params} list or individually.
    calculate_sampling_parameters = function(...) {

      # Set attributes
      if (length(list(...))) {
        self$set_attributes(...)
      }

      # Ensure the carrying capacity mean and standard deviation data are set
      if (is.null(self$carrying_capacity_mean) || is.null(self$carrying_capacity_sd)) {
        stop("Sampling parameter calculations require the human carrying capacity mean and standard deviation time-series data to be set first", call. = FALSE)
      }

      # Apply human occupancy mask to carrying capacity mean and standard deviation
      if (!is.null(self$human_occupancy_mask)) {
        self$carrying_capacity_mean <- self$carrying_capacity_mean*self$human_occupancy_mask
        self$carrying_capacity_sd <- self$carrying_capacity_sd*self$human_occupancy_mask
      }

      # Calculate minimum, mean and maximum for sampling distribution
      distrib_min <- self$carrying_capacity_mean - self$sd_number*self$carrying_capacity_sd
      distrib_min[which(distrib_min < 0)] <- 0
      distrib_mean <- self$carrying_capacity_mean
      distrib_max <- self$carrying_capacity_mean + self$sd_number*self$carrying_capacity_sd

      # Apply lower threshold to distributions to avoid unnecessary sampling calculations
      if (!is.null(self$lower_threshold)) {
        indices_below <- which(distrib_max > 0 & distrib_max < self$lower_threshold)
        distrib_min[indices_below] <- 0
        distrib_mean[indices_below] <- 0
        distrib_max[indices_below] <- 0
      }

      # Apply upper bounds
      if (!is.null(self$mean_upper)) {
        distrib_mean[which(distrib_mean > self$mean_upper)] <- self$mean_upper
      }
      if (!is.null(self$max_upper)) {
        distrib_max[which(distrib_max > self$max_upper)] <- self$max_upper
      } else {
        self$max_upper <- max(distrib_max)
      }

      # Build distribution data frame of non-zero min-mean-max combinations:
      distrib_data <- as.data.frame(cbind(1:(nrow(distrib_mean)*ncol(distrib_mean)),
                                          which(array(TRUE, c(dim(distrib_mean))), arr.ind = TRUE)[,1]))
      names(distrib_data) <- c("index", "pop")

      # Scale by maximum abundance
      distrib_data$min <- as.vector(distrib_min/self$max_upper)
      distrib_data$mean <- as.vector(distrib_mean/self$max_upper)
      distrib_data$max <- as.vector(distrib_max/self$max_upper)

      # Eliminate zeros
      self$distrib_data <- distrib_data[-which(distrib_data$max == 0),]

      # Separate into flat and triangular
      self$distrib_data <- list(flat = distrib_data[which(distrib_data$min == distrib_data$max),],
                                triangular = distrib_data[which(distrib_data$min < distrib_data$max),])
    },

    #' @description
    #' Calculates the human density values via the (triangular) sampling distribution for each simulation grid and time step utilizing correlated deviates for each grid cell.
    #' @return Returns character string message when calculation prerequisites are not met (for simulation logging).
    calculate_human_densities = function() {

      # Ensure the sampling distribution parameters are set
      if (is.null(self$distrib_data)) {
        messages <- self$calculate_sampling_parameters()
        if (is.character(messages)) {
          return(messages)
        }
      }

      # Ensure the correlation model is set when required
      if (self$uses_correlations && is.null(self$correlation_model)) {
        return("The correlation model for generating sample deviates needs to be set first")
      }

      # Ensure single number sample is a number between 0 and 1 when no correlations are used
      if (!self$uses_correlations && !is.null(self$human_density_sample) &&
          (!is.numeric(self$human_density_sample) || self$human_density_sample < 0 || self$human_density_sample > 1)) {
        return("The human density sample parameter must be a number between 0 and 1 (inclusive)")
      }

      # Generate human density sample via sampling distribution
      if (!is.null(self$distrib_data) && (self$uses_correlations || (!self$uses_correlations && !is.null(self$human_density_sample)))) {

        # Generate correlated random deviates
        if (self$uses_correlations) {
          # Use correlation model to generate correlated normal deviates using the sample random seed when present
          if (self$temporal_correlation < 1) {
            correlated_deviates <- self$correlation_model$generate_correlated_normal_deviates(random_seed = self$human_density_seed,
                                                                                              temporal_correlation = self$temporal_correlation,
                                                                                              time_steps = ncol(self$carrying_capacity_mean))
          } else {
            correlated_deviates <- self$correlation_model$generate_correlated_normal_deviates(random_seed = self$human_density_seed)
          }
          if (is.character(correlated_deviates)) { # return with error message
            return(correlated_deviates)
          }
          # Convert normal correlated deviates to correlated uniform samples between lower and upper values if present
          if (!is.null(self$human_density_lower) && !is.null(self$human_density_upper)) {
            correlated_samples <- self$human_density_lower + pnorm(correlated_deviates)*(self$human_density_upper - self$human_density_lower)
          } else { # use 0-1
            correlated_samples <- pnorm(correlated_deviates)
          }
        }

        # Initialize human density value matrix
        human_densities <- array(0, dim(self$carrying_capacity_mean))

        # Handle any flat triangles (min = max), which result in NA values from the triangular distribution
        distrib_data <- self$distrib_data$flat
        if (nrow(distrib_data)) {
          human_densities[distrib_data$index] <- distrib_data$mean
        }

        # Apply the triangular distribution for the remaining data
        distrib_data <- self$distrib_data$triangular

        # Match (correlated) samples with distribution data
        if (self$uses_correlations) {
          if (self$temporal_correlation < 1) { # samples in a [populations x duration] matrix
            correlated_samples <- correlated_samples[distrib_data$index]
          } else { # samples in a vector of length = populations
            correlated_samples <- correlated_samples[distrib_data$pop]
          }
        } else { # use single number sample
          correlated_samples <- self$human_density_sample
        }
        
        # Sample the human densities from the triangular distribution
        human_densities[distrib_data$index] <- metRology::qtri(correlated_samples,
                                                               min = distrib_data$min,
                                                               max = distrib_data$max,
                                                               mode = distrib_data$mean)

        # Apply lower threshold
        if (!is.null(self$lower_threshold)) {
          human_densities[which(human_densities < (self$lower_threshold/self$max_upper))] <- 0
        }
        if (!is.null(self$decimals)) {
          nonzero_indices <- which(human_densities > 0)
          human_densities[nonzero_indices] <- round(human_densities[nonzero_indices], self$decimals)
        }

        self$human_densities <- human_densities

      } else {
        return("Human density calculation requires sampling distribution parameters to be set first")
      }
    }

  ), # end public

  private = list(

    ## Attributes ##

    # .attribute_aliases       [inherited]

    # Generative attributes #
    # .generative_template     [inherited]
    # .generative_requirements [inherited]

    # Model attributes #
    .model_attributes = c("carrying_capacity_mean", "carrying_capacity_sd", "human_occupancy_mask",
                          "lower_threshold", "mean_upper", "max_upper", "sd_number", "distrib_data",
                          "uses_correlations", "correlation_model", "temporal_correlation", "decimals",
                          "human_density_sample", "human_density_lower", "human_density_upper",
                          "human_density_seed", "human_densities"),
    .human_density_sample = NULL,
    .human_density_lower = NULL,
    .human_density_upper = NULL,
    .human_density_seed = NULL,
    .human_densities = NULL,

    # Attributes accessible via model get/set methods #
    .active_attributes = c("description", "inputs", "outputs", "file_templates", "function_templates",
                           "carrying_capacity_mean", "carrying_capacity_sd", "human_occupancy_mask",
                           "lower_threshold", "mean_upper", "max_upper", "sd_number", "distrib_data",
                           "uses_correlations", "correlation_model", "temporal_correlation", "decimals",
                           "human_density_sample", "human_density_lower", "human_density_upper",
                           "human_density_seed", "human_densities")

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

    # Model attribute accessors #

    # Nested model attribute accessors #

    # description [inherited]

    # inputs [inherited]

    # outputs [inherited]

    # file_templates [inherited]

    # function_templates [inherited]

    #' @field carrying_capacity_mean Matrix (or data frame) of human carrying capacity time-series data means (simulation cells by duration).
    carrying_capacity_mean = function(value) {
      if (missing(value)) {
        self$generative_template$carrying_capacity_mean
      } else {
        if (is.character(value) && file.exists(value)) {
          if (length(grep(".CSV", toupper(value), fixed = TRUE))) {
            self$generative_template$carrying_capacity_mean <- read.csv(file = value)
          } else if (length(grep(".RDS", toupper(value), fixed = TRUE))) {
            self$generative_template$carrying_capacity_mean <- readRDS(file = value)
          } else {
            self$generative_template$carrying_capacity_mean <- read.table(file = value)
          }
          self$generative_template$carrying_capacity_mean <- as.matrix(self$generative_template$carrying_capacity_mean)
        } else {
          self$generative_template$carrying_capacity_mean <- as.matrix(value)
        }
        self$generative_template$carrying_capacity_mean[is.na(self$generative_template$carrying_capacity_mean)] <- 0
      }
    },

    #' @field carrying_capacity_sd Matrix (or data frame) of human carrying capacity time-series data standard deviations (simulation cells by duration).
    carrying_capacity_sd = function(value) {
      if (missing(value)) {
        self$generative_template$carrying_capacity_sd
      } else {
        if (is.character(value) && file.exists(value)) {
          if (length(grep(".CSV", toupper(value), fixed = TRUE))) {
            self$generative_template$carrying_capacity_sd <- read.csv(file = value)
          } else if (length(grep(".RDS", toupper(value), fixed = TRUE))) {
            self$generative_template$carrying_capacity_sd <- readRDS(file = value)
          } else {
            self$generative_template$carrying_capacity_sd <- read.table(file = value)
          }
          self$generative_template$carrying_capacity_sd <- as.matrix(self$generative_template$carrying_capacity_sd)
        } else {
          self$generative_template$carrying_capacity_sd <- as.matrix(value)
        }
        self$generative_template$carrying_capacity_sd[is.na(self$generative_template$carrying_capacity_sd)] <- 0
      }
    },

    #' @field human_occupancy_mask Optional binary mask (matrix or data frame) for human carrying capacity time-series data (simulation cells by duration).
    human_occupancy_mask = function(value) {
      if (missing(value)) {
        self$generative_template$human_occupancy_mask
      } else {
        if (is.character(value) && file.exists(value)) {
          if (length(grep(".CSV", toupper(value), fixed = TRUE))) {
            self$generative_template$human_occupancy_mask <- read.csv(file = value)
          } else if (length(grep(".RDS", toupper(value), fixed = TRUE))) {
            self$generative_template$human_occupancy_mask <- readRDS(file = value)
          } else {
            self$generative_template$human_occupancy_mask <- read.table(file = value)
          }
          self$generative_template$human_occupancy_mask <- as.matrix(self$generative_template$human_occupancy_mask)
        } else {
          if (is.numeric(value)) {
            self$generative_template$human_occupancy_mask <- as.matrix(value)
          } else {
            self$generative_template$human_occupancy_mask <- value
          }
        }
        self$generative_template$human_occupancy_mask[is.na(self$generative_template$human_occupancy_mask)] <- 0
      }
    },

    #' @field lower_threshold Optional lower threshold for human occupancy (lower values are set to zero).
    lower_threshold = function(value) {
      if (missing(value)) {
        self$generative_template$lower_threshold
      } else {
        self$generative_template$lower_threshold <- value
      }
    },

    #' @field mean_upper Optional upper limit for sample distribution mean carrying capacities.
    mean_upper = function(value) {
      if (missing(value)) {
        self$generative_template$mean_upper
      } else {
        self$generative_template$mean_upper <- value
      }
    },

    #' @field max_upper Optional upper limit for sample distribution maximum carrying capacities.
    max_upper = function(value) {
      if (missing(value)) {
        self$generative_template$max_upper
      } else {
        self$generative_template$max_upper <- value
      }
    },

    #' @field sd_number Number of standard deviations from the sample distribution mean to the lower and upper limits (default = 1).
    sd_number = function(value) {
      if (missing(value)) {
        self$generative_template$sd_number
      } else {
        self$generative_template$sd_number <- value
      }
    },

    #' @field distrib_data Calculated sample distribution minimum, maximum and mean carrying capacity time-series data.
    distrib_data = function(value) {
      if (missing(value)) {
        self$generative_template$distrib_data
      } else {
        self$generative_template$distrib_data <- value
      }
    },

    #' @field uses_correlations A boolean to indicate that a correlation model is used for generating correlated random deviates.
    uses_correlations = function(value) {
      if (missing(value)) {
        self$generative_template$uses_correlations
      } else {
        self$generative_template$uses_correlations <- value
      }
    },
    
    #' @field correlation_model A correlation model for generating correlated random deviates (type of \emph{CorrelationModel} or an inherited class).
    correlation_model = function(value) {
      if (missing(value)) {
        self$generative_template$correlation_model
      } else {
        if (!is.null(value) && !("CorrelationModel" %in% class(value))) {
          stop("correlation model must be a CorrelationModel or inherited class object", call. = FALSE)
        } else {
          if (!is.null(value)) {
            self$uses_correlations <- TRUE
          } else {
            self$uses_correlations <- FALSE
          }
          self$generative_template$correlation_model <- value
        }
      }
    },
    
    #' @field temporal_correlation Absolute correlation coefficient between simulation time steps for all grid cells (0-1; default = 1).
    temporal_correlation = function(value) {
      if (missing(value)) {
        self$generative_template$temporal_correlation
      } else {
        self$generative_template$temporal_correlation <- value
      }
    },
    
    #' @field decimals Number of decimal places applied to the calculated human densities (default: NULL = no rounding).
    decimals = function(value) {
      if (missing(value)) {
        self$generative_template$decimals
      } else {
        self$generative_template$decimals <- value
      }
    },

    # Local (non-nested) model attribute accessors #

    #' @field distrib_mean_matrix Calculated sample distribution mean carrying capacity time-series matrix.
    distrib_mean_matrix = function(value) {
      if (missing(value)) {
        if (!is.null(self$distrib_data) && !is.null(self$carrying_capacity_mean)) {
          distrib_mean_matrix <- array(0, dim(self$carrying_capacity_mean))
          distrib_mean_matrix[self$distrib_data$index] <- self$distrib_data$mean
          distrib_mean_matrix
        } else {
          NULL
        }
      } else {
        stop("The sample distribution mean carrying capacity time-series matrix is read only and is calculated internally from the distribution data", call. = FALSE)
      }
    },

    #' @field human_density_sample Human density sample number (or file index) or probability (for default generation).
    human_density_sample = function(value) {
      if (missing(value)) {
        private$.human_density_sample
      } else {
        private$.human_density_sample <- value
      }
    },

    #' @field human_density_lower Human density lower probability for correlated generation.
    human_density_lower = function(value) {
      if (missing(value)) {
        private$.human_density_lower
      } else {
        private$.human_density_lower <- value
      }
    },
    
    #' @field human_density_upper Human density upper probability for correlated generation.
    human_density_upper = function(value) {
      if (missing(value)) {
        private$.human_density_upper
      } else {
        private$.human_density_upper <- value
      }
    },
    
    #' @field human_density_seed Human density random seed for repeatable correlated generation.
    human_density_seed = function(value) {
      if (missing(value)) {
        private$.human_density_seed
      } else {
        private$.human_density_seed <- value
      }
    },
    
    #' @field human_densities Calculated sample human density values (simulation cells by duration).
    human_densities = function(value) {
      if (missing(value)) {
        if (is.null(private$.human_densities) && "human_densities" %in% names(self$generative_requirements)) {
          template_type <- self$generative_requirements[["human_densities"]]
          if (template_type == "file") {
            private$.human_densities <- self$read_file("human_densities")
            private$.human_densities[which(is.na(private$.human_densities))] <- 0
          } else if (template_type == "function") {
            private$.human_densities <- self$run_function("human_densities")
          } else if (template_type == "default") { # use internal function
            message <- self$calculate_human_densities()
            if (is.character(message)) {
              self$warning_messages <- unique(append(self$warning_messages, message))
            }
          }
        }
        if (!is.null(self$human_occupancy_mask) && !is.null(private$.human_densities) &&
            all(dim(self$human_occupancy_mask) == dim(private$.human_densities))) {
          private$.human_densities*self$human_occupancy_mask
        } else {
          private$.human_densities
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
