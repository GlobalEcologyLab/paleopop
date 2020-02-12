#' An R6 Class to represent a Human Density Model.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @include GenerativeModel.R
#' @include HumanDensityTemplate.R
#' @field model_attributes A list of public model attributes (name-value pairs) - another stored as private.
#' @field attached A list of attached attributes (name-value pairs).
#' @field error_messages A vector of error messages encountered when setting model attributes.
#' @field warning_messages A vector of warning messages encountered when setting model attributes.
#' @field file_templates A nested list of generative file template attributes.
#' @field function_templates A nested list of generative function template attributes.
#' @field generative_templates A nested object for the generative file and function templates for shallow cloning.
#' @field generative_requirements A list of attribute names and the template setting ("default", "file", or "function") that is required to generate their values.
#' @field carrying_capacity_mean Matrix (or data frame) of human carrying capacity time-series data means (simulation cells by duration).
#' @field carrying_capacity_sd Matrix (or data frame) of human carrying capacity time-series data standard deviations (simulation cells by duration).
#' @field human_occupancy_mask Optional binary mask (matrix or data frame) for human carrying capacity time-series data (simulation cells by duration).
#' @field lower_threshold Optional lower threshold for human occupancy (lower values are set to zero).
#' @field mean_upper Optional upper limit for sample distribution mean carrying capacities.
#' @field max_upper Optional upper limit for sample distribution maximum carrying capacities.
#' @field sd_number Number of standard deviations from the sample distribution mean to the lower and upper limits (default = 1).
#' @field distrib_data Calculated sample distribution minimum, maximum and mean carrying capacity time-series data.
#' @field uses_correlations A boolean to indicate that a correlation model is used for generating correlated random deviates.
#' @field correlation_model A correlation model for generating correlated random deviates for each grid cell (type: CorrelationModel or inherited class).
#' @field temporal_correlation Absolute correlation coefficient between simulation time steps for all grid cells (0-1; default = 1).
#' @field decimals Number of decimal places applied to the calculated human densities (default: NULL = no rounding).
#' @field distrib_mean_matrix Calculated sample distribution mean carrying capacity time-series matrix.
#' @field human_density_sample Human density sample number (or file index) or probability for non-correlated generation.
#' @field human_density_lower Human density lower probability for correlated generation.
#' @field human_density_upper Human density upper probability for correlated generation.
#' @field human_density_seed Human density random seed for repeatable correlated generation.
#' @field human_density_matrix Sampled human density values (simulation cells by duration).
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(params = list(), ...)}}{Initialization method sets given attributes individually and/or from a list.}
#'   \item{\code{new_clone(...)}}{Method returns a new object of the current object class with appropriately passed parameters.}
#'   \item{\code{get_attribute_names()}}{Inherited method returns a list of all attribute names including public and private model attributes, as well as attached attributes.}
#'   \item{\code{get_attributes(params)}}{Inherited method returns a list of all attributes (when no params) or selected parameters (when array of parameter names provided).}
#'   \item{\code{set_attributes(params = list(), ...)}}{Inherited method sets given attributes individually and/or from a list.}
#'   \item{\code{read_model(path)}}{Inherited method reads a model from a RDS file via a path to a file, or a directory (using class name).}
#'   \item{\code{save_model(path)}}{Inherited method saves a model to a RDS file via a path to a file, or a directory (using class name).}
#'   \item{\code{add_file_template(param, path_template, path_params = c(), file_type = "RDS")}}{Inherited method adds a file template for reading RDS/CSV files for a given parameter.}
#'   \item{\code{add_function_template(param, source_path, call_template, call_params = c())}}{Inherited method adds a function template for running a function to calculate a given parameter.}
#'   \item{\code{read_file(param)}}{Inherited method returns the contents of the file based on the file template for the specified parameter.}
#'   \item{\code{run_function(param)}}{Inherited method returns the calculated result of the function based on the function template for the specified parameter.}
#'   \item{\code{add_generative_requirements(params = list(), ...)}}{Inherited method adds attribute names and the template setting ("file" or "function") that is required to generate their values, passed as a list or individually.}
#'   \item{\code{generative_requirements_satisfied()}}{Method returns a boolean to indicate that all the file and/or template settings that are required for attribute generation are present.}
#'   \item{\code{calculate_sampling_parameters(lower_threshold = NULL, mean_upper = NULL, max_upper = NULL, sd_number = NULL)}}{Method calculates the minimum, mean and maximum human density values for each simulation grid and time step based on the input mean and standard deviation data.}
#'   \item{\code{calculate_human_density()}}{Method calculates the human density values via the (triangular) sampling distribution for each simulation grid and time step utilizing correlated deviates for each grid cell.}
#' }
#' @export HumanDensityModel

HumanDensityModel <- R6Class("HumanDensityModel",
  inherit = GenerativeModel,
  public = list(

    ## Attributes

    # Model attributes
    model_attributes = c(), # private only (see below)

    ## Methods

    initialize = function(human_density_template = NULL, generative_requirements = NULL, ...) {
      if (is.null(human_density_template)) { # nested object for cloning
        self$human_density_template <- HumanDensityTemplate$new()
      } else {
        self$human_density_template <- human_density_template
      }
      if (is.null(generative_requirements)) {
        generative_requirements <- list(human_density_matrix = "default")
      }
      super$initialize(generative_requirements = generative_requirements, ...)
    },

    new_clone = function(...) {
      return(super$new_clone(human_density_template = self$human_density_template, ...))
    },

    generative_requirements_satisfied = function() {
      satisfied <- super$generative_requirements_satisfied()
      if (is.list(satisfied) && "human_density_matrix" %in% names(satisfied) &&
          self$generative_requirements$human_density_matrix == "default") {
        satisfied$human_density_matrix <- (!is.null(self$carrying_capacity_mean) &&
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

    calculate_sampling_parameters = function(lower_threshold = NULL, mean_upper = NULL, max_upper = NULL, sd_number = NULL) {
      # Set function parameters
      if (!is.null(lower_threshold)) {
        self$lower_threshold <- lower_threshold
      }
      if (!is.null(mean_upper)) {
        self$mean_upper <- mean_upper
      }
      if (!is.null(max_upper)) {
        self$max_upper <- max_upper
      }
      if (!is.null(sd_number)) {
        self$sd_number <- sd_number
      }
      # Ensure the carrying capacity mean and standard deviation data are set
      if (!is.null(self$carrying_capacity_mean) && !is.null(self$carrying_capacity_sd)) {
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
      } else {
        warning("Sampling parameter calculations require the human carrying capacity mean and standard deviation time-series data to be set first", call. = FALSE)
      }
    },

    calculate_human_density = function() {
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
        # Sample from the triangular distribution for each human density value
        human_density_matrix <- array(0, dim(self$carrying_capacity_mean))
        # Handle any flat triangles (min = max), which result in NA values from the triangular distribution
        flat_indices <- which(self$distrib_data$min == self$distrib_data$max)
        if (length(flat_indices)) {
          flat_distrib_data <- self$distrib_data[flat_indices,]
          human_density_matrix[flat_distrib_data$index] <- flat_distrib_data$mean
          distrib_data <- self$distrib_data[-flat_indices,]
        } else {
          distrib_data <- self$distrib_data
        }
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
        # Apply the triangular distribution for the remaining data
        human_density_matrix[distrib_data$index] <- metRology::qtri(correlated_samples,
                                                                    min = distrib_data$min,
                                                                    max = distrib_data$max,
                                                                    mode = distrib_data$mean)
        # Apply lower threshold
        if (!is.null(self$lower_threshold)) {
          human_density_matrix[which(human_density_matrix < (self$lower_threshold/self$max_upper))] <- 0
        }
        if (!is.null(self$decimals)) {
          human_density_matrix <- round(human_density_matrix, self$decimals)
        }
        self$human_density_matrix <- human_density_matrix
      } else {
        return("Human density calculation requires sampling distribution parameters to be set first")
      }
    }

  ), # end public

  private = list(

    ## Attributes

    # Generative attributes
    .human_density_template = NULL,

    # Model attributes
    .model_attributes = c("carrying_capacity_mean", "carrying_capacity_sd", "human_occupancy_mask",
                          "lower_threshold", "mean_upper", "max_upper", "sd_number", "distrib_data",
                          "uses_correlations", "correlation_model", "temporal_correlation", "decimals",
                          "human_density_sample", "human_density_lower", "human_density_upper",
                          "human_density_seed", "human_density_matrix"),
    .active_attributes = c("carrying_capacity_mean", "carrying_capacity_sd", "human_occupancy_mask",
                           "lower_threshold", "mean_upper", "max_upper", "sd_number", "distrib_data",
                           "uses_correlations", "correlation_model", "temporal_correlation", "decimals",
                           "human_density_sample", "human_density_lower", "human_density_upper",
                           "human_density_seed", "human_density_matrix"),
    .carrying_capacity_mean = NULL,
    .carrying_capacity_sd = NULL,
    .human_occupancy_mask = NULL,
    .lower_threshold = NULL,
    .mean_upper = NULL,
    .max_upper = NULL,
    .sd_number = NULL,
    .distrib_data = NULL,
    .uses_correlations = TRUE,
    .correlation_model = NULL,
    .temporal_correlation = NULL,
    .decimals = NULL,
    .human_density_sample = NULL,
    .human_density_lower = NULL,
    .human_density_upper = NULL,
    .human_density_seed = NULL,
    .human_density_matrix = NULL

  ), # end private

  # Active binding accessors for private attributes (above)
  active = list(

    human_density_template = function(value) {
      if (missing(value)) {
        private$.human_density_template
      } else {
        private$.human_density_template <- value
      }
    },

    carrying_capacity_mean = function(value) {
      if (missing(value)) {
        private$.human_density_template$carrying_capacity_mean
      } else {
        if (is.character(value) && file.exists(value)) {
          if (length(grep(".CSV", toupper(value), fixed = TRUE))) {
            private$.human_density_template$carrying_capacity_mean <- read.csv(file = value)
          } else if (length(grep(".RDS", toupper(value), fixed = TRUE))) {
            private$.human_density_template$carrying_capacity_mean <- readRDS(file = value)
          } else {
            private$.human_density_template$carrying_capacity_mean <- read.table(file = value)
          }
          private$.human_density_template$carrying_capacity_mean <- as.matrix(private$.human_density_template$carrying_capacity_mean)
        } else {
          private$.human_density_template$carrying_capacity_mean <- as.matrix(value)
        }
        private$.human_density_template$carrying_capacity_mean[is.na(private$.human_density_template$carrying_capacity_mean)] <- 0
      }
    },

    carrying_capacity_sd = function(value) {
      if (missing(value)) {
        private$.human_density_template$carrying_capacity_sd
      } else {
        if (is.character(value) && file.exists(value)) {
          if (length(grep(".CSV", toupper(value), fixed = TRUE))) {
            private$.human_density_template$carrying_capacity_sd <- read.csv(file = value)
          } else if (length(grep(".RDS", toupper(value), fixed = TRUE))) {
            private$.human_density_template$carrying_capacity_sd <- readRDS(file = value)
          } else {
            private$.human_density_template$carrying_capacity_sd <- read.table(file = value)
          }
          private$.human_density_template$carrying_capacity_sd <- as.matrix(private$.human_density_template$carrying_capacity_sd)
        } else {
          private$.human_density_template$carrying_capacity_sd <- as.matrix(value)
        }
        private$.human_density_template$carrying_capacity_sd[is.na(private$.human_density_template$carrying_capacity_sd)] <- 0
      }
    },

    human_occupancy_mask = function(value) {
      if (missing(value)) {
        private$.human_density_template$human_occupancy_mask
      } else {
        if (is.character(value) && file.exists(value)) {
          if (length(grep(".CSV", toupper(value), fixed = TRUE))) {
            private$.human_density_template$human_occupancy_mask <- read.csv(file = value)
          } else if (length(grep(".RDS", toupper(value), fixed = TRUE))) {
            private$.human_density_template$human_occupancy_mask <- readRDS(file = value)
          } else {
            private$.human_density_template$human_occupancy_mask <- read.table(file = value)
          }
          private$.human_density_template$human_occupancy_mask <- as.matrix(private$.human_density_template$human_occupancy_mask)
        } else {
          if (is.numeric(value)) {
            private$.human_density_template$human_occupancy_mask <- as.matrix(value)
          } else {
            private$.human_density_template$human_occupancy_mask <- value
          }
        }
        private$.human_density_template$human_occupancy_mask[is.na(private$.human_density_template$human_occupancy_mask)] <- 0
      }
    },

    lower_threshold = function(value) {
      if (missing(value)) {
        private$.human_density_template$lower_threshold
      } else {
        private$.human_density_template$lower_threshold <- value
      }
    },

    mean_upper = function(value) {
      if (missing(value)) {
        private$.human_density_template$mean_upper
      } else {
        private$.human_density_template$mean_upper <- value
      }
    },

    max_upper = function(value) {
      if (missing(value)) {
        private$.human_density_template$max_upper
      } else {
        private$.human_density_template$max_upper <- value
      }
    },

    sd_number = function(value) {
      if (missing(value)) {
        private$.human_density_template$sd_number
      } else {
        private$.human_density_template$sd_number <- value
      }
    },

    distrib_data = function(value) {
      if (missing(value)) {
        private$.human_density_template$distrib_data
      } else {
        private$.human_density_template$distrib_data <- value
      }
    },

    uses_correlations = function(value) {
      if (missing(value)) {
        private$.human_density_template$uses_correlations
      } else {
        private$.human_density_template$uses_correlations <- value
      }
    },

    correlation_model = function(value) {
      if (missing(value)) {
        private$.human_density_template$correlation_model
      } else {
        if (!is.null(value) && !("CorrelationModel" %in% class(value))) {
          warning("correlation model must be a CorrelationModel or inherited class object", call. = FALSE)
        } else {
          private$.human_density_template$correlation_model <- value
        }
      }
    },

    temporal_correlation = function(value) {
      if (missing(value)) {
        private$.human_density_template$temporal_correlation
      } else {
        private$.human_density_template$temporal_correlation <- value
      }
    },

    decimals = function(value) {
      if (missing(value)) {
        private$.human_density_template$decimals
      } else {
        private$.human_density_template$decimals <- value
      }
    },

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
        stop("Read only - calculated dynamically from distrib_data", call. = FALSE)
      }
    },

    human_density_sample = function(value) {
      if (missing(value)) {
        private$.human_density_sample
      } else {
        private$.human_density_sample <- value
      }
    },

    human_density_lower = function(value) {
      if (missing(value)) {
        private$.human_density_lower
      } else {
        private$.human_density_lower <- value
      }
    },

    human_density_upper = function(value) {
      if (missing(value)) {
        private$.human_density_upper
      } else {
        private$.human_density_upper <- value
      }
    },

    human_density_seed = function(value) {
      if (missing(value)) {
        private$.human_density_seed
      } else {
        private$.human_density_seed <- value
      }
    },

    human_density_matrix = function(value) {
      if (missing(value)) {
        if (is.null(private$.human_density_matrix) && "human_density_matrix" %in% names(self$generative_requirements)) {
          template_type <- self$generative_requirements[["human_density_matrix"]]
          if (template_type == "file") {
            private$.human_density_matrix <- self$read_file("human_density_matrix")
            private$.human_density_matrix[which(is.na(private$.human_density_matrix))] <- 0
          } else if (template_type == "function") {
            private$.human_density <- self$run_function("human_density_matrix")
          } else if (template_type == "default") { # use internal function
            self$calculate_human_density()
          }
        }
        if (!is.null(self$human_occupancy_mask) && !is.null(private$.human_density_matrix) &&
            all(dim(self$human_occupancy_mask) == dim(private$.human_density_matrix))) {
          private$.human_density_matrix*self$human_occupancy_mask
        } else {
          private$.human_density_matrix
        }
      } else {
        private$.human_density_matrix <- value
      }
    }

  ) # end active
)

# # TEST HumanDensityModel
# # Original global human density data
# orig_coords <- "C:\\Users\\Sean\\GoogleDrive\\ARC-Grant\\mammoth\\HDgen\\center.coord.40k.shifted.reduced.csv"
# orig_hd_mean <- "C:\\Users\\Sean\\GoogleDrive\\ARC-Grant\\mammoth\\HDgen\\full.K.mean.for.Damien.RDS"
# orig_hd_sd <- "C:\\Users\\Sean\\GoogleDrive\\ARC-Grant\\mammoth\\HDgen\\full.K.SD.for.Damien.RDS"
# orig_hd_mask <- "C:\\Users\\Sean\\GoogleDrive\\ARC-Grant\\mammoth\\HDgen\\inhabitable.mask.for.Damien.RDS"
# # Mammoth study area human density data
# mamm_coords <- "C:\\Users\\Sean\\GoogleDrive\\ARC-Grant\\mammoth\\Mammoth Model\\data\\studysite_xy_modified3.csv"
# mamm_hd_mean <- region_subset(orig_coords=orig_coords,orig_matrix=orig_hd_mean,subset_coords=mamm_coords)
# dim(mamm_hd_mean); mamm_hd_mean[1:10,1:5]
# mamm_hd_sd <- region_subset(orig_coords=orig_coords,orig_matrix=orig_hd_sd,subset_coords=mamm_coords)
# dim(mamm_hd_sd); mamm_hd_sd[1:10,1:5]
# mamm_hd_mask <- region_subset(orig_coords=orig_coords,orig_matrix=orig_hd_mask,subset_coords=mamm_coords)
# dim(mamm_hd_mask); mamm_hd_mask[1:10,1:5]
# # Add 80-generation time step burn-in to each study area human density data matrix
# mamm_hd_mean <- cbind(array(mamm_hd_mean[,1],c(nrow(mamm_hd_mean),80)),mamm_hd_mean)
# mamm_hd_sd <- cbind(array(mamm_hd_sd[,1],c(nrow(mamm_hd_sd),80)),mamm_hd_sd)
# mamm_hd_mask <- cbind(array(mamm_hd_mask[,1],c(nrow(mamm_hd_mask),80)),mamm_hd_mask)
# # Additional land-sea mask
# land_sea_mask <- read.csv(file="C:\\Users\\Sean\\GoogleDrive\\ARC-Grant\\mammoth\\HDgen\\land_sea_mask.csv",header=T,sep =",")
# land_sea_mask <- as.matrix(cbind(array(land_sea_mask[,4],c(nrow(land_sea_mask),80)),land_sea_mask[,4:844])) # burn-in
# land_sea_mask[is.na(land_sea_mask)] <- 0; dimnames(land_sea_mask) <- NULL
# dim(land_sea_mask); land_sea_mask[1:10,1:10]
# # Build Human Density Model
# hd_model <- HumanDensityModel$new()
# # Check generative requirements
# hd_model$generative_requirements
# hd_model$generative_requirements_satisfied()
# # Attempt to generate sampling parameters without carrying capacity mean & sd
# hd_model$calculate_sampling_parameters(mean_upper = 300, max_upper = 500)
# # Rebuild with carrying capacity mean & sd and occupancy_mask
# hd_model <- HumanDensityModel$new(carrying_capacity_mean=mamm_hd_mean,  # self<-hd_model
#                                   carrying_capacity_sd=mamm_hd_sd,
#                                   human_occupancy_mask=mamm_hd_mask*land_sea_mask);
# # Check generative requirements again
# hd_model$generative_requirements_satisfied()
# # Generate sampling parameters / distribution data
# hd_model$calculate_sampling_parameters(mean_upper = 300, max_upper = 500)
# head(hd_model$distrib_data); dim(hd_model$distrib_data)
# max(hd_model$distrib_data$mean)
# max(hd_model$distrib_data$max)
# ## Generation without correlations
# hd_model$uses_correlations <- FALSE
# # Attempt to calculate human densities
# hd_model$calculate_human_density()
# # Set invalid sample and attempt to calculate human densities
# hd_model$human_density_sample <- "dummy"
# hd_model$calculate_human_density()
# hd_model$human_density_sample <- -0.1
# hd_model$calculate_human_density()
# hd_model$human_density_sample <- 1.1
# hd_model$calculate_human_density()
# # Set valid sample and attempt to calculate human densities
# hd_model$human_density_sample <- 0.5
# hd_model$generative_requirements_satisfied()
# hd_model$calculate_human_density()
# hd_model$human_density_matrix[1:10,1:5]
# # Rounding
# hd_model$decimals <- 4
# hd_model$calculate_human_density()
# hd_model$human_density_matrix[1:10,1:5]
# dim(hd_model$human_density_matrix)
# ## Generation with correlations
# hd_model$uses_correlations <- TRUE
# # Check generative requirements and attempt to calculate human densities
# hd_model$generative_requirements_satisfied()
# hd_model$calculate_human_density()
# # Set up the correlation model
# corr_funct_params <- c(0.99, 8.00, 1.00)
# correlation_model <- CorrelationModel$new()
# hd_model$correlation_model <- correlation_model
# # Check generative requirements and attempt to calculate human densities
# hd_model$generative_requirements_satisfied()
# hd_model$calculate_human_density()
# # Calculate the correlations
# correlation_model$coordinates <- mamm_coords
# correlation_model$set_correlation_function_defaults(a=corr_funct_params[1],b=corr_funct_params[2],c=corr_funct_params[3])
# correlation_model$calculate_compact_decomposition()
# dim(hd_model$correlation_model$t_decomposition_compact_matrix)
# # Check generative requirements and attempt to calculate human densities
# hd_model$human_density_sample <- NULL # ignored when correlation used
# hd_model$generative_requirements_satisfied()
# hd_model$calculate_human_density(); hd_model$human_density_matrix[1:10,1:5]
# # Apply a lower threshold
# hd_model$calculate_sampling_parameters(lower_threshold = 2.46, mean_upper = 300, max_upper = 500)
# hd_model$calculate_human_density(); hd_model$human_density_matrix[1:10,1:5]
# # Apply lower and upper sample probabilities
# hd_model$human_density_lower <- 0.2
# hd_model$human_density_upper <- 0.4
# hd_model$calculate_human_density(); hd_model$human_density_matrix[1:10,1:5]
# # Use random seed
# hd_model$human_density_seed <- 1234
# hd_model$calculate_human_density(); hd_model$human_density_matrix[1:10,1:5]
# human_density_matrix1 <- hd_model$human_density_matrix
# hd_model$calculate_human_density(); hd_model$human_density_matrix[1:10,1:5]
# all(hd_model$human_density_matrix == human_density_matrix1)
# # Apply temporal correlation
# hd_model$temporal_correlation <- 0.95
# hd_model$calculate_human_density(); hd_model$human_density_matrix[1:10,1:5]
