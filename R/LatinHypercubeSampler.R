#' An R6 Class to represent a Latin Hypercube Sampler.
#'
#' @importFrom R6 R6Class
#' @export LatinHypercubeSampler

LatinHypercubeSampler <- R6Class("LatinHypercubeSampler",

  public = list(

    ## Attributes ##

    # See private/active for sample generation attributes

    # Dynamically attached attributes

    #' @field attached A list of attached attributes (name-value pairs).
    attached = list(),

    ## Methods ##

    #' @description
    #' Initialization method sets parameter names when provided.
    #' @param parameter_names Optional vector of sample parameter names.
    initialize = function(parameter_names = NULL) {
      self$parameter_names <- parameter_names
      self$parameter_distributions <- list()
    },

    #' @description
    #' Sets a parameter to sampled from a vector of classes.
    #' @param parameter_name Character string name of sample parameter.
    #' @param classes Vector of class values.
    set_class_parameter = function(parameter_name, classes) {
      if (!(parameter_name %in% self$parameter_names)) { # add parameter name
        self$parameter_names <- c(self$parameter_names, parameter_name)
      }
      self$parameter_distributions[[parameter_name]] <- list(type = "class", classes = classes)
    },

    #' @description
    #' Sets a parameter to be sampled from a uniform distribution with lower and upper bounds, optionally rounded to a specified number of decimal places.
    #' @param parameter_name Character string name of sample parameter.
    #' @param lower Lower bound of the uniform distribution (default = 0).
    #' @param upper Upper bound of the uniform distribution (default = 1).
    #' @param decimals Optional number of decimals applied to generated samples.
    set_uniform_parameter = function(parameter_name, lower = 0, upper = 1, decimals = NULL) {
      if (!(parameter_name %in% self$parameter_names)) { # add parameter name
        self$parameter_names <- c(self$parameter_names, parameter_name)
      }
      self$parameter_distributions[[parameter_name]] <- list(type = "uniform", lower = lower, upper = upper, decimals = decimals)
    },

    #' @description
    #' Sets a parameter to be sampled from a normal distribution with mean and standard deviation, optionally rounded to a specified number of decimal places.
    #' @param parameter_name Character string name of sample parameter.
    #' @param mean Mean parameter for the normal distribution (default = 0).
    #' @param sd Standard deviation parameter for the normal distribution (default = 1).
    #' @param decimals Optional number of decimals applied to generated samples.
    set_normal_parameter = function(parameter_name, mean = 0, sd = 1, decimals = NULL) {
      if (!(parameter_name %in% self$parameter_names)) { # add parameter name
        self$parameter_names <- c(self$parameter_names, parameter_name)
      }
      self$parameter_distributions[[parameter_name]] <- list(type = "normal", mean = mean, sd = sd, decimals = decimals)
    },

    #' @description
    #' Sets a parameter to be sampled from a lognormal distribution with log mean and log standard deviation, optionally rounded to a specified number of decimal places.
    #' @param parameter_name Character string name of sample parameter.
    #' @param meanlog Log mean parameter for the lognormal distribution (default = 0).
    #' @param sdlog Log standard deviation parameter for the lognormal distribution (default = 1).
    #' @param decimals Optional number of decimals applied to generated samples.
    set_lognormal_parameter = function(parameter_name, meanlog = 0, sdlog = 1, decimals = NULL) {
      if (!(parameter_name %in% self$parameter_names)) { # add parameter name
        self$parameter_names <- c(self$parameter_names, parameter_name)
      }
      self$parameter_distributions[[parameter_name]] <- list(type = "lognormal", meanlog = meanlog, sdlog = sdlog, decimals = decimals)
    },

    #' @description
    #' Sets a parameter to be sampled from a triangular distribution with lower and upper bounds and mode (peak), optionally rounded to a specified number of decimal places.
    #' @param parameter_name Character string name of sample parameter.
    #' @param lower Lower bound of the triangular distribution (default = 0).
    #' @param upper Upper bound of the triangular distribution (default = 1).
    #' @param mode Mode (or peak) of the triangular distribution (default = (lower + upper)/2).
    #' @param decimals Optional number of decimals applied to generated samples.
    set_triangular_parameter = function(parameter_name, lower = 0, upper = 1, mode = (lower + upper)/2, decimals = NULL) {
      if (!(parameter_name %in% self$parameter_names)) { # add parameter name
        self$parameter_names <- c(self$parameter_names, parameter_name)
      }
      self$parameter_distributions[[parameter_name]] <- list(type = "triangular", lower = lower, upper = upper, mode = mode, decimals = decimals)
    },

    #' @description
    #' Generates latin hypercube sample data for the parameters using corresponding distributions.
    #' @param number Number of samples to generate (default = 10).
    generate_samples = function(number = 10) {

      # Clear sample data
      self$sample_data <- NULL

      # Ensure distributions are set for all parameters
      if (all(self$parameter_names %in% names(self$parameter_distributions))) {

        # Generate uniform 0-1 LHS
        self$sample_data <- as.data.frame(lhs::randomLHS(number, length(self$parameter_names)))
        names(self$sample_data) <- self$parameter_names

        # Apply distributions for each parameter
        for (param in self$parameter_names) {
          distribution <- self$parameter_distributions[[param]]
          if (distribution$type == "class") {
            self$sample_data[[param]] <- distribution$classes[as.numeric(cut(self$sample_data[[param]], breaks = c(0, (1:length(distribution$classes)/length(distribution$classes)))))]
          } else if (distribution$type == "uniform") {
            self$sample_data[[param]] <- distribution$lower + self$sample_data[[param]]*(distribution$upper - distribution$lower)
          } else if (distribution$type == "normal") {
            self$sample_data[[param]] <- qnorm(self$sample_data[[param]], mean = distribution$mean, sd = distribution$sd)
          } else if (distribution$type == "lognormal") {
            self$sample_data[[param]] <- qlnorm(self$sample_data[[param]], meanlog = distribution$meanlog, sdlog = distribution$sdlog)
          } else if (distribution$type == "triangular") {
            self$sample_data[[param]] <- metRology::qtri(self$sample_data[[param]], min = distribution$lower, max = distribution$upper, mode = distribution$mode)
          }
          if (!is.null(distribution$decimals)) {
            self$sample_data[[param]] <- round(self$sample_data[[param]], distribution$decimals)
          }
        }

      } else {
        parameters_not_set <- self$parameter_names[which(!(self$parameter_names %in% names(self$parameter_distributions)))]
        stop(sprintf("Parameter distributions need to be set before generating samples: %s", paste(parameters_not_set, collapse = ", ")), call. = FALSE)
      }

    },

    #' @description
    #' Saves generated latin hypercube samples to a RSD (R data) or CSV (comma-separated values) file as specified by a path.
    #' @param path Directory path for saving samples.
    #' @param file_type Type of file for saving samples: "RSD" (default) or "CSV".
    save_samples = function(path, file_type = "RDS") {
      if (!is.null(self$sample_data)) {
        file_name <- paste0("sample_data.", file_type) # default file name
        if(file.exists(path) && dir.exists(path)) { # existing directory
          path <- file.path(path, file_name)
        } else if (!file.exists(path)) { # new directory and/or file
          if (length(grep(".", path, fixed = TRUE))) { # assume file name in path
            file_name <- basename(path)
            dir_path <- dirname(path)
          } else { # new directory
            dir_path <- path
          }
          if (!dir.exists(dir_path)) {
            suppressWarnings(try(dir_created <- dir.create(dir_path, recursive = TRUE), silent = TRUE))
            if (!dir_created) {
              stop(paste("Could not create directory", dir_path), call. = FALSE)
            }
          }
          path <- file.path(dir_path, file_name)
        }
        if (dir.exists(dirname(path))) {
          if (toupper(file_type) == "CSV") {
            write.csv(self$sample_data, file = path, row.names = FALSE)
          } else { # RSD
            saveRDS(self$sample_data, file = path)
          }
        } else {
          stop(paste("Could not save file", path), call. = FALSE)
        }
      } else {
        stop("There is no generated sample data to save", call. = FALSE)
      }

    }

  ), # end public

  private = list(

    ## Attributes ##

    # Sample generation attributes #
    .parameter_names = c(),
    .parameter_distributions = NULL,
    .sample_data = NULL

  ), # end private

  # Active binding accessors for private attributes (above) #
  active = list(

    #' @field parameter_names A vector of sample parameter names.
    parameter_names = function(value) {
      if (missing(value)) {
        private$.parameter_names
      } else {
        private$.parameter_names <- value
      }
    },

    #' @field parameter_distributions A list of sample distribution values (nested list with appropriate parameters).
    parameter_distributions = function(value) {
      if (missing(value)) {
        private$.parameter_distributions
      } else {
        private$.parameter_distributions <- value
      }
    },

    #' @field sample_data A data frame of generated sample values.
    sample_data = function(value) {
      if (missing(value)) {
        private$.sample_data
      } else {
        private$.sample_data <- value
      }
    }

  ) # end active

)
