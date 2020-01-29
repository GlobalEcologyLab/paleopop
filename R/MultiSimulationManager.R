#' R6 Class Representing a Multi-Simulation Manager.
#'
#' @description
#' An R6 class to represent a manager for running multiple population model simulations
#' and saving results.
#'
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom R6 R6Class
#' @include GenericResultsManager.R
#' @include PopulationSimulator.R
#' @include PopulationModel.R
#' @include NicheCarryingCapacityModel.R
#' @include DispersalModel.R
#' @include HumanDensityModel.R
#' @export MultiSimulationManager

MultiSimulationManager <- R6Class("MultiSimulationManager",
  inherit = GenericResultsManager,
  public = list(

    ## Attributes ##

    # Inherited public attributes (from GenericResultsManager) #
    #   attached

    ## Methods ##

    # Inherited methods (from GenericClass & GenericResultsManager) #
    #   new_clone(...)
    #   read_from_rds(path)
    #   save_to_rds(path = NULL)
    #   get_attribute(param)
    #   get_message_sample(status_message, sample_index)
    #   get_results_filename(sample_index)

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method sets any included attributes (sample_data, model_template, niche_k_model, dispersal_model, human_density_model, population_simulator, parallel_cores, results_dir, results_filename_attributes) and attaches other attributes individually listed.
    #' @param ... Parameters listed individually.
    initialize = function(...) {
      if (!("population_simulator" %in% names(list(...)))) {
        self$population_simulator <- PopulationSimulator$new()
      }
      super$initialize(...)
    },

    # New methods #

    #' @description
    #' Runs the multiple population simulations (via the set function), stores the results, and creates/writes a simulation log.
    #' @return Simulator log as a list.
    run = function() {

      # Check for error messages
      if (!is.null(self$error_messages)) {
        error_messages <- self$error_messages
        self$error_messages <- NULL
        stop(error_messages, call. = FALSE)
      }

      # Check that model and sample data is present
      if (is.null(self$model_template) || length(self$sample_data) == 0) {
        stop("No model samples to run", call. = FALSE)
      }

      # Check that population simulator/function is present
      if (is.null(self$population_simulator)) {
        stop("The population simulator has not been set", call. = FALSE)
      } else if (is.null(self$population_simulator$simulation_function)) {
        stop("The population simulator function has not been set", call. = FALSE)
      }

      # Check that results directory is present or create it if required
      if (is.null(self$results_dir)) {
        stop("No output directory set for results", call. = FALSE)
      }
      if (is.na(file.info(self$results_dir)[1,"isdir"])) {
        suppressWarnings(try(
          dir_created <- dir.create(self$results_dir, recursive = TRUE),
          silent = TRUE))
        if (!dir_created) {
          stop(paste("Could not create results directory", self$results_dir), call. = FALSE)
        }
      }

      # Create a nested PaleoPop (or descendant) model for cloning
      self$nested_model <- self$model_template$new_clone(template = self$model_template)

      # Allow extra attachments to be passed
      if ("nested_model" %in% names(self$attached)) {
        self$nested_model$attached <- self$attached$nested_model
      }

      # Resolve sample attributes and attach (as aliases) them separately to the model to avoid repetition
      self$nested_model$attached$sample_model_names <- names(self$sample_data)[names(self$sample_data) %in% self$nested_model$get_attribute_aliases()]
      self$nested_model$sample_attributes <- self$nested_model$attached$sample_model_names
      if (all(self$niche_k_model$inputs %in% names(self$sample_data))) { # sampled niche K?
        self$nested_model$attached$sample_niche_k_names <- self$niche_k_model$outputs
        self$nested_model$sample_attributes <- unique(c(self$nested_model$sample_attributes, self$niche_k_model$outputs))
      }
      if (all(self$dispersal_model$inputs %in% names(self$sample_data))) { # sampled dispersals?
        self$nested_model$attached$sample_dispersal_names <- self$dispersal_model$outputs
        self$nested_model$sample_attributes <- unique(c(self$nested_model$sample_attributes, self$dispersal_model$outputs))
      }
      if (all(self$human_density_model$inputs %in% names(self$sample_data))) { # sampled human densities?
        self$nested_model$attached$sample_human_density_names <- self$human_density_model$outputs
        self$nested_model$sample_attributes <- unique(c(self$nested_model$sample_attributes, self$human_density_model$outputs))
      }

      # Check the completeness/consistency of the first sample only
      model <- self$nested_model$clone()
      self$set_model_sample(model, 1)
      if (length(model$error_messages)) {
        stop(c("Error(s) setting model sample attributes: ", model$error_messages), call. = FALSE)
      }
      if (!model$is_complete()) {
        incomplete_message <- "Model attributes are incomplete"
        if (!model$is_consistent()) {
          incomplete_message <- paste(incomplete_message, "/inconsistent", sep = "")
        }
        incomplete_message <- paste0(incomplete_message, ": ", paste(model$incomplete_attributes(), collapse = ", "))
        stop(incomplete_message, call. = FALSE)
      }
      model <- NULL # release from memory

      # Run sample simulations in parallel
      doParallel::registerDoParallel(cores = self$parallel_cores)
      simulation_log <- foreach(i = 1:nrow(self$sample_data), .errorhandling = c("pass")) %dopar% {

        # Clone the model
        model <- self$nested_model$clone()

        # Set the model sample attributes
        self$set_model_sample(model, i)
        if (length(model$error_messages)) {
          return(list(successful = FALSE, message = self$get_message_sample("Error(s) setting model %s sample attributes", i), errors = model$error_messages))
        }

        # Create and run the simulator
        simulator <- self$population_simulator$new_clone(population_model = model, sample_id = i)
        simulator_run_status <- simulator$run()

        # Substitute sample details into the simulator run status message
        simulator_run_status$message <- self$get_message_sample(simulator_run_status$message, i)

        # Save results
        if (!is.null(simulator$results)) {
          results_file <- file.path(self$results_dir, paste0(self$get_results_filename(i), ".RDS"))
          suppressWarnings(try(
            saveRDS(simulator$results, file = results_file),
            silent = TRUE))
          if (file.exists(results_file)) {
            simulator_run_status$message <- paste0(simulator_run_status$message, " and the results were saved")
          } else {
            simulator_run_status$successful <- FALSE
            simulator_run_status$message <- paste0(simulator_run_status$message, ", but the results could not be saved in ", results_file)
          }
        }

        return(simulator_run_status)
      }
      doParallel::stopImplicitCluster()

      # Summarize and write log to a file
      simulation_log <- self$log_simulation(simulation_log)

      return(simulation_log)
    },

    #' @description
    #' Sets the model sample attributes via the sample data frame, and the niche K, dispersal and human density generative models.
    #' @param model Population model (clone) to receive sample attributes.
    #' @param sample_index Index of sample from data frame.
    set_model_sample = function(model, sample_index) {
      sample_list <- as.list(self$sample_data[sample_index, ])
      model$set_sample_attributes(params = sample_list[model$attached$sample_model_names])
      if (!is.null(model$attached$sample_niche_k_names)) {
        model$set_sample_attributes(params = self$niche_k_model$generate(input_values = sample_list[self$niche_k_model$inputs]))
      }
      if (!is.null(model$attached$sample_dispersal_names)) {
        model$set_sample_attributes(params = self$dispersal_model$generate(input_values = sample_list[self$dispersal_model$inputs]))
      }
      if (!is.null(model$attached$sample_human_density_names)) {
        model$set_sample_attributes(params = self$human_density_model$generate(input_values = sample_list[self$human_density_model$inputs]))
      }
    },

    #' @description
    #' Summarizes the simulation log generated within the run method and writes it to (text and RDS) files in the results directory.
    #' @param simulation_log Nested list of simulation log entries generated via the run method.
    log_simulation = function(simulation_log) {
      # Determine which simulations were successful and collect any warnings
      successful_array <- array(FALSE, length(simulation_log))
      warning_indices <- c()
      for (i in 1:length(simulation_log)) {
        if (is.null(simulation_log[[i]]$successful)) {
          simulation_log[[i]] <- list(message = as.character(simulation_log[[i]]), successful = FALSE)
        }
        successful_array[i] <- simulation_log[[i]]$successful
        if (!is.null(simulation_log[[i]]$warnings)) {
          warning_indices <- c(warning_indices, i)
        }
      }
      # Add a summary and failure & warning indices to the log
      simulation_log <- list(summary = sprintf("%s of %s sample models ran and saved results successfully",
                                               length(which(successful_array)), length(simulation_log)),
                             failed_indices = which(!successful_array),
                             warning_indices = warning_indices,
                             full_log = simulation_log)
      if (length(warning_indices)) {
        transformation_log$summary <- paste(transformation_log$summary, "with warnings")
      }
      # Write a log file
      log_file <- file.path(self$results_dir, "simulation_log.txt")
      suppressWarnings(try({
        file_con <- file(log_file, 'w')
        writeLines(c(simulation_log$summary), con = file_con)
        if (length(simulation_log$failed_indices)) {
          writeLines(c("", paste(length(simulation_log$failed_indices), "failed runs/errors:")), con = file_con)
          for (i in simulation_log$failed_indices) {
            writeLines(c("", simulation_log$full_log[[i]]$message), con = file_con)
            if (!is.null(simulation_log$full_log[[i]]$errors)) {
              writeLines(simulation_log$full_log[[i]]$errors, con = file_con)
            }
          }
        }
        if (length(warning_indices)) {
          writeLines(c("", paste(length(warning_indices), "warnings:")), con = file_con)
          for (i in warning_indices) {
            writeLines(c("", simulation_log$full_log[[i]]$message), con = file_con)
            writeLines(simulation_log$full_log[[i]]$warnings, con = file_con)
          }
        }
        close(file_con)
        # Also save extended log as RDS
        saveRDS(simulation_log, file = file.path(self$results_dir, "simulation_log.RDS"))
      }, silent = TRUE))
      return(simulation_log)
    }

  ), # end public

  private = list(

  ## Attributes ##

  # Simulation manager attributes #
  .manager_attributes = c("sample_data", "model_template", "nested_model", "niche_k_model",
                          "dispersal_model", "human_density_model", "population_simulator",
                          "parallel_cores", "results_dir", "results_filename_attributes"),
  # .sample_data                   [inherited]
  .model_template = NULL,
  .nested_model = NULL,
  # .niche_k_model                 [inherited]
  .dispersal_model = NULL,
  # .human_density_model           [inherited]
  .population_simulator = NULL
  # .parallel_cores                [inherited]
  # .results_dir                   [inherited]
  # .results_filename_attributes   [inherited]

  # Errors and warnings #
  # .error_messages                [inherited]
  # .warning_messages              [inherited]

  ## Methods ##

  ), # end private

  # Active binding accessors for private simulation manager attributes (above) #
  active = list(

    # sample_data                  [inherited]

    #' @field model_template A PopulationModel (or inherited class) object with parameters common to all simulations.
    model_template = function(value) {
      if (missing(value)) {
        private$.model_template
      } else {
        if (!is.null(value) && !("PopulationModel" %in% class(value))) {
          stop("Model template must be a PopulationModel or inherited class object", call. = FALSE)
        } else {
          private$.model_template <- value
        }
      }
    },

    #' @field nested_model A PopulationModel (or inherited class) object with empty sample parameters and a nested model template common to all simulations.
    nested_model = function(value) {
      if (missing(value)) {
        private$.nested_model
      } else {
        if (!is.null(value) && !("PopulationModel" %in% class(value))) {
          stop("Nested model must be a PopulationModel or inherited class object", call. = FALSE)
        } else {
          private$.nested_model <- value
        }
      }
    },

    # niche_k_model                [inherited]

    #' @field dispersal_model A dispersal generative model (\emph{DispersalModel} or inherited class) object for generating sample dispersals.
    dispersal_model = function(value) {
      if (missing(value)) {
        private$.dispersal_model
      } else {
        if (!is.null(value) && !("DispersalModel" %in% class(value))) {
          stop("Dispersal generative model must be a DispersalModel or inherited class object", call. = FALSE)
        } else {
          private$.dispersal_model <- value
        }
      }
    },

    # human_density_model          [inherited]

    #' @field population_simulator A PopulationSimulator (or inherited class) object for running the simulations.
    population_simulator = function(value) {
      if (missing(value)) {
        private$.population_simulator
      } else {
        if (!is.null(value) && !("PopulationSimulator" %in% class(value))) {
          stop("Model template must be a PopulationSimulator or inherited class object", call. = FALSE)
        } else {
          private$.population_simulator <- value
        }
      }
    }

    # parallel_cores               [inherited]

    # results_dir                  [inherited]

    # results_filename_attributes  [inherited]

    # error_messages               [inherited]

    # warning_messages             [inherited]

  ) # end active
)
