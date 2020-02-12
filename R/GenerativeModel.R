#' R6 Class Representing a Model for Dynamic Attribute Generation
#'
#' @description
#' R6 class representing a model that dynamically generates attribute values
#' (\emph{outputs}) via reading data from files, or running assigned functions or
#' built-in functions (assigned as \emph{default} in inherited classes), using
#' simulation sample parameters (\emph{inputs}).
#'
#' @importFrom R6 R6Class
#' @include GenericModel.R
#' @export GenerativeModel

GenerativeModel <- R6Class("GenerativeModel",
  inherit = GenericModel,
  public = list(

    ## Attributes ##

    ## Methods ##

    # Inherited methods (from GenericClass & GenericModel) #
    #   read_from_rds(path)
    #   save_to_rds(path)
    #   get_attribute_names()
    #   get_attributes(params)
    #   set_attributes(params = list(), ...)

    # Overwritten/overridden methods #

    #' @description
    #' Initialization method sets the generative template and requirements as well as any attributes passed via a \emph{params} list or individually.
    #' @param generative_template A \emph{GenerativeTemplate} or inherited class object containing the file and function templates utilized (facilitates shallow cloning).
    #' @param generative_requirements A list of attribute names and the template setting ("file" or "function") that is required to generate their values.
    #' @param ... Parameters passed via a \emph{params} list or individually.
    initialize = function(generative_template = NULL, generative_requirements = NULL, ...) {
      if (!is.null(generative_template)) {
        self$generative_template <- generative_template
      }
      if (is.null(generative_requirements)) {
        self$generative_requirements <- list()
      } else {
        self$generative_requirements <- generative_requirements
      }
      super$initialize(...)
      if (is.null(self$description)) {
        self$description <- "Unnamed"
      }
    },

    #' @description
    #' Creates a new (re-initialized) object of the current (inherited) object class with optionally passed parameters.
    #' @param ... Parameters passed via the inherited class constructor (defined in initialize and run via new).
    #' @return New object of the inherited class.
    new_clone = function(...) {
      return(super$new_clone(generative_template = self$generative_template,
                             generative_requirements = self$generative_requirements, ...))
    },

    # New methods #

    #' @description
    #' Returns a list of generated output values (attributes) corresponding to the sample input values (attributes).
    #' @param input_values List of sample input values for generative model attributes.
    #' @return List containing generated model output attributes and/or any error/warning messages.
    generate = function(input_values = list()) {
      if (is.null(self$inputs) || is.null(self$outputs)) {
        missing <- c("inputs", "outputs")[which(c(is.null(self$inputs), is.null(self$outputs)))]
        return(list(warning_messages = sprintf("%s generation requires settings for: %s", self$description, paste(missing, collapse = ", "))))
      }
      inputs_present <- 0
      for (param in self$inputs) {
        if (any(self$get_attribute_aliases(param) %in% names(input_values))) {
          inputs_present <- inputs_present + 1
        }
      }
      if (inputs_present == length(self$inputs)) {
        generative_requirements_satisfied <- self$generative_requirements_satisfied()
        if (all(unlist(generative_requirements_satisfied))) { # ensure the required file or function templates are set for specified outputs
          return(self$new_clone(params = input_values)$get_attributes(c(self$outputs, "error_messages", "warning_messages")))
        } else {
          return(list(warning_messages = sprintf("%s generation requires further settings for output(s): %s", self$description, paste(names(which(!unlist(generative_requirements_satisfied))), collapse = ", "))))
        }
      } else {
        return(list(warning_messages = sprintf("%s generation requires inputs: %s", self$description, paste(self$inputs, collapse = ", "))))
      }
    },

    #' @description
    #' Adds a file template for reading RDS/CSV files for a given model attribute.
    #' @param param Name of model attribute to be read from a file.
    #' @param path_template Template string for the file path with \emph{\%s} placeholders (see \emph{sprintf}) for simulation sample parameters.
    #' @param path_params Array of the names of the simulation sample parameters to be substituted (in order) into the path template.
    #' @param file_type File type \emph{"RDS"} (default) or \emph{"CSV"} to be read.
    add_file_template = function(param, path_template, path_params = c(), file_type = "RDS") {
      if (is.character(param) && is.character(path_template)) {
        if (length(path_params) == (length(strsplit(paste0("#", path_template,"#"), "%s")[[1]]) - 1)) {
          self$file_templates[[param]] <- list()
          self$file_templates[[param]]$path_template <- path_template
          self$file_templates[[param]]$path_params <- c(path_params)
          if (toupper(file_type) == "RDS" || toupper(file_type) == "CSV") {
            self$file_templates[[param]]$file_type <- toupper(file_type)
          } else {
            return("The file type should be RDS or CSV")
          }
        } else {
          return("Ensure the path template contains a corresponding %s for each path parameter")
        }
      } else {
        return("The parameter name and path template should be strings")
      }
    },

    #' @description
    #' Adds a function template for running a function to calculate a given model attribute.
    #' @param param Name of model attribute to be generated using a function.
    #' @param source_path Path to the file containing the function definition.
    #' @param call_template Template string for the function call with \emph{\%s} placeholders (see \emph{sprintf}) for simulation sample parameters.
    #' @param call_params Array of the names of the simulation sample parameters to be substituted (in order) into the function call template.
    add_function_template = function(param, source_path, call_template, call_params = c()) {
      if (is.character(param) && is.character(source_path) && is.character(call_template)) {
        self$function_templates[[param]] <- list()
        if (length(call_params) == (length(strsplit(paste0("#", call_template,"#"), "%s", fixed = TRUE)[[1]]) - 1)) {
          if (file.exists(source_path)) {
            self$function_templates[[param]][["source_path"]] <- source_path
            function_name <- strsplit(call_template, "(", fixed = TRUE)[[1]][1]
            source(source_path)
            self$function_templates[[param]][[function_name]] <- eval(parse(text = sprintf("%s", function_name)))
          } else {
            return("Ensure the function source path exists")
          }
          self$function_templates[[param]][["call_template"]] <- call_template
          self$function_templates[[param]][["call_params"]] <- c(call_params)
        } else {
          return("Ensure the function call template contains a corresponding %s for each call parameter")
        }
      } else {
        return("The parameter name, source path, and call template should be strings")
      }
    },

    #' @description
    #' Reads and returns the value of a model attribute from a file using the corresponding file template and simulation sample parameters.
    #' @param param Name of model attribute to be read from the file.
    #' @return Model attribute value read from a file.
    read_file = function(param) {
      if (param %in% names(self$file_templates)) {
        path_template <- self$file_templates[[param]]$path_template
        path_params <- self$file_templates[[param]]$path_params
        if (all(path_params %in% names(self$get_attributes(path_params)))) {
          file_path <- eval(parse(text = paste0("sprintf(path_template, ", paste("self$", path_params, sep = "", collapse = ", "), ")")))
          if (is.character(file_path) && file.exists(file_path)) {
            if (self$file_templates[[param]]$file_type == "CSV") {
              return(read.csv(file = file_path))
            } else { # RDS
              return(readRDS(file = file_path))
            }
          } else {
            self$error_messages <- unique(append(self$error_messages, paste("Error reading file", file_path)))
            return(NULL)
          }
        } else {
          missing_params <- path_params[which(!path_params %in% names(self$get_attributes(path_params)))]
          self$warning_messages <- unique(append(self$warning_messages, paste("Missing parameter(s) for file template: ", paste(missing_params, collapse = ", "))))
          return(NULL)
        }
      }
    },

    #' @description
    #' Returns the calculated value of a model attribute using the corresponding function template and simulation sample parameters.
    #' @param param Name of model attribute to be calculated using a function.
    #' @return Model attribute value calculated using a function.
    run_function = function(param) {
      if (param %in% names(self$function_templates)) {
        call_template <- sprintf("self$function_templates$%s$%s", param, self$function_templates[[param]]$call_template)
        call_params <- self$function_templates[[param]]$call_params
        function_call <- eval(parse(text = paste0("sprintf(call_template, ", paste("self$", call_params, sep = "", collapse = ", "), ")")))
        return(eval(parse(text = function_call)))
      }
    },

    #' @description
    #' Adds attribute names and the template setting (\emph{"file"} or \emph{"function"}) that is required to generate their values (via a \emph{params} list or individually).
    #' Returns the calculated value of a model attribute using the corresponding function template and simulation sample parameters.
    #' @param params Parameters passed via a list (e.g. \code{params = list(attr1 = "file", attr2 = "function")}).
    #' @param ... Parameters passed individually (e.g. \code{attr3 = "file"}).
    #' @return Returns message if problems encountered.
    add_generative_requirements = function(params = list(), ...) {
      params <- c(list(...), params) # prioritise individual parameters
      invalid_params <- list()
      # Set via params list when valid
      for (param in names(params)) {
        if ((param %in% self$model_attributes || param %in% private$.active_attributes) && params[[param]] %in% c("file", "function")) {
          self$generative_requirements[[param]] <- params[[param]]
        } else {
          invalid_params[[param]] <- params[[param]]
        }
      }
      if (length(invalid_params)) { # return a message for invalid parameters
        return_message <- "Generative requirements must be added to model attributes only as 'file' or 'function':"
        for (param in names(invalid_params)) {
          return_message <- paste0(return_message, (sprintf(" %s = '%s' not added;", param, invalid_params[[param]])))
        }
        return(return_message)
      }
    },

    #' @description
    #' Returns a boolean to indicate that all the file and/or function template settings that are required for attribute generation are present.
    #' @return Boolean to indicate that the required settings for attribute generation are present.
    generative_requirements_satisfied = function() {
      if (length(self$generative_requirements)) {
        satisfied <- self$generative_requirements
        satisfied[] <- FALSE
        for (i in length(self$generative_requirements)) {
          param <- names(self$generative_requirements)[i]
          template <- self$generative_requirements[[i]]
          if (template == "file") {
            satisfied[[i]] <- param %in% names(self$file_templates)
            if (!satisfied[[i]]) {
              satisfied$file_templates <- FALSE
            }
          } else if (template == "function") {
            satisfied[[i]] <- param %in% names(self$function_templates)
            if (!satisfied[[i]]) {
              satisfied$function_templates <- FALSE
            }
          }
        }
        return(satisfied)
      } else { # no requirements
        return(TRUE)
      }
    }

  ), # end public

  private = list(

    ## Attributes ##

    # .attribute_aliases [inherited]

    # Generative attributes #
    .generative_template = NULL,
    .generative_requirements = NULL,

    # Model attributes #
    # .model_attributes  [inherited]

    # Attributes accessible via model get/set methods #
    .active_attributes = c("description", "inputs", "outputs", "file_templates", "function_templates")

    # Errors and warnings #
    # .error_messages    [inherited]
    # .warning_messages  [inherited]

  ), # end private

  # Active binding accessors for private attributes (above and template nested) #
  active = list(

    # attribute_aliases [inherited]

    # Generative attribute accessors #

    #' @field generative_template A nested object for model attributes that are maintained via shallow or \emph{new} cloning.
    generative_template = function(value) {
      if (missing(value)) {
        private$.generative_template
      } else {
        private$.generative_template <- value
      }
    },

    #' @field generative_requirements A list of attribute names and the template setting (\emph{"file"}, \emph{"function"}, or \emph{"default"}) that is required to generate their values.
    generative_requirements = function(value) {
      if (missing(value)) {
        private$.generative_requirements
      } else {
        private$.generative_requirements <- value
      }
    },

    # Model attribute accessors #

    # Nested model attribute accessors #

    #' @field description A brief description of what the generative model generates.
    description = function(value) {
      if (missing(value)) {
        self$generative_template$description
      } else {
        self$generative_template$description <- value
      }
    },

    #' @field inputs An array of input attribute names for the generative model.
    inputs = function(value) {
      if (missing(value)) {
        self$generative_template$inputs
      } else {
        self$generative_template$inputs <- value
      }
    },

    #' @field outputs An array of output attribute names for the generative model.
    outputs = function(value) {
      if (missing(value)) {
        self$generative_template$outputs
      } else {
        self$generative_template$outputs <- value
      }
    },

    #' @field file_templates A nested list of file template attributes.
    file_templates = function(value) {
      if (missing(value)) {
        self$generative_template$file_templates
      } else {
        self$generative_template$file_templates <- value
      }
    },

    #' @field function_templates A nested list of function template attributes.
    function_templates = function(value) {
      if (missing(value)) {
        self$generative_template$function_templates
      } else {
        self$generative_template$function_templates <- value
      }
    }

    # Local (non-nested) model attribute accessors #
    #   in inherited classes

    # Errors and warnings #
    
    # error_messages    [inherited]
    
    # warning_messages  [inherited]
    
  ) # end active
)
