#' R6 Class with Generic Reusable Functionality
#'
#' @description
#' R6 class with generic (abstract) new cloning and RDS save and read functionality.
#'
#' @examples
#' TestInheritedClass <- R6::R6Class("TestInheritedClass",
#'                                   inherit = GenericClass,
#'                                   public = list(attr1 = NULL, attr2 = NULL))
#' test_class <- TestInheritedClass$new()
#' test_class$attr1 <- 11
#' test_class$attr2 <- 22
#' test_class
#' test_class$new_clone()
#'
#' @importFrom R6 R6Class
#' @export GenericClass

GenericClass <- R6Class("GenericClass",
  public = list(

    ## Attributes ##

    #' @field object_generator Class object generator used to create new clones, particularly for user inheritance.
    object_generator = NULL,

    ## Methods ##
    
    # Overwritten/overridden methods #

    #' @description
    #' Initialization method saves an object generator for new cloning.
    #' @param object_generator Class object generator used to create new clones, particularly for user inheritance.
    #' @param ... Parameters passed individually (ignored).
    initialize = function(object_generator = NULL, ...) {
      if (!is.null(object_generator)) {
        self$object_generator <- object_generator
      } else {
        self$object_generator <- eval(parse(text = class(self)[1]))
      }
    },
    
    # New methods #
    
    #' @description
    #' Creates a new (re-initialized) object of the current (inherited) object class with optionally passed parameters.
    #' @param ... Parameters passed via the inherited class constructor (defined in initialize and run via new).
    #' @return New object of the inherited class.
    new_clone = function(...) {
      return(self$object_generator$new(object_generator = self$object_generator, ...))
    },

    #' @description
    #' Reads a class object from a RDS file via a path to a file, or a directory (using class name).
    #' @param path Path to file or directory containing \emph{<class-name>.RDS} file.
    read_from_rds = function(path) {
      file_name <- paste0(class(self)[1], ".RDS") # default file name
      if(file.exists(path) && dir.exists(path)) { # existing directory
        path <- file.path(path, file_name)
      }
      if (file.exists(path)) {
        object <- readRDS(file = path)
        if (class(object)[1] == class(self)[1]) {
          return(object)
        } else {
          stop(paste("Class of object read from file:", class(object)[1],
                        "does not match this class:", class(self)[1]), call. = FALSE)
        }
      } else {
        stop(paste("File", path, "not found"), call. = FALSE)
      }
    },

    #' @description
    #' Saves a class object to a RDS file via a path to a file, or a directory (using class name).
    #' @param path Path to file or directory (where file \emph{<class-name>.RDS} is saved).
    save_to_rds = function(path) {
      file_name <- paste0(class(self)[1], ".RDS") # default file name
      if (file.exists(path) && dir.exists(path)) { # existing directory
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
        saveRDS(self, file = path)
      } else {
        stop(paste("Could not save file", path), call. = FALSE)
      }
    }

  ), # end public

  private = list(

    ## Attributes ##
    
  ) # end private

)
