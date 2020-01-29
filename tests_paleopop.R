#### GenericClass ####
TEST_DIRECTORY <- "E:\\paleopop\\test_saves"
TestInheritedClass <- R6::R6Class("TestInheritedClass", inherit = GenericClass,
                                  public = list(attr1 = NULL, attr2 = NULL))
test_class <- TestInheritedClass$new()
test_class$attr1 <- "test1"
test_class$attr2 <- c(11, 22, 33)
test_class
# New cloning
test_clone <- test_class$new_clone()
test_clone
# File save and read
test_class$save_to_rds(path = TEST_DIRECTORY)
test_class$attr1 <- "test2"
test_class$save_to_rds(path = file.path(TEST_DIRECTORY, "test2"))
test_class$attr1 <- "test3"
test_class$save_to_rds(path = file.path(TEST_DIRECTORY, "test3.RDS"))
test_class$attr1 <- "test4"
test_class$save_to_rds(path = file.path(TEST_DIRECTORY, "test4", "test4.RDS"))
test_class$save_to_rds(path = "S:\\bogus\\path")
test_class$save_to_rds(path = "S:\\bogus\\path\\test5.RDS")
TestInheritedClass$new()$read_from_rds(path = TEST_DIRECTORY)
test_clone$read_from_rds(path = TEST_DIRECTORY)
test_clone$read_from_rds(path = file.path(TEST_DIRECTORY, "test2"))
test_clone$read_from_rds(path = file.path(TEST_DIRECTORY, "test3.RDS"))
test_clone$read_from_rds(path = file.path(TEST_DIRECTORY, "test4", "test4.RDS"))
test_clone$read_from_rds(path = "S:\\bogus\\path")
test_clone$read_from_rds(path = file.path(TEST_DIRECTORY, "test5.RDS"))


#### GenericModel ####
TEST_DIRECTORY <- "E:\\paleopop\\test_saves"
TestInheritedModel <- R6::R6Class("TestInheritedModel", inherit = GenericModel,
  public = list(model_attributes = c("attr1", "attr2"), attr1 = NULL, attr2 = NULL),
  private = list(.model_attributes = c("attr3", "attr4"), .active_attributes = c("attr3"), .attr3 = NULL, .attr4 = NULL),
  active = list(attr3 = function(value) {if (missing(value)) { private$.attr3 } else { private$.attr3 <- value }})
)
# Initialization
params1 <- list(attr1 = 101, attr3 = c("a","b"), dog = array(7,3))
inherited_model <- TestInheritedModel$new(params = params1, attr2 = 202, attr4 = c("c","d"), cat = array(9,3))
inherited_model$attr1; inherited_model$attr2; inherited_model$attr3; inherited_model$attached
# Get and set attribute methods
inherited_model$get_attribute_names()
inherited_model$get_attributes()
inherited_model$get_attributes(c("attr2","attr4","cat","dummy"))
inherited_model$set_attributes(list(attr1 = 110, attr3 = c("x","y")), attr2 = 220, attr4 = c("w","x"), dog = array(8,3), horse = "fast")
inherited_model; inherited_model$attached
# Error and warning messages
inherited_model$set_attributes(list(error_messages = "error1", warning_messages = "warning1"), warning_messages = "warning2", error_messages = "error2")
inherited_model$error_messages; inherited_model$warning_messages
inherited_model$set_attributes(list(error_messages = c("error1", "error3"), warning_messages = c("warning2", "warning3")))
inherited_model$error_messages; inherited_model$warning_messages
# Get and set via aliases
inherited_model$attribute_aliases <- list(a1 = "attr1", a2 = "attr2", a3 = "attr3", a4 = "attr4", c = "cat", d = "dog", h = "horse",
                                          errors = "error_messages", warnings = "warning_messages")
inherited_model$get_attribute_aliases()
inherited_model$get_attribute_aliases("attr1")
inherited_model$get_attribute_aliases(c("attr1", "cat", "dummy"))
inherited_model$get_attributes(c("a1", "a2", "a3", "a4", "c", "d", "h", "errors", "warnings"))
inherited_model$set_attributes(list(a1 = 111, a3 = c("q","p")), a2 = 222, a4 = c("a","z"), c = array(3,3), d = array(5,4), h = "faster")
inherited_model$set_attributes(list(errors = c("error5", "error6"), warnings = c("warning7")))
inherited_model$get_attributes(c("a1", "a2", "a3", "a4", "c", "d", "h", "errors", "warnings"))
inherited_model$get_attributes()
inherited_model$error_messages
inherited_model$warning_messages
# New cloning
params2 <- list(attr1 = 222, attr3 = c("t","u"), dog = array(6,6))
inherited_clone <- inherited_model$new_clone(params = params2, attr2 = 333, attr4 = c("i","j"), cat = array(7,6))
inherited_clone$get_attributes()
inherited_clone$attribute_aliases


#### PopulationModel ####
# Initialization
template_model <- PopulationModel$new(params = list(duration = 10, populations = 5),
                                      coordinates = array(1:10, c(5, 2)),
                                      correlation_matrix = (diag(5)*0.8 + 0.2))
template_model$duration; template_model$populations
template_model$coordinates
template_model$correlation_matrix
attribute_aliases <- list(correlations = "correlation_matrix",
                          capacities = "carrying_capacities",
                          dispersals = "dispersal_matrix")
template_model$attribute_aliases <- attribute_aliases
template_model$get_attributes("correlations")
# Template nesting
nested_model <- PopulationModel$new(template = template_model)
nested_model$duration
nested_model$get_template()$populations
nested_model <- PopulationModel$new(template = list(duration = 10,
                                                    populations = 5,
                                                    coordinates = array(1:10, c(5, 2)),
                                                    correlation_matrix = (diag(5)*0.8 + 0.2)),
                                    attribute_aliases = attribute_aliases)
class(nested_model$get_template())
nested_model$duration
# Set sample attributes
nested_model$set_sample_attributes(params = c("carrying_capacities", "dispersal_matrix"),
                                   initial_abundances = seq(10, 30, by = 5),
                                   human_densities = array((1:50)/50, c(5, 10)))
nested_model$sample_attributes
nested_model$carrying_capacities <- matrix(seq(50, 32, by = -2), nrow = 5, ncol = 10, byrow = TRUE)
nested_model$get_attributes()
# Get/set via aliases
nested_model$get_attributes(c("correlations", "capacities"))
nested_model$set_attributes(correlations = (diag(5)*0.7 + 0.3),
                            dispersals = (diag(5)*-0.05 + 0.05))
nested_model$get_attributes(c("correlations", "dispersals"))
nested_model$set_sample_attributes(params = list(dispersals = (diag(5)*-0.1 + 0.1)),
                                   capacities = matrix(seq(80, 35, by = -5), nrow = 5, ncol = 10, byrow = TRUE))
nested_model$get_attributes(c("dispersals", "capacities"))
nested_model$get_attributes()
# Check consistency and completeness
nested_model$is_consistent()
nested_model$is_complete()
nested_model$list_completeness()
nested_model$incomplete_attributes()
nested_model$coordinates <- NULL # OK
nested_model$dispersal_matrix <- NULL # OK
nested_model$correlation_matrix <- NULL # OK
nested_model$human_densities <- NULL # OK
nested_model$is_complete()
nested_model$initial_abundances <- nested_model$initial_abundances[-1]
nested_model$is_consistent(c("duration", "populations", "coordinates", "carrying_capacities",
                             "dispersal_matrix", "correlation_matrix", "human_densities"))
nested_model$is_consistent("initial_abundances")
nested_model$is_complete()
nested_model$incomplete_attributes()
nested_model$coordinates <- array(1:12, c(6, 2))
nested_model$is_consistent("coordinates")
nested_model$coordinates <- array(1:10, c(5, 3))
nested_model$is_consistent("coordinates")
nested_model$carrying_capacities <- cbind(nested_model$carrying_capacities, 30)
nested_model$is_consistent("carrying_capacities")
nested_model$carrying_capacities <- rbind(nested_model$carrying_capacities[,-1], 50)
nested_model$is_consistent("carrying_capacities")
nested_model$dispersal_matrix <- (diag(5)*-0.1 + 0.1)[-1,]
nested_model$is_consistent("dispersal_matrix")
nested_model$dispersal_matrix <- (diag(5)*-0.1 + 0.1)[,-1]
nested_model$is_consistent("dispersal_matrix")
nested_model$correlation_matrix <- (diag(5)*0.8 + 0.2)[-1,]
nested_model$is_consistent("correlation_matrix")
nested_model$correlation_matrix <- (diag(5)*0.8 + 0.2)[,-1]
nested_model$is_consistent("correlation_matrix")
nested_model$human_densities = array((1:60)/60, c(6, 10))
nested_model$is_consistent("human_densities")
nested_model$human_densities = array((1:55)/55, c(5, 11))
nested_model$is_consistent("human_densities")
nested_model$is_complete()
nested_model$list_completeness()
nested_model$incomplete_attributes()


#### PaleoPopModel ####
template_model = PaleoPopModel$new(duration = 10,
                                   years_per_step = 3,
                                   transition_rate = 1.0,
                                   populations = 5,
                                   coordinates = array(1:10, c(5, 2)),
                                   dispersal_target_k_threshold = 4,
                                   compact_decomposition = list(matrix = (diag(5)*0.8 + 0.2),
                                                                map = array(1:5, c(5, 5))),
                                   harvest = TRUE,
                                   harvest_g = 0.4)
template_model$get_attributes()
# Nest template (fixed attributes) within another model (sample attributes)
nested_model <- PaleoPopModel$new(template = template_model)
dispersal_matrix <- (diag(5)*-0.1 + 0.1)
dispersal_data <- which(dispersal_matrix > 0, arr.ind = TRUE)
dispersal_data <- data.frame(target_pop = dispersal_data[, 1], source_pop = dispersal_data[, 2],
                             emigrant_row = dispersal_data[, 1], immigrant_row = dispersal_data[, 2],
                             dispersal_rate = dispersal_matrix[dispersal_data])
nested_model$set_sample_attributes(standard_deviation = 0.22,
                                   initial_abundances = seq(10, 30, by = 5),
                                   growth_rate_max = 1.3,
                                   local_threshold = 3,
                                   carrying_capacities = matrix(seq(50, 32, by = -2),
                                                                nrow = 5, ncol = 10, byrow = TRUE),
                                   dispersal_data = dispersal_data,
                                   harvest_max = 0.3,
                                   harvest_z = 1.4,
                                   harvest_max_n = 50,
                                   human_densities = array((1:50)/50, c(5, 10)))
nested_model$get_attributes()
# Check default alias
nested_model$attribute_aliases
nested_model$get_attributes("density")
# Check consistency and completeness (new and altered complex attributes)
nested_model$dispersal_data <- dispersal_data[, 1:4]
nested_model$is_consistent("dispersal_data")
nested_model$dispersal_data <- dispersal_data
nested_model$dispersal_data[1, 1] <- 0
nested_model$is_consistent("dispersal_data")
nested_model$dispersal_data <- dispersal_data
nested_model$dispersal_data[2, 2] <- 6
nested_model$is_consistent("dispersal_data")
nested_model$is_complete()
nested_model$incomplete_attributes()
nested_model$dispersal_data <- NULL
nested_model$is_complete()
compact_decomposition = list(matrix = (diag(5)*0.8 + 0.2), map = array(1:5, c(5, 5)))
nested_model$compact_decomposition$matrix <- NULL
nested_model$is_consistent("compact_decomposition")
nested_model$incomplete_attributes()
nested_model$compact_decomposition <- compact_decomposition
nested_model$compact_decomposition$map <- NULL
nested_model$is_consistent("compact_decomposition")
nested_model$incomplete_attributes()
nested_model$compact_decomposition <- compact_decomposition
nested_model$compact_decomposition$matrix[1,1] <- -1
nested_model$incomplete_attributes()
nested_model$compact_decomposition$matrix <- compact_decomposition$matrix[, -1]
nested_model$is_consistent("compact_decomposition")
nested_model$compact_decomposition <- compact_decomposition
nested_model$compact_decomposition$map <- compact_decomposition$map[, -1]
nested_model$is_consistent("compact_decomposition")
nested_model$compact_decomposition <- compact_decomposition
nested_model$compact_decomposition$matrix <- compact_decomposition$matrix[-1,]
nested_model$is_consistent("compact_decomposition")
nested_model$is_complete()
nested_model$incomplete_attributes()
nested_model$compact_decomposition <- NULL
nested_model$is_complete()


#### GenerativeModel ####
TEST_DIRECTORY <- "E:\\paleopop\\test_inputs"
TestGenerativeModel <- R6::R6Class("TestGenerativeModel", inherit = GenerativeModel,
                                   public = list(),
                                   private = list(.model_attributes = c("attr1", "attr2", "attr3"), .active_attributes = c("attribute_aliases", "attr1", "attr2", "attr3"), .attr1 = NULL, .attr2 = NULL, .attr3 = NULL),
                                   active = list(attr1 = function(value) {if (missing(value)) { private$.attr1 } else { private$.attr1 <- value }},
                                                 attr2 = function(value) {if (missing(value)) { private$.attr2 } else { private$.attr2 <- value }},
                                                 attr3 = function(value) {if (missing(value)) { private$.attr3 } else { private$.attr3 <- value }})
)
generative_model <- TestGenerativeModel$new(generative_template = GenerativeTemplate$new(),
                                            params = list(attr1 = 40, attr2 = 5))
# File reading via templates
generative_model$add_file_template("attr3", path_template = file.path(TEST_DIRECTORY, "Test_NB%s_cut_%s.RData"),
                                   path_params = c("attr1", "attr2"), file_type = "RDS")
generative_model$file_templates
generative_model$read_file("attr3")[621:630,1:10]
print(generative_model$read_file("none1"))
generative_model$add_file_template("attr3", path_template = file.path(TEST_DIRECTORY, "Test_NB%s_cut_%s.csv"),
                                   path_params = c("attr1", "attr2"), file_type = "CSV")
generative_model$read_file("attr3")[1:10, 1:5]
generative_model$add_file_template("attr3", path_template = file.path(TEST_DIRECTORY, "Test_%s_%s_no_such_file.csv"),
                                   path_params = c("attr1","attr2"), file_type = "CSV")
generative_model$read_file("attr3")
generative_model$error_messages
# Missing path param
generative_model$attr1 <- NULL
generative_model$read_file("attr3")
generative_model$warning_messages
# Generative requirements
generative_model$add_generative_requirements(list(attr3 = "file", dummy1 = "file"), attr3 = "function", dummy2 = "function")
generative_model$generative_requirements
generative_model$generative_requirements_satisfied()
# Function execution via templates
generative_model$add_function_template("attr3", source_path = file.path(TEST_DIRECTORY, "test_function.R"),
                                       call_template = "test_function(param1 = %s, param2 = %s)",
                                       call_params = c("attr1", "attr2"))
generative_model$function_templates
generative_model$generative_requirements_satisfied()
generative_model$attr1 <- 45
generative_model$run_function("attr3")
print(generative_model$run_function("none2"))
# Aliases
generative_model$attribute_aliases <- list(a1 = "attr1", a2 = "attr2", a3 = "attr3")
generative_model$get_attributes(c("a1", "a2", "a3"))
generative_model$attr3
# Cloning
generative_clone <- generative_model$new_clone(params = list(attr1 = 50, attr2 = 6))
generative_clone$attribute_aliases
generative_clone$generative_requirements
generative_clone$run_function("attr3")
# Generation
generative_model$generate()
generative_model$inputs <- c("attr1", "attr2")
generative_model$outputs <- c("attr3")
generative_model$generate()
generative_model$generate(input_values = list(attr1 = 5, attr2 = 7)) # empty list
# Generation via aliases
generative_model$generate(input_values = list(a1 = 6))
generative_model$generate(input_values = list(a1 = 6, a2 = 8)) # empty list


#### NicheCarryingCapacityModel ####
K_CUTS_DIR <- "E:\\paleopop\\Mammoth\\k_cuts"
niche_k_model <- NicheCarryingCapacityModel$new()
niche_k_model$add_generative_requirements(carrying_capacities = "file")
niche_k_model$add_file_template("carrying_capacities",
                                path_template = file.path(K_CUTS_DIR, "NB%s_K_cut_%s.RData"),
                                path_params = c("niche_breadth", "niche_cuts"),
                                file_type = "RDS")
# Cloning and dynamic generation
niche_k_clone <- niche_k_model$new_clone(niche_breadth = 80, niche_cuts = 1, density = 100)
niche_k_clone$carrying_capacities[1:10, 1:10]
dim(niche_k_clone$carrying_capacities)
niche_k_clone$initial_abundances[1:10]
length(niche_k_clone$initial_abundances)
# Generation
niche_k_model$generate()
generated <- niche_k_model$generate(input_values = list(niche_breadth = 80, niche_cuts = 1, density = 100)) # empty list
names(generated)
generated$carrying_capacities[1:10, 1:10]
dim(generated$carrying_capacities)
generated$initial_abundances[1:10]
length(generated$initial_abundances)
# Generation via aliases
niche_k_model$attribute_aliases
niche_k_model$generate()
generated <- niche_k_model$generate(input_values = list(breadth = 80, cuts = 1, max_n = 100)) # empty list
generated$carrying_capacities[1:10, 1:10]


#### DispersalModel ####
DATA_DIR <- "E:\\paleopop\\Mammoth\\data"
dispersal_model <- DispersalModel$new()
# Check requirements satisfied and try to calculate distance data and dispersals
dispersal_model$generative_requirements <- list(dispersal_data = "default")
dispersal_model$generative_requirements_satisfied()
dispersal_model$calculate_distance_matrix()
dispersal_model$calculate_distance_data()
dispersal_model$calculate_dispersals()
dispersal_model$coordinates <- file.path(DATA_DIR, "studysite_xy_modified3_mini.csv")
head(dispersal_model$coordinates)
nrow(dispersal_model$coordinates)
dispersal_model$set_distance_classes(minimum = 100, maximum = 500, interval = 20)
dispersal_model$distance_classes
dispersal_model$dispersal_function_data <- file.path(DATA_DIR, "mammoth_dispersal_function_data.csv")
dispersal_model$dispersal_function_data
# Check requirements satisfied again and try to calculate dispersals
dispersal_model$generative_requirements_satisfied()
dispersal_model$calculate_dispersals()
# Calculate distance matrix
distance_matrix <- dispersal_model$calculate_distance_matrix()
distance_matrix[1:10,1:5]
# Calculate distance data
dispersal_model$calculate_distance_data()
head(dispersal_model$distance_data, 10)
dim(dispersal_model$distance_data)
# Consistency check/protection
dispersal_model$calculate_distance_data(distance_matrix = distance_matrix[1:99, 1:99])
dispersal_model$coordinates <- NULL
dispersal_model$coordinates <- file.path(DATA_DIR, "ice_mini_coordinates_id.csv")
dispersal_model$distance_classes <- NULL
dispersal_model$distance_classes <- seq(100, 500, 20)
# Check requirements satisfied again and try to calculate dispersals
dispersal_model$generative_requirements_satisfied()
dispersal_model$calculate_dispersals()
dispersal_model$calculate_distance_data()
# Re-calculate distance data using pre-calculated distance matrix
distance_data <- dispersal_model$distance_data
distance_matrix <- geosphere::distm(dispersal_model$coordinates, dispersal_model$coordinates, fun = geosphere::distGeo)/1000
dispersal_model$calculate_distance_data(distance_matrix = distance_matrix)
all(distance_data == dispersal_model$distance_data)
# Set sample parameters
sample_data <- read.csv(file = file.path(DATA_DIR, "masterDF_80.csv"))[1:10, c("DISP1","DISP2")]
names(sample_data) <- c("dispersal_proportion","dispersal_max_distance")
sample_data[1,]
dispersal_model$set_attributes(params = as.list(sample_data[1,]))
dispersal_model$get_attributes(c("dispersal_proportion", "dispersal_function_data", "dispersal_index", "dispersal_breadth", "dispersal_max_distance"))
# Check default aliases
dispersal_model$get_attribute_aliases(c("dispersal_proportion", "dispersal_breadth", "dispersal_max_distance"))
dispersal_model$get_attributes(c("proportion", "dispersal_b", "r"))
# Calculated dispersals
dispersal_model$calculate_dispersals()
head(dispersal_model$dispersal_data, 10)
dim(dispersal_model$dispersal_data)
min(dispersal_model$dispersal_data$dispersal_rate)
max(dispersal_model$dispersal_data[, 3:4])
compact_matrix <- array(0, c(max(dispersal_model$dispersal_data[, 3:4]), 100))
compact_matrix[as.matrix(dispersal_model$dispersal_data[, c(3,2)])] <- dispersal_model$dispersal_data$dispersal_rate
unique(round(colSums(compact_matrix), 10))
round(dispersal_model$dispersal_proportion, 10)
# Add proportion multiplier
dispersal_model$proportion_multiplier <- 0.5
dispersal_model$dispersal_proportion
# Calculated dispersal matrix
dispersal_model$calculate_dispersals(type = "matrix")
dispersal_model$dispersal_matrix[1:10, 1:5]
unique(round(colSums(dispersal_model$dispersal_matrix), 10))
# Re-calculate with rounding
dispersal_model$decimals <- 3
dispersal_model$calculate_dispersals()
head(dispersal_model$dispersal_data, 10)
dispersal_model$calculate_dispersals(type = "matrix")
dispersal_model$dispersal_matrix[1:10, 1:5]
dispersal_model$decimals <- NULL
# Cloning and dynamic generation
dispersal_clone <- dispersal_model$new_clone()
dispersal_clone$get_attributes(c("dispersal_proportion", "dispersal_function_data", "dispersal_index", "dispersal_breadth", "dispersal_max_distance"))
dispersal_model$distance_data <- NULL
dispersal_model$new_clone()$get_attributes(c("dispersal_data", "warning_messages"))
dispersal_model$calculate_distance_data()
dispersal_model$new_clone()$get_attributes(c("dispersal_data", "warning_messages"))
# Clone with sample parameters
sample_data[3,]
dispersal_clone <- dispersal_model$new_clone(params = as.list(sample_data[3,]))
dispersal_clone$get_attributes(c("dispersal_proportion", "dispersal_function_data", "dispersal_index", "dispersal_breadth", "dispersal_max_distance"))
# Check default aliases of clone
dispersal_clone$get_attribute_aliases(c("dispersal_proportion", "dispersal_breadth", "dispersal_max_distance"))
dispersal_clone$get_attributes(c("proportion", "dispersal_b", "r"))
# Generate data from clone
dispersal_clone$get_attributes(c("dispersal_data"))$dispersal_data[1:10,]
# Generate matrix from clone
dispersal_model$generative_requirements$dispersal_matrix = "default"
dispersal_model$new_clone()$get_attributes(c("dispersal_matrix", "warning_messages"))
generated <- dispersal_model$new_clone(params = as.list(sample_data[3,]))$get_attributes(c("dispersal_matrix"))
generated$dispersal_matrix[1:10, 1:5]
# Generation
dispersal_model$generate()
generated <- dispersal_model$generate(input_values = as.list(sample_data[3,]))
names(generated)
generated$dispersal_data[1:10,]
dim(generated$dispersal_data)
# Generation via aliases
names(sample_data) <- c("dispersal_p", "dispersal_r")
sample_data[4,]
dispersal_model$dispersal_function_data
dispersal_clone <- dispersal_model$new_clone(params = as.list(sample_data[4,]))
dispersal_clone$get_attributes(c("dispersal_p", "dispersal_b", "dispersal_r", "dispersal_index"))
dispersal_clone$dispersal_data[1:10,]
names(dispersal_model$dispersal_function_data) <- c("b", "r")
dispersal_model$dispersal_function_data
sample_data[5,]
dispersal_clone <- dispersal_model$new_clone(params = as.list(sample_data[5,]))
dispersal_clone$get_attributes(c("p", "b", "r", "dispersal_index"))
dispersal_clone$dispersal_data[1:10,]
generated <- dispersal_model$generate(input_values = as.list(sample_data[5,]))
generated$dispersal_data[1:10,]


#### region_subset function ####
DATA_DIR <- "E:\\paleopop\\Mammoth\\data"
region_subset()
subset_matrix <- region_subset(orig_coords = file.path(DATA_DIR, "center.coord.40k.shifted.reduced.csv"),
                               orig_matrix = file.path(DATA_DIR, "full.K.mean.for.Damien.RDS"),
                               subset_coords = file.path(DATA_DIR, "studysite_xy_modified3.csv"))
dim(subset_matrix); subset_matrix[1:10,1:5]


#### HumanDensityModel ####
DATA_DIR <- "E:\\paleopop\\Mammoth\\data"
# Original global human density data
orig_coords <- file.path(DATA_DIR, "center.coord.40k.shifted.reduced.csv")
orig_hd_mean <- file.path(DATA_DIR, "full.K.mean.for.Damien.RDS")
orig_hd_sd <- file.path(DATA_DIR, "full.K.SD.for.Damien.RDS")
orig_hd_mask <- file.path(DATA_DIR, "inhabitable.mask.for.Damien.RDS")
# Mammoth study area human density data
mamm_coords <- file.path(DATA_DIR, "studysite_xy_modified3.csv")
mamm_hd_mean <- region_subset(orig_coords = orig_coords, orig_matrix = orig_hd_mean, subset_coords = mamm_coords)
dim(mamm_hd_mean); mamm_hd_mean[1:10, 1:5]
mamm_hd_sd <- region_subset(orig_coords=orig_coords, orig_matrix = orig_hd_sd, subset_coords = mamm_coords)
dim(mamm_hd_sd); mamm_hd_sd[1:10, 1:5]
mamm_hd_mask <- region_subset(orig_coords=orig_coords, orig_matrix = orig_hd_mask, subset_coords = mamm_coords)
dim(mamm_hd_mask); mamm_hd_mask[1:10, 1:5]
# Add 80-generation time step burn-in to each study area human density data matrix
mamm_hd_mean <- cbind(array(mamm_hd_mean[, 1], c(nrow(mamm_hd_mean), 80)), mamm_hd_mean)
mamm_hd_sd <- cbind(array(mamm_hd_sd[, 1], c(nrow(mamm_hd_sd), 80)), mamm_hd_sd)
mamm_hd_mask <- cbind(array(mamm_hd_mask[, 1],c(nrow(mamm_hd_mask), 80)), mamm_hd_mask)
# Additional land-sea mask
land_sea_mask <- read.csv(file = file.path(DATA_DIR, "land_sea_mask.csv"), header = TRUE, sep = ",")
land_sea_mask <- as.matrix(cbind(array(land_sea_mask[, 4], c(nrow(land_sea_mask), 80)), land_sea_mask[, 4:844])) # burn-in
land_sea_mask[is.na(land_sea_mask)] <- 0; dimnames(land_sea_mask) <- NULL
dim(land_sea_mask); land_sea_mask[1:10, 1:10]
# Build Human Density Model
hd_model <- HumanDensityModel$new()
# Check generative requirements
hd_model$generative_requirements
hd_model$generative_requirements_satisfied()
# Attempt to generate sampling parameters without carrying capacity mean & sd
hd_model$calculate_sampling_parameters(mean_upper = 300, max_upper = 500)
# Rebuild with carrying capacity mean & sd and occupancy_mask
hd_model <- HumanDensityModel$new(carrying_capacity_mean = mamm_hd_mean,
                                  carrying_capacity_sd = mamm_hd_sd,
                                  human_occupancy_mask = mamm_hd_mask*land_sea_mask)
# Check generative requirements again
hd_model$generative_requirements_satisfied()
# Generate sampling parameters / distribution data
hd_model$calculate_sampling_parameters(mean_upper = 300, max_upper = 500)
head(hd_model$distrib_data$flat); dim(hd_model$distrib_data$flat)
head(hd_model$distrib_data$triangular); dim(hd_model$distrib_data$triangular)
max(hd_model$distrib_data$triangular$mean)
max(hd_model$distrib_data$triangular$max)
# Attempt to calculate human densities
hd_model$calculate_human_density()
# Set invalid sample and attempt to calculate human densities
hd_model$human_density_sample <- "dummy"
hd_model$calculate_human_density()
hd_model$human_density_sample <- -0.1
hd_model$calculate_human_density()
hd_model$human_density_sample <- 1.1
hd_model$calculate_human_density()
# Set valid sample and attempt to calculate human densities
hd_model$human_density_sample <- 0.5
hd_model$generative_requirements_satisfied()
hd_model$calculate_human_density()
hd_model$human_densities[1:10, 1:5]
# Rounding
hd_model$decimals <- 4
hd_model$calculate_human_density()
hd_model$human_densities[1:10, 1:5]
dim(hd_model$human_densities)
# Check default aliases
hd_model$get_attribute_aliases("human_density_sample")
hd_model$get_attributes(c("hd_sample", "hd_p"))
# Generation via cloning
hd_model$new_clone()$get_attributes(c("human_densities", "warning_messages"))
hd_model$new_clone(human_density_sample = 1.1)$get_attributes(c("human_densities", "warning_messages"))
generated <- hd_model$new_clone(human_density_sample = 0.7)$get_attributes("human_densities")
generated$human_densities[1:10, 1:5]
# Generation
hd_model$generate()
generated <- hd_model$generate(input_values = list(human_density_sample = 0.7))
generated$human_densities[1:10, 1:5]
# Generation via aliases
generated <- hd_model$generate(input_values = list(hd_sample = 0.8))
generated$human_densities[1:10, 1:5]


#### CorrelationModel ####
DATA_DIR <- "E:\\paleopop\\Mammoth\\data"
correlation_model <- CorrelationModel$new()
# Check coordinates and parameters present before calculations
correlation_model$calculate_distance_matrix()
correlation_model$calculate_correlations()
# Set coordinates
correlation_model$coordinates <- file.path(DATA_DIR, "studysite_xy_modified3_mini.csv")
head(correlation_model$coordinates)
nrow(correlation_model$coordinates)
# Calculate distance matrix
distance_matrix <- correlation_model$calculate_distance_matrix()
distance_matrix[1:10,1:5]
# Invalid correlation function parameters
correlation_model$correlation_amplitude <- -1
correlation_model$correlation_amplitude <- 2
correlation_model$correlation_amplitude <- "0"
correlation_model$correlation_breadth <- -1
correlation_model$correlation_breadth <- "0"
correlation_model$correlation_amplitude <- NULL # ok
correlation_model$correlation_breadth <- NULL # ok
# Calculate correlations
correlation_model$calculate_correlations(correlation_amplitude = 0.99, correlation_breadth = 8.0)
dim(correlation_model$correlation_matrix)
correlation_model$correlation_matrix[1:10,1:5]
# Check default aliases
correlation_model$get_attribute_aliases(c("correlation_amplitude", "correlation_breadth"))
correlation_model$get_attributes(c("amplitude", "correlation_b", "a"))
# Consistency checks/protections
correlation_model$calculate_correlations(distance_matrix = distance_matrix[1:99, 1:99])
correlation_model$calculate_correlations()
# Calculate Cholesky decomposition
correlation_model$compact_only <- FALSE
correlation_model$calculate_cholesky_decomposition()
correlation_model$t_decomposition_matrix[1:10,1:5]
# Cholesky decomposition cannot be calculated
correlation_model$correlation_matrix[1,1] <- -100
correlation_model$calculate_cholesky_decomposition()
correlation_model$t_decomposition_matrix
correlation_model$generate_correlated_normal_deviates()
# Calculate_compact_decomposition
correlation_model$correlation_matrix <- NULL
correlation_model$compact_only <- TRUE
correlation_model$calculate_compact_decomposition()
correlation_model$correlation_matrix
correlation_model$t_decomposition_matrix
correlation_model$t_decomposition_compact_matrix[,1:5]
dim(correlation_model$t_decomposition_compact_matrix)
correlation_model$t_decomposition_compact_map[,1:5]
dim(correlation_model$t_decomposition_compact_map)
# Disabled coordinates and function parameters to avoid inconsistency
correlation_model$coordinates <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
correlation_model$correlation_amplitude <- 0.5
correlation_model$correlation_breadth <- 0.1
# Get compact decomposition (as a list)
compact_decomposition <- correlation_model$get_compact_decomposition()
names(compact_decomposition)
all(compact_decomposition$matrix == correlation_model$t_decomposition_compact_matrix)
all(compact_decomposition$map == correlation_model$t_decomposition_compact_map, na.rm = TRUE)
# Generate correlated normal deviates
gen1 <- correlation_model$generate_correlated_normal_deviates(random_seed = 1234); gen1
gen2 <- correlation_model$generate_correlated_normal_deviates(random_seed = 1234)
all(gen1 == gen2)


#### LatinHypercubeSampler ####
TEST_DIRECTORY <- "E:\\paleopop\\test_saves"
lhs_generator <- LatinHypercubeSampler$new(parameter_names = c("param_class", "param_uniform", "param_norm"))
lhs_generator$parameter_names
lhs_generator$parameter_distributions
lhs_generator$generate_samples()
lhs_generator$set_class_parameter("param_class", c("a", "b", "c", "d", "e"))
lhs_generator$set_uniform_parameter("param_uniform", upper = 2)
lhs_generator$set_normal_parameter("param_norm", mean = 100, sd = 10, decimals = 0)
lhs_generator$set_lognormal_parameter("param_lognorm", decimals = 4)
lhs_generator$set_triangular_parameter("param_triang", lower = 2, upper = 6, decimals = 2)
lhs_generator$parameter_names
lhs_generator$parameter_distributions
lhs_generator$generate_samples()
lhs_generator$sample_data
lhs_generator$generate_samples(number = 100000)
hist(lhs_generator$sample_data$param_uniform)
hist(lhs_generator$sample_data$param_norm)
hist(lhs_generator$sample_data$param_lognorm)
hist(lhs_generator$sample_data$param_triang)
lhs_generator$save_samples(TEST_DIRECTORY)
lhs_generator$save_samples(TEST_DIRECTORY, file_type = "csv")


#### PopulationSimulator ####
TEST_DIRECTORY <- "E:\\paleopop\\test_inputs"
# Default initialization
pop_simulator <- PopulationSimulator$new()
pop_simulator$simulation_function
# Attempt to run without simulator function
pop_simulator$simulation_function <- NULL
pop_simulator$run()
# Attempted run without population model
pop_simulator$simulation_function <- "paleopop_simulator"
pop_simulator$run()
# Attempted to set invalid simulation functions
pop_simulator$simulation_function <- file.path(TEST_DIRECTORY, "test_not_a_function.R")
pop_simulator$simulation_function <- file.path(TEST_DIRECTORY, "no_such_file.R")
pop_simulator$simulation_function <- "dummy"
# Attempt to set invalid model
pop_simulator$population_model <- "dummy"
# Set model
pop_simulator$population_model <- PopulationModel$new(duration = 10,
                                                      populations = 5,
                                                      coordinates = array(1:10, c(5, 2)),
                                                      initial_abundances = seq(10, 30, by = 5),
                                                      carrying_capacities = matrix(seq(50, 32, by = -2), nrow = 5, ncol = 10, byrow = TRUE),
                                                      dispersal_matrix = (diag(5)*-0.1 + 0.1),
                                                      correlation_matrix = (diag(5)*0.8 + 0.2),
                                                      human_densities = array((1:50)/50, c(5, 10)))
# Attempt to run with incomplete model
pop_simulator$simulation_function <- "paleopop_simulator"
pop_simulator$population_model$duration <- NULL
pop_simulator$run()
pop_simulator$population_model$duration <- 10
# Attempt to run with invalid simulator functions
pop_simulator$simulation_function <- file.path(TEST_DIRECTORY, "test_function.R")
pop_simulator$simulation_function
pop_simulator$run()
pop_simulator$results
pop_simulator$simulation_function <- paste
pop_simulator$simulation_function
pop_simulator$run()
pop_simulator$results
pop_simulator$simulation_function <- "mean"
pop_simulator$simulation_function
pop_simulator$run()
pop_simulator$results
# Set simulator function from a file (stores function definition)
pop_simulator$simulation_function <- file.path(TEST_DIRECTORY, "test_simulator.R")
pop_simulator$simulation_function
# Run test simulator (returns model attributes -> results)
pop_simulator$run()
pop_simulator$results
# Set simulator function to base function "class" directly and via character string
pop_simulator$simulation_function <- class
pop_simulator$simulation_function
pop_simulator$run()
pop_simulator$results
pop_simulator$simulation_function <- "class"
pop_simulator$simulation_function
pop_simulator$run()
pop_simulator$results
# New cloning retains the simulator function only
simulator_clone <- pop_simulator$new_clone()
simulator_clone$simulation_function
simulator_clone$population_model
simulator_clone$results
# Run the paleopop simulator
pop_simulator$simulation_function <- "paleopop_simulator"
pop_simulator$population_model <- PaleoPopModel$new(params = pop_simulator$population_model$get_attributes())
pop_simulator$population_model$set_attributes(years_per_step = 3,
                                              transition_rate = 1.0,
                                              standard_deviation = 0.22,
                                              growth_rate_max = 1.3,
                                              harvest = FALSE,
                                              results_selection = c("abundance"))
pop_simulator$run()
pop_simulator$results


#### GenericResultsManager ####
TEST_RESULTS_DIRECTORY <- "E:\\paleopop\\test_results"
TEST_SAVE_DIRECTORY <- "E:\\paleopop\\test_saves"
# Default initialization
generic_manager <- GenericResultsManager$new(results_dir = TEST_RESULTS_DIRECTORY,
                                             results_filename_attributes = c("test"))
# Generate substitution messages and filenames without sample data
generic_manager$get_message_sample("testing %s message", 1)
generic_manager$get_results_filename(2)
generic_manager$results_filename_attributes <- c("pre", "param1", "param2", "param3", "post")
generic_manager$get_message_sample("testing %s message", 1)
generic_manager$get_results_filename(2)
# Set sample attributes to invalid then valid attributes
generic_manager$sample_data <- "dummy"
generic_manager$sample_data <- NULL
generic_manager$sample_data <- data.frame(param1 = 1:3, param2 = 4:6, param3 = c(8,10,12))
generic_manager$niche_k_model <- "dummy"
generic_manager$human_density_model <- "dummy"
generic_manager$niche_k_model <- NULL
generic_manager$human_density_model <- NULL
generic_manager$niche_k_model <- NicheCarryingCapacityModel$new()
generic_manager$human_density_model <- HumanDensityModel$new()
# Generate substitution messages and filenames without sample data
generic_manager$results_filename_attributes <- c("test")
generic_manager$get_message_sample("testing %s message", 1)
generic_manager$get_results_filename(2)
generic_manager$results_filename_attributes <- c("test", "param1")
generic_manager$get_message_sample("testing %s message", 1)
generic_manager$get_results_filename(2)
generic_manager$results_filename_attributes <- c("param2", "post")
generic_manager$get_message_sample("testing %s message", 1)
generic_manager$get_results_filename(2)
generic_manager$results_filename_attributes <- c("test", "param1", "param2", "param3", "post")
generic_manager$get_message_sample("testing %s message", 1)
generic_manager$get_results_filename(2)
# Test save/read
generic_manager$save_to_rds(TEST_SAVE_DIRECTORY)
read_manager <- GenericResultsManager$new()$read_from_rds(TEST_SAVE_DIRECTORY)
read_manager
read_manager$niche_k_model
generic_manager$save_to_rds()
generic_manager$results_dir <- NULL
generic_manager$save_to_rds() # fails


#### MultiSimulationManager ####
TEST_RESULTS_DIRECTORY <- "E:\\paleopop\\test_results"
TEST_INPUTS_DIRECTORY <- "E:\\paleopop\\test_inputs"
# Default initialization
multi_sim_manager <- MultiSimulationManager$new()
# Attempt to set invalid attributes
multi_sim_manager$population_simulator <- "dummy"
multi_sim_manager$model_template <- "dummy"
multi_sim_manager$nested_model <- "dummy"
multi_sim_manager$dispersal_model <- "dummy"
# Attempt to run without model or samples
multi_sim_manager$run()
multi_sim_manager$model_template <- PopulationModel$new()
multi_sim_manager$sample_data <- data.frame() # empty
multi_sim_manager$run()
# Attempt to run without population simulator
multi_sim_manager$sample_data <- data.frame(sample = 1:3) # empty
multi_sim_manager$population_simulator <- NULL
multi_sim_manager$run()
# Attempt to run without output directory
multi_sim_manager$population_simulator <- PopulationSimulator$new()
multi_sim_manager$run()
# Attempt to run with an invalid results directory
multi_sim_manager$results_dir <- "G:\\no_such_drive\\"
multi_sim_manager$run()
# Attempt to run with an incomplete model
multi_sim_manager$results_dir <- file.path(TEST_RESULTS_DIRECTORY, "test1")
multi_sim_manager$run()
# Attempt to run with a minimally complete model with simple simulation function (lists attributes)
multi_sim_manager$population_simulator$simulation_function <- file.path(TEST_INPUTS_DIRECTORY, "test_simulator.R")
multi_sim_manager$population_simulator$simulation_function
multi_sim_manager$model_template$set_attributes(duration = 10,
                                                populations = 5,
                                                initial_abundances = seq(10, 30, by = 5),
                                                carrying_capacities = matrix(seq(50, 32, by = -2), nrow = 5, ncol = 10, byrow = TRUE))
multi_sim_manager$run()
# Check results
for (i in 1:3) {
  print(sprintf("Results %s:", i))
  print(readRDS(file = file.path(multi_sim_manager$results_dir, sprintf("%s.RDS", multi_sim_manager$get_results_filename(i)))))
}
# Set sample attributes for duration and generators
multi_sim_manager$sample_data$duration <- c(8,10,12)
multi_sim_manager$sample_data$niche_cuts <- 1:3
multi_sim_manager$sample_data$dispersal_index <- 1:3
multi_sim_manager$sample_data$human_density_sample <- 1:3
multi_sim_manager$sample_data
# Add simple (function) niche k, human density, and dispersal models generators
multi_sim_manager$niche_k_model <- NicheCarryingCapacityModel$new(generative_requirements = list(carrying_capacities = "function"),
                                                                  inputs = c("niche_cuts"),
                                                                  outputs = c("initial_abundances", "carrying_capacities"))
multi_sim_manager$niche_k_model$function_templates <- list(carrying_capacities = list(f = function(cuts) return(array(cuts*10, c(5, 6+2*cuts))),
                                                                                      call_template = "f(cuts = %s)",
                                                                                      call_params = c("niche_cuts")))
#multi_sim_manager$niche_k_model$new_clone(niche_cuts = 1)$carrying_capacities
multi_sim_manager$human_density_model <- HumanDensityModel$new(generative_requirements = list(human_densities = "function"),
                                                    inputs = c("human_density_sample"),
                                                    outputs = c("human_densities"))
multi_sim_manager$human_density_model$function_templates <- list(human_densities = list(f = function(i) return(array((1:(5*(6+2*i)))/(10*(6+2*i)), c(5, 6+2*i))),
                                                                             call_template = "f(i = %s)",
                                                                             call_params = c("human_density_sample")))
#multi_sim_manager$human_density_model$new_clone(human_density_sample = 1)$human_densities
multi_sim_manager$dispersal_model <- DispersalModel$new(generative_requirements = list(dispersal_matrix = "function"),
                                                        inputs = c("dispersal_index"),
                                                        outputs = c("dispersal_matrix"))
multi_sim_manager$dispersal_model$function_templates <- list(dispersal_matrix = list(f = function(i) return((diag(5)*-0.1*i + 0.1*i)),
                                                                                     call_template = "f(i = %s)",
                                                                                     call_params = c("dispersal_index")))
#multi_sim_manager$dispersal_model$new_clone(dispersal_index = 1)$dispersal_matrix
# Run the multi-simulator with these generators in parallel
# multi_sim_manager$population_simulator$simulation_function = function (model) { return(model$get_attributes()) }
# multi_sim_manager$population_simulator$simulation_function
multi_sim_manager$results_dir <- file.path(TEST_RESULTS_DIRECTORY, "test2")
multi_sim_manager$parallel_cores <- 3
multi_sim_manager$run()
# Check results
for (i in 1:3) {
  print(sprintf("Results %s:", i))
  print(readRDS(file = file.path(multi_sim_manager$results_dir, sprintf("%s.RDS", multi_sim_manager$get_results_filename(i)))))
}
# Run again with a directly assigned simulation function for later transformations to metrics
multi_sim_manager$population_simulator$simulation_function = function (model) { return(list(abundance = round(model$carrying_capacities*(0.5 - model$human_densities)))) }
# multi_sim_manager$population_simulator$simulation_function
multi_sim_manager$results_dir <- file.path(TEST_RESULTS_DIRECTORY, "metrics")
multi_sim_manager$parallel_cores <- 3
multi_sim_manager$run()
# Check results
for (i in 1:3) {
  print(sprintf("Results %s:", i))
  print(readRDS(file = file.path(multi_sim_manager$results_dir, sprintf("%s.RDS", multi_sim_manager$get_results_filename(i)))))
}
multi_sim_manager$save_to_rds()


#### Manual ####
# install LaTeX then >initexmf --admin --mkmaps
# CRAN width limit
#' -----10--------20--------30--------40--------50--------60--------70--------80--------90
devtools::build_manual(pkg = ".", path = "E:\\paleopop\\Mammoth")
