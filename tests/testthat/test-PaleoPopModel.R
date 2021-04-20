test_that("consistency and completeness", {
  library(raster)
  region <- PaleoRegion$new(coordinates = array(c(1:4, 4:1), c(7, 2)))
  attribute_aliases <- list(sd = "standard_deviation", pops = "populations")
  nested_model <- PaleoPopModel$new(template = PaleoPopModel$new(),
                                      attribute_aliases = attribute_aliases)
  expect_equal(nested_model$list_consistency(), list(time_steps = NA, years_per_step = T, standard_deviation = NA,
                                                     carrying_capacity = NA, coordinates = NA, random_seed = NA,
                                                     populations = NA, initial_abundance = NA, transition_rate = NA,
                                                     compact_decomposition = NA, density_dependence = NA, 
                                                     growth_rate_max = NA, dispersal_data = NA, 
                                                     dispersal_target_k = NA, harvest = NA, harvest_max = NA,
                                                     harvest_g = NA, harvest_z = NA, harvest_max_n = NA,
                                                     human_density = NA, abundance_threshold = NA,
                                                     occupancy_threshold = NA, results_selection = T))
  expect_null(nested_model$inconsistent_attributes())
  expect_equal(nested_model$inconsistent_attributes(include_nas = TRUE),
               list(inconsistent = NULL, not_available = c("time_steps", "standard_deviation", "carrying_capacity",
                                                           "coordinates", "random_seed", "populations", 
                                                           "initial_abundance", "transition_rate",
                                                           "compact_decomposition", "density_dependence", 
                                                           "growth_rate_max", "dispersal_data", "dispersal_target_k",
                                                           "harvest", "harvest_max", "harvest_g", "harvest_z", 
                                                           "harvest_max_n", "human_density", "abundance_threshold",
                                                           "occupancy_threshold")))
  expect_equal(nested_model$list_completeness(), list(coordinates = NA, random_seed = NA, time_steps = F,
                                                      years_per_step = T, populations = F, initial_abundance = F,
                                                      transition_rate = F, standard_deviation = F, 
                                                      compact_decomposition = NA, carrying_capacity = F,
                                                      density_dependence = NA, growth_rate_max = NA,
                                                      dispersal_data = NA, dispersal_target_k = NA, harvest = F,
                                                      harvest_max = NA, harvest_g = NA, harvest_z = NA, 
                                                      harvest_max_n = NA, human_density = NA, 
                                                      abundance_threshold = NA, 
                                                      occupancy_threshold = NA, results_selection = T))
  expect_true(nested_model$is_consistent())
  expect_equal(nested_model$incomplete_attributes(), c("time_steps", "populations", "initial_abundance",
                                                       "transition_rate", "standard_deviation", 
                                                       "carrying_capacity", "harvest"))
  expect_equal(nested_model$incomplete_attributes(include_nas = TRUE),
               list(incomplete = c("time_steps", "populations", "initial_abundance", "transition_rate",
                                   "standard_deviation", "carrying_capacity", "harvest"), 
                    not_available = c("coordinates", "random_seed", "compact_decomposition", "density_dependence",
                                      "growth_rate_max", "dispersal_data", "dispersal_target_k", "harvest_max",
                                      "harvest_g", "harvest_z", "harvest_max_n", "human_density",
                                      "abundance_threshold", "occupancy_threshold")))
  expect_false(nested_model$is_complete())
  nested_model$set_attributes(time_steps = 9, pops = 8, initial_abundance = seq(1000, 7000, 1000), 
                              transition_rate = 1.0, sd = 0.005, carrying_capacity = rep(3000, 7),
                              harvest = F)
  expect_equal(nested_model$inconsistent_attributes(), c("carrying_capacity", "initial_abundance"))
  expect_equal(nested_model$incomplete_attributes(), c("initial_abundance", "carrying_capacity"))
  nested_model$set_attributes(pops = 7)
  expect_equal(nested_model$inconsistent_attributes(), c("carrying_capacity"))
  expect_false(nested_model$is_consistent())
  nested_model$set_attributes(carrying_capacity = raster::stack(replicate(10, region$region_raster*1000)))
  expect_true(nested_model$is_consistent())
  expect_true(nested_model$is_complete())
})