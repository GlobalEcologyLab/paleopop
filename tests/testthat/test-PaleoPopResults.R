test_that("occupancy", {
  library(raster)
  # Ring Island example region
  coordinates <- data.frame(x = rep(seq(-178.02, -178.06, -0.01), 5),
                            y = rep(seq(19.02, 19.06, 0.01), each = 5))
  template_raster <- Region$new(coordinates = coordinates)$region_raster # full extent
  sealevel_raster <- template_raster
  template_raster[][c(7:9, 12:14, 17:19)] <- NA # make Ring Island
  sealevel_raster[][c(7:9, 12:14, 17:18)] <- NA
  raster_stack <- raster::stack(x = append(replicate(9, template_raster), sealevel_raster))
  region <- PaleoRegion$new(template_raster = raster_stack)
  
  # Model template
  model_template <- PaleoPopModel$new(
    region = region,
    time_steps = 10,
    years_per_step = 12, # years per generational time-step
    standard_deviation = 0.1,
    growth_rate_max = 0.6,
    harvest = T,
    harvest_max = 0.1, 
    harvest_z = 2, 
    harvest_g = 0.4, 
    harvest_max_n = 50,
    human_density = array(rep(10), c(17,10)), 
    populations = region$region_cells,
    initial_abundance = seq(9000, 0, -1000),
    transition_rate = 1.0,
    carrying_capacity = rep(1000, 17),
    dispersal = (!diag(nrow = 17, ncol = 17))*0.05,
    density_dependence = "logistic",
    dispersal_target_k = 10,
    occupancy_threshold = 1, 
    abundance_threshold = 10,
    results_selection = c("abundance")
  )
  
  # Simulations
  results <- paleopop_simulator(model_template)
  
  # Results
  results_model <- PaleoPopResults$new(results = results, region = region, time_steps = 10)
  
  expect_true(all(!is.na(results_model$occupancy)))
  expect_equal(results_model$occupancy[1:4,6:10], array(rep(0), c(4,5)))
})

test_that("extirpation", {
  library(raster)
  set.seed(567)
  # Ring Island example region
  coordinates <- data.frame(x = rep(seq(-178.02, -178.06, -0.01), 5),
                            y = rep(seq(19.02, 19.06, 0.01), each = 5))
  template_raster <- Region$new(coordinates = coordinates)$region_raster # full extent
  sealevel_raster <- template_raster
  template_raster[][c(7:9, 12:14, 17:19)] <- NA # make Ring Island
  sealevel_raster[][c(7:9, 12:14, 17:18)] <- NA
  raster_stack <- raster::stack(x = append(replicate(9, template_raster), sealevel_raster))
  region <- PaleoRegion$new(template_raster = raster_stack)
  
  # Model template
  model_template <- PaleoPopModel$new(
    region = region,
    time_steps = 10,
    years_per_step = 12, # years per generational time-step
    standard_deviation = 0.1,
    growth_rate_max = 0.6,
    harvest = T,
    harvest_max = 0.1, 
    harvest_z = 2, 
    harvest_g = 0.4, 
    harvest_max_n = 50,
    human_density = array(rep(10), c(17,10)), 
    populations = region$region_cells,
    initial_abundance = seq(9000, 0, -1000),
    transition_rate = 1.0,
    carrying_capacity = rep(1000, 17),
    dispersal = (!diag(nrow = 17, ncol = 17))*0.05,
    density_dependence = "logistic",
    dispersal_target_k = 10,
    occupancy_threshold = 1, 
    abundance_threshold = 10,
    results_selection = c("abundance")
  )
  
  # Simulations
  results <- paleopop_simulator(model_template)
  
  # Results
  results_model <- PaleoPopResults$new(results = results, region = region, time_steps = 10)
  
  expect_true(all(results_model$extirpation>=1))
  expect_true(all(results_model$extirpation<7))
})

test_that("ema", {
  inputs <- list(time_steps = 9, populations = 7, initial_abundance = seq(1000, 7000, 1000), 
                 transition_rate = 1.0, sd = 0.005, carrying_capacity = rep(3000, 7),
                 harvest = T, harvest_max = 0.2, harvest_z = 1.5, harvest_g = 0.4, harvest_max_n = 50,
                 human_density = array(rep(10), c(7,9)), 
                 results_selection = c("abundance"))
  results <- paleopop_simulator(inputs)
  results_model <- PaleoPopResults$new(results = results, time_steps = 9)
  expect_true(all(results_model$abundance>=results_model$ema))
})