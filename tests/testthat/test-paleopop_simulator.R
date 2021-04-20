test_that("minimal inputs", {
  expect_error(paleopop_simulator(list()),
               "Minimal inputs required to run simulation should include: time_steps, populations, initial_abundance, transition_rate, carrying_capacity")
  inputs <- list(time_steps = 9, populations = 7, initial_abundance = seq(1000, 7000, 1000), 
                 transition_rate = 1.0, sd = 0.005, carrying_capacity = rep(3000, 7),
                 harvest = F, random_seed = 321)
  expect_equal(signif(paleopop_simulator(inputs)$abundance[,1], 1), seq(1000, 7000, 1000))
})

test_that("results collection", {
  # Single replicate
  inputs <- list(time_steps = 9, populations = 7, initial_abundance = seq(1000, 7000, 1000), 
                 transition_rate = 1.0, sd = 0.005, carrying_capacity = rep(3000, 7),
                 harvest = T, harvest_max = 0.2, harvest_z = 1.5, harvest_g = 0.4, harvest_max_n = 50,
                 human_density = array(rep(10), c(7,9)), 
                 results_selection = c("abundance", "human_density"), random_seed = 321)
  expect_equal(signif(paleopop_simulator(inputs)$abundance[2:7,1], 1), seq(2000, 7000, 1000))
  expect_equal(signif(paleopop_simulator(inputs)$abundance[2:7, 9], 1), seq(1000, 6000, 1000))
  expect_equal(paleopop_simulator(inputs)$human_density, array(rep(10), c(7,9)))
})