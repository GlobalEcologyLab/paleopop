test_that("subset range", {
  library(sf)
  coordinates <- data.frame(x = rep(seq(-1, -5, -1), 5),
                            y = rep(seq(1, 5, 1), each = 5))
  values <- matrix(seq(1, 25, 1))
  subset <- data.frame(x = rep(seq(0, -4, -0.5), 5), 
                                 y = rep(seq(1, 3, 0.5), each = 9))
  rsub <- region_subset(coordinates, values, subset)
  expect_true(all(sort(unique(dplyr::semi_join(cbind(coordinates, values), subset, by = c("x", "y"))$values))==sort(unique(rsub))))
})

test_that("subset interpolation", {
  library(sf)
  coordinates <- data.frame(x = rep(seq(-1, -5, -1), 5),
                            y = rep(seq(1, 5, 1), each = 5))
  values <- matrix(seq(1, 25, 1))
  subset <- data.frame(x = rep(seq(0, -4, -0.5), 5), 
                       y = rep(seq(1, 3, 0.5), each = 9))
  rsub <- region_subset(coordinates, values, subset)
  expect_length(rsub, nrow(subset))
})