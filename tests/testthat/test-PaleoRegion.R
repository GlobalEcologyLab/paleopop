test_that("temporal mask", {
  coordinates <- data.frame(x = rep(seq(-178.02, -178.06, -0.01), 5),
                            y = rep(seq(19.02, 19.06, 0.01), each = 5))
  template_raster <- Region$new(coordinates = coordinates)$region_raster # full extent
  sealevel_raster <- template_raster
  template_raster[][c(7:9, 12:14, 17:19)] <- NA # make Ring Island
  sealevel_raster[][c(7:9, 12:14, 17:18)] <- NA
  raster_stack <- raster::stack(x = append(replicate(9, template_raster), sealevel_raster))
  region <- PaleoRegion$new(template_raster = raster_stack)
  expect_equal(dim(region$temporal_mask)[2], dim(raster_stack)[3])
  expect_equal(dim(region$temporal_mask)[1], sum(rowSums(!is.na(raster::values(raster_stack)))>0))
  expect_equal(length(raster::values(raster_stack)[!is.na(raster::values(raster_stack))]), sum(rowSums(region$temporal_mask)))
})

test_that("temporal mask raster", {
  coordinates <- data.frame(x = rep(seq(-178.02, -178.06, -0.01), 5),
                            y = rep(seq(19.02, 19.06, 0.01), each = 5))
  template_raster <- Region$new(coordinates = coordinates)$region_raster # full extent
  sealevel_raster <- template_raster
  template_raster[][c(7:9, 12:14, 17:19)] <- NA # make Ring Island
  sealevel_raster[][c(7:9, 12:14, 17:18)] <- NA
  raster_stack <- raster::stack(x = append(replicate(9, template_raster), sealevel_raster))
  region <- PaleoRegion$new(template_raster = raster_stack)
  temp <- region$temporal_mask_raster()
  expect_equal(dim(temp), dim(raster_stack))
  expect_equal(complete.cases(raster::values(temp)), complete.cases(raster::values(raster_stack)))
})