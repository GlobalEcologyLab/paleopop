
<!-- README.md is generated from README.Rmd. Please edit that file -->

# paleopop

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/GlobalEcologyLab/paleopop/branch/master/graph/badge.svg)](https://codecov.io/gh/GlobalEcologyLab/paleopop?branch=master)
[![R-CMD-check](https://github.com/GlobalEcologyLab/paleopop/workflows/R-CMD-check/badge.svg)](https://github.com/GlobalEcologyLab/paleopop/actions)
<!-- badges: end -->

`paleopop` is an extension to `poems`, a spatially-explicit,
process-explicit, pattern-oriented framework for modeling population
dynamics. This extension adds functionality for modeling large
populations at generational time-steps over paleontological time-scales.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("GlobalEcologyLab/paleopop")
```

## Example

One of the major additions in `paleopop` is the `PaleoRegion` R6 class,
which allows for regions that change over time due to ice sheets, sea
level, bathymetry, and so on.

``` r
library(poems)
library(paleopop)
coordinates <- data.frame(x = rep(seq(-178.02, -178.06, -0.01), 5),
                          y = rep(seq(19.02, 19.06, 0.01), each = 5))
template_raster <- Region$new(coordinates = coordinates)$region_raster # full extent
sealevel_raster <- template_raster
template_raster[][c(7:9, 12:14, 17:19)] <- NA # make Ring Island
sealevel_raster[][c(7:9, 12:14, 17:18)] <- NA
raster_stack <- raster::stack(x = append(replicate(9, template_raster), sealevel_raster))
region <- PaleoRegion$new(template_raster = raster_stack)
raster::plot(region$temporal_mask_raster()[[1]], main = "Ring Island (first timestep)",
             xlab = "Longitude (degrees)", ylab = "Latitude (degrees)",
             colNA = "blue", legend = F)
```

<img src="man/figures/README-paleoregion example-1.png" width="100%" />

``` r
raster::plot(region$temporal_mask_raster()[[10]], main = "Ring Island (last timestep)",
             xlab = "Longitude (degrees)", ylab = "Latitude (degrees)",
             colNA = "blue", legend = F)
```

<img src="man/figures/README-paleoregion example-2.png" width="100%" />

`paleopop` also includes the `PaleoPopModel` class for running
simulations over paleo time scales, and the `PaleoPopResults` class for
storing the outputs from the paleo population simulator.

``` r
model_template <- PaleoPopModel$new(
  region = region,
  time_steps = 10,
  years_per_step = 12, # years per generational time-step
  standard_deviation = 0.1,
  growth_rate_max = 0.6,
  harvest = F,
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

results <- paleopop_simulator(model_template)
results # examine
#> $abundance
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#>  [1,]    0    0    0    0    0    0    0    0    0     0
#>  [2,]    0    0    0    0    0    0    0    0    0     0
#>  [3,]   42   68  110  184  274  434  617  817  872  1081
#>  [4,]  578  712  782  991 1046 1028 1094 1029 1032  1220
#>  [5,]   52   98  158  225  347  515  660 1043  902  1026
#>  [6,]  466  615  904  959 1018  957  870  997  974   846
#>  [7,]  575  636  755 1055  991  767  936  869  855   749
#>  [8,] 1109 1058  992 1050 1024 1074 1015 1256 1065  1010
#>  [9,] 1108  974  828  902 1095  993  875  973 1014   960
#> [10,]    0    0    0    0    0    0    0    0    0     0
#> [11,] 1088 1066 1128 1200  941  948 1023 1083 1125  1205
#> [12,]    0    0    0    0    0    0    0    0    0     0
#> [13,]   84  162  273  416  596  635  848  858  725   752
#> [14,]  103  192  306  464  724  769  788  855  744   942
#> [15,] 2575  959  966  920  920  890  983  992  891   801
#> [16,] 1017  925  945  936  943 1040  960 1002 1004  1064
#> [17,] 1315 1131 1183 1186  931  820  994 1128  950   884
raster::plot(region$raster_from_values(results$abundance[,10]),
             main = "Final abundance", xlab = "Longitude (degrees)", 
             ylab = "Latitude (degrees)", colNA = "blue")
```

<img src="man/figures/README-paleosimulator example-1.png" width="100%" />

A practical example of how to use `paleopop`, with more complex
parameterization, can be found in the vignette.
