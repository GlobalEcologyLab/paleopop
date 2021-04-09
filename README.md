
<!-- README.md is generated from README.Rmd. Please edit that file -->

# paleopop

<!-- badges: start -->
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
#>  [2,]  902  945  969 1018 1042  984 1015 1151  842   898
#>  [3,]   19   33   51   67  124  202  272  407  570   898
#>  [4,]  546  773  800  937  854 1033 1014  964 1046  1072
#>  [5,]  232  354  507  664  690  846  987 1043  851   833
#>  [6,]  409  626  726  720  943  876  875  696  808   883
#>  [7,]  716  867  919  990  982 1042 1111  996  885   856
#>  [8,] 1316 1080  976 1028  928  870  722  877 1008   987
#>  [9,] 1074 1209  961  890 1065  836  741  881  803   904
#> [10,]    0    0    0    0    0    0    0    0    0     0
#> [11,]    0    0    0    0    0    0    0    0    0     0
#> [12,]   97  177  336  555  777  919  832  904  857  1020
#> [13,]  311  526  756  942  885  883  930  798  942   890
#> [14,]  142  233  379  591  746  804  947  977 1096  1052
#> [15,]  382  545  827  953  858  893  880  984 1110   954
#> [16,] 1432  958 1036 1072 1037 1129  944  944 1063  1088
#> [17,]  681  935  860  952 1060  993  811  863  983   831
raster::plot(region$raster_from_values(results$abundance[,10]),
             main = "Final abundance", xlab = "Longitude (degrees)", 
             ylab = "Latitude (degrees)", colNA = "blue")
```

<img src="man/figures/README-paleosimulator example-1.png" width="100%" />

A practical example of how to use `paleopop`, with more complex
parameterization, can be found in the vignette.
