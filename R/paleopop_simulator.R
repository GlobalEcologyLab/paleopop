#' Runs a customized population model simulation.
#'
#' Simulates a customized population model, optimized for single-generation transitions
#' and large populations, across multiple generations and returns simulation results.
#' Each generational time-step includes:
#' \enumerate{
#'   \item Density dependence and harvest calculations
#'   \item Enviromental stochasticity calculations
#'   \item Generational transition calculations
#'   \item Dispersal calculations
#'   \item Results collection
#' }
#'
#' @param inputs Nested list/object with named elements:
#'   \describe{
#'     \item{\code{duration}}{Number of simulation time steps.}
#'     \item{\code{years_per_step}}{Number of years per time step.}
#'     \item{\code{transition_rate}}{Rate of transition (or fecundity) between generations.}
#'     \item{\code{standard_deviation}}{Standard deviation applied to transition rates.}
#'     \item{\code{populations}}{Number of populations.}
#'     \item{\code{initial_abundances}}{Array of initial abundances for each population.}
#'     \item{\code{growth_rate_max}}{Maximum growth rate.}
#'     \item{\code{local_threshold}}{Abundance threshold (that needs to be exceeded) for each population to persist.}
#'     \item{\code{dispersal_target_k_threshold}}{Target population carrying capacity threshold for density dependent dispersal.}
#'     \item{\code{carrying_capacities}}{Matrix of carrying capacities (\emph{populations} rows by \emph{duration} columns).}
#'     \item{\code{dispersal_data}}{Data frame of non-zero dispersal rates and indices for the construction of compact matrices (columns: \emph{target_pop}, \emph{source_pop}, \emph{emigrant_row}, \emph{immigrant_row}, \emph{dispersal_rate}).}
#'     \item{\code{compact_decomposition}}{List containing a pre-calculated compact transposed (Cholesky) decomposition matrix and a corresponding map of population indices for environmental correlation (list names: \emph{matrix}, \emph{map}).}
#'     \item{\code{harvest}}{Boolean for utilizing harvesting.}
#'     \item{\code{harvest_max}}{Proportion harvested per year (note: annual time scale - not generational).}
#'     \item{\code{harvest_g}}{The \emph{G} parameter in the harvest function.}
#'     \item{\code{harvest_z}}{The \emph{Z} parameter in the harvest function.}
#'     \item{\code{harvest_max_n}}{Maximum density per grid cell.}
#'     \item{\code{human_densities}}{Matrix of human density (fraction) (\emph{populations} rows by \emph{duration} columns).}
#'     \item{\code{results_selection}}{List of results selection from: "abundance", "ema", "extirpation", "harvested", "occupancy".}
#'   }
#' @return Simulation results as a nested list (as selected):
#'   \describe{
#'     \item{\code{abundance}}{Matrix of simulation abundances (\emph{populations} rows by \emph{duration} columns).}
#'     \item{\code{ema}}{Matrix of expected minimum abundances (\emph{populations} rows by \emph{duration} columns).}
#'     \item{\code{extirpation}}{Array of extirpation times for each population.}
#'     \item{\code{harvested}}{Matrix of estimated individuals harvested (\emph{populations} rows by \emph{duration} columns).}
#'     \item{\code{occupancy}}{Array of number of populations occupied at each time-step.}
#'   }
#' @export paleopop_simulator

paleopop_simulator <- function(inputs) {

  ## Unpack inputs and calculate re-usable variables ##

  # General model settings
  duration <- inputs$duration
  years_per_step <- inputs$years_per_step

  # Transition rate and standard deviation
  transition_rate <- inputs$transition_rate
  standard_deviation <- inputs$standard_deviation
  environmental_stochasticity <- (standard_deviation*transition_rate > 0)

  # Population settings
  populations <- inputs$populations
  population_abundances <- array(inputs$initial_abundances, c(1, populations)) # single column matrix for faster dispersal calculations
  growth_rate_max <- inputs$growth_rate_max
  local_threshold <- inputs$local_threshold
  dispersal_target_k_threshold <- inputs$dispersal_target_k_threshold
  carrying_capacities <- inputs$carrying_capacities

  # Dispersal and correlation

  # Create re-usable dispersal structures
  dispersal_present <- !is.null(inputs$dispersal_data)
  if (dispersal_present) {

    # Unpack dispersal data and determine compact matrix dimenstions
    dispersal_data <- inputs$dispersal_data
    dispersal_compact_rows <- max(dispersal_data[, c("emigrant_row", "immigrant_row")])

    # Create compact array of zeros for quick initialization
    dispersal_zero_array <- array(0, c(dispersal_compact_rows, populations))

    # Create a compact matrix of dispersal rates
    dispersal_compact_matrix <- dispersal_zero_array
    dispersal_compact_matrix[as.matrix(dispersal_data[, c("emigrant_row", "source_pop")])] <- dispersal_data$dispersal_rate

    # Does dispersal dependent on target population carrying capacity K?
    dispersal_depends_on_target_pop_k <- (!is.null(dispersal_target_k_threshold) && dispersal_target_k_threshold > 0)
    if (dispersal_depends_on_target_pop_k) {

      # Create a map of compact array indices for mapping dispersers (emigrants) to target populations
      dispersal_target_pop_map <- dispersal_zero_array
      dispersal_target_pop_map[as.matrix(dispersal_data[, c("emigrant_row", "source_pop")])] <- dispersal_data$target_pop
    }

    # Create a map of compact array indices for mapping dispersers (emigrants) to immigrants
    dispersal_compact_indices <- array(1:(dispersal_compact_rows*populations), c(dispersal_compact_rows, populations))
    dispersal_immigrant_map <- dispersal_zero_array
    dispersal_immigrant_map[as.matrix(dispersal_data[, c("emigrant_row", "source_pop")])] <- dispersal_compact_indices[as.matrix(dispersal_data[, c("immigrant_row", "target_pop")])]

    # Create indices for replicating abundances
    dispersal_abundance_rep_indices <- rep(1, dispersal_compact_rows)

    # Release variables from memory
    dispersal_data <- NULL; dispersal_compact_indices <- NULL

  } # dispersal present?

  # Environmental correlation?
  if (environmental_stochasticity) {

    # Flag for environmental correlation
    use_env_correlation <- FALSE

    # Unpack compact decompostion
    if (!is.null(inputs$compact_decomposition)) {
      use_env_correlation <- TRUE
      t_decomposition_compact_matrix <- inputs$compact_decomposition$matrix
      t_decomposition_compact_map <- inputs$compact_decomposition$map
      t_decomposition_compact_rows <- nrow(t_decomposition_compact_matrix)
    }

  } # environmental stochasticity?

  # Human impact (harvest) settings
  harvest <- inputs$harvest
  if (harvest) {
    harvest_max <- inputs$harvest_max
    harvest_g <- inputs$harvest_g
    harvest_z <- inputs$harvest_z
    harvest_max_n <- inputs$harvest_max_n
    human_densities <- inputs$human_densities
  }

  # Results required selection
  results_selection <- inputs$results_selection

  ## Initialization ##

  # Initialize carrying capacity
  carrying_capacity_t_max <- ncol(carrying_capacities)
  if (carrying_capacity_t_max == 1) { # no temporal trend in K
    carrying_capacity <- carrying_capacities[, 1]
  }

  # Intialize results collection list components and other variables
  results <- list()
  if ("abundance" %in% results_selection) {
    results$abundance <- array(0, c(populations, duration))
  }
  if ("ema" %in% results_selection) {
    results$ema <- array(0, c(populations, duration))
    min_abundances <- population_abundances[1,]
  }
  if ("extirpation" %in% results_selection) {
    results$extirpation <- array(NA, populations)
    results$extirpation[which(population_abundances == 0)] <- 0
  }
  if ("harvested" %in% results_selection) {
    results$harvested <- array(0, c(populations, duration))
  }
  if ("occupancy" %in% results_selection) {
    results$occupancy <- array(0, duration)
  }

  # Dispersal tracking (for testing/debug purposes)
  if ("dispersal_tracking" %in% names(inputs) ||
      ("attached" %in% names(inputs) && "dispersal_tracking" %in% names(inputs$attached))) {
    results$emigrants <- array(0, c(populations, duration))
    results$immigrants <- array(0, c(populations, duration))
    dispersal_tracking <- TRUE
  } else {
    dispersal_tracking <- FALSE
  }

  ## Simulation time steps ##
  for (tm in 1:duration) {

    # Set transitions for each population
    transitions <- array(transition_rate, populations)

    # Load carrying capacity for each population for time if temporal trend in K
    if (carrying_capacity_t_max > 1) {
      carrying_capacity <- carrying_capacities[, min(tm, carrying_capacity_t_max)]
    }

    ## Density dependence and harvest calculations ##

    # Selective calculations based on occupied populations and carrying capacity
    occupied_indices <- which(as.logical(carrying_capacity*population_abundances[1,])) # > 0
    occupied_populations <- length(occupied_indices)
    zero_indices <- which(carrying_capacity <= 0 & as.logical(population_abundances))

    # Currently replicating harvest annual calculations (variation of Beverton-Holt model for density dependence)
    if (occupied_populations) {

      # Focus on occupied populations
      selected_carrying_capacity <- carrying_capacity[occupied_indices]
      selected_population_abundances <- population_abundances[occupied_indices]

      # Calculate annual growth rate
      annual_growth_rate_max <- max(growth_rate_max^(1/years_per_step), 1)
      annual_growth_rate <- annual_growth_rate_max*selected_carrying_capacity/
        (annual_growth_rate_max*selected_population_abundances - selected_population_abundances + selected_carrying_capacity)

      # Adjust the growth rate to simulate human impact/harvesting
      if (harvest) {

        # Focus on human presence in occupied cells
        harvest_rate <- array(0, occupied_populations)
        human_presence_indices <- which(as.logical(human_densities[occupied_indices, tm]))
        human_presence_occupied_indices <- occupied_indices[human_presence_indices]

        # Calculate harvest rate
        if (length(human_presence_indices)) {
          prey_density <- population_abundances[human_presence_occupied_indices]/harvest_max_n
          prey_z <- prey_density^harvest_z
          max_functional_response <- (harvest_max*prey_z)/(harvest_g + prey_z) # at max human density
          functional_response <- max_functional_response*human_densities[human_presence_occupied_indices, tm] # at current human density
          harvest_rate[human_presence_indices] <- functional_response/prey_density
        }

        # Subtract harvest rate from annual growth rate
        annual_growth_rate <- pmax(annual_growth_rate - harvest_rate, 0) # annual

        # Store approximate harvested when required
        if ("harvested" %in% results_selection) {
          harvested <- array(0, populations)
          harvested[occupied_indices] <- population_abundances[occupied_indices]*harvest_rate[occupied_indices]
        }

      } # harvest?

      # Convert back to generational time scale
      growth_rate <- annual_growth_rate^years_per_step # generational

      # Calculate density dependent multipliers and apply to transitions
      density_dependence_multipliers <- growth_rate/transition_rate
      transitions[occupied_indices] <- transitions[occupied_indices]*density_dependence_multipliers

      # Set any negative values to zero
      negative_indices <- occupied_indices[which(transitions[occupied_indices] < 0)]
      if (length(negative_indices)) {
        transitions[negative_indices] <- 0
      }

    } # occupied populations?

    ## Enviromental stochasticity calculations ##
    if (occupied_populations && environmental_stochasticity) {

      # Generate correlated normal deviates for each occupied population (as per Burgman, Ferson & Akcakaya, 1993)
      if (use_env_correlation) {
        occupied_correlated_deviates <- .colSums(t_decomposition_compact_matrix[, occupied_indices]*rnorm(populations)[t_decomposition_compact_map[, occupied_indices]],
                                                 m = t_decomposition_compact_rows, n = occupied_populations, na.rm = TRUE)
      } else {
        occupied_correlated_deviates <- rnorm(occupied_populations)
      }

      # Sample from lognormal distribution (as per Burgman, Ferson & Akcakaya, 1993)
      if (length(occupied_indices)) {
        log_common <- log((standard_deviation/transitions[occupied_indices])^2 + 1)
        log_common[which(transitions[occupied_indices] == 0)] <- 0
        transitions[occupied_indices] <- transitions[occupied_indices]*exp(sqrt(log_common)*occupied_correlated_deviates - 0.5*log_common)
      }

      # Set any negative values to zero
      negative_indices <- occupied_indices[which(transitions[occupied_indices] < 0)]
      if (length(negative_indices)) {
        transitions[negative_indices] <- 0
      }

    } # occupied populations & environmental stochasticity?

    ## Generational transition calculations ##

    # Remove populations no longer having carrying capacity (determined above)
    population_abundances[zero_indices] <- 0

    # Sample the next generation's population abundances from a Poisson distribution
    if (occupied_populations) {
      population_abundances[occupied_indices] <- rpois(occupied_populations, transitions[occupied_indices]*population_abundances[occupied_indices])
    }

    ## Dispersal calculations ##
    if (occupied_populations && dispersal_present) {

      # Select dispersals for occupied populations and their non-zero indices
      occupied_dispersals <- dispersal_compact_matrix[, occupied_indices]
      occupied_dispersal_indices <- which(as.logical(occupied_dispersals)) # > 0

      # Modify dispersal rates when dispersal depends on target population carrying capacity K
      if (dispersal_depends_on_target_pop_k) {

        # Calculate the (below-threshold) multipliers
        dd_multipliers <- array(1, populations)
        modify_pop_indices <- which(carrying_capacity < dispersal_target_k_threshold)
        dd_multipliers[modify_pop_indices] <- carrying_capacity[modify_pop_indices]/dispersal_target_k_threshold

        # Select multipliers via target populations for non-zero occupied dispersals
        selected_dd_multipliers <- dd_multipliers[dispersal_target_pop_map[, occupied_indices][occupied_dispersal_indices]]

        # Apply modifying multipliers to dispersals
        modify_indices <- which(selected_dd_multipliers < 1)
        if (length(modify_indices)) {
          modify_dipersal_indices <- occupied_dispersal_indices[modify_indices]
          occupied_dispersals[modify_dipersal_indices] <- occupied_dispersals[modify_dipersal_indices]*selected_dd_multipliers[modify_indices]
          occupied_dispersal_indices <- which(as.logical(occupied_dispersals)) # > 0
        }

        # Release variables from memory
        modify_pop_indices <- NULL; dd_multipliers <- NULL; selected_dd_multipliers <- NULL; modify_indices <- NULL; modify_dipersal_indices <- NULL

      } # dispersal depends on target pop k?

      # Disperser generation via abundance and corresponding dispersal rates
      occupied_abundances <- population_abundances[occupied_indices]
      occupied_abundances_rep <- population_abundances[dispersal_abundance_rep_indices, occupied_indices]
      dispersers <- array(0, c(dispersal_compact_rows, occupied_populations))

      # Generate dispersers using a binomial distribution
      dispersers[occupied_dispersal_indices] <- rbinom(length(occupied_dispersal_indices), occupied_abundances_rep[occupied_dispersal_indices], occupied_dispersals[occupied_dispersal_indices])

      # Release variables from memory
      occupied_dispersals <- NULL; occupied_abundances_rep <- NULL; occupied_dispersal_indices <- NULL

      # Calculate emigrants
      emigrants <- array(0, occupied_populations)
      emigrants[] <- .colSums(dispersers, m = dispersal_compact_rows, n = occupied_populations)

      # Check consistency of emigrants (not to exceed abundances)
      excessive_indices <- which(emigrants > occupied_abundances)
      if (length(excessive_indices) > 0) { # reduce emigrants to equal abundance via random sampling
        for (excessive_index in excessive_indices) {
          excessive_rows <- which(as.logical(dispersers[, excessive_index])) # > 0
          excessive_dispersers <- dispersers[excessive_rows, excessive_index]
          disperser_reduction <- emigrants[excessive_index] - occupied_abundances[excessive_index]
          for(remove_row_index in sample(rep(excessive_rows, times = excessive_dispersers), disperser_reduction)) {
            dispersers[remove_row_index, excessive_index] <- dispersers[remove_row_index, excessive_index] - 1
          }
        }
        emigrants[excessive_indices] <- occupied_abundances[excessive_indices]
      }

      # Update population abundances
      population_abundances[occupied_indices] <- population_abundances[occupied_indices] - emigrants

      # Dispersal tracking
      if (dispersal_tracking) {
        results$emigrants[occupied_indices, tm] <- emigrants
      }

      # Release variables from memory
      occupied_abundances <- NULL; emigrants <- NULL; excessive_indices <- NULL

      # Calculate immigrants via dispersal immigrant map
      disperser_indices <- which(as.logical(dispersers)) # >0
      immigrant_array <- dispersal_zero_array
      immigrant_array[dispersal_immigrant_map[, occupied_indices][disperser_indices]] <- dispersers[disperser_indices]
      immigrants <- .colSums(immigrant_array, m = dispersal_compact_rows, n = populations)

      # Update population abundances
      population_abundances[1,] <- population_abundances[1,] + immigrants

      # Dispersal tracking
      if (dispersal_tracking) {
        results$immigrants[, tm] <- immigrants
      }

      # Release variables from memory
      dispersers <- NULL; immigrant_array <- NULL; immigrants <- NULL

    } # occupied populations & dispersal present?

    ## Apply threshold to population abundances ##
    if (!is.null(local_threshold)) {
      below_threshold_indices <- which(as.logical(population_abundances) & population_abundances <= local_threshold)
      if (length(below_threshold_indices)) {
        population_abundances[below_threshold_indices] <- 0
      }
    }

    ## Results collection ##
    if ("abundance" %in% results_selection) {
      results$abundance[, tm] <- population_abundances[1,]
    }
    if ("ema" %in% results_selection) {
      min_abundances <- pmin(min_abundances, population_abundances[1,])
      results$ema[,tm] <- min_abundances
    }
    if ("extirpation" %in% results_selection) {
      results$extirpation <- pmin(results$extirpation, rep(tm, populations), na.rm = TRUE)
      results$extirpation[which(as.logical(population_abundances))] <- NA
    }
    if ("harvested" %in% results_selection) {
      results$harvested[, tm] <- harvested
    }
    if ("occupancy" %in% results_selection) {
      results$occupancy[tm] <- sum(as.logical(population_abundances))
    }

  } ### End time steps ###

  return(results)

} ### End paleopop ###
