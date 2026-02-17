library(ringbp)
library(data.table)
library(epiparameter)

# not exact, works for differentiating LSHTM HPC from running locally
on_hpc <- nchar(Sys.getenv("SLURM_CLUSTER_NAME")) > 0

cat("Running on HPC: ", on_hpc, "\n")

cat("Running interactively: ", interactive(), "\n")

h5n1_weibull_params <- epiparameter::convert_summary_stats_to_params(
  "weibull", mean = 3.3, sd = 1.5
)
h7n9_weibull_params <- epiparameter::convert_summary_stats_to_params(
  "weibull", mean = 3.1, sd = 1.4
)

fast <-  epiparameter::convert_summary_stats_to_params("weibull", mean = 3, sd = 2)
slow <- epiparameter::convert_summary_stats_to_params("weibull", mean = 5, sd = 2)

# Put parameters that are grouped by disease into this data.table
scenarios <- data.table(
  expand.grid(
    delay_group = list(data.table(
      delay = c("fast", "slow"),
      onset_to_isolation = c(
        \(n) stats::rweibull(n = n, shape = fast$shape, scale = fast$scale),
        \(n) stats::rweibull(n = n, shape = slow$shape, scale = slow$scale)
      )
    )),
    incubation_period_group = list(data.table(
      subtype = c("H1N1", "H5N1", "H7N9"),
      incubation_period = c(
        \(n) stats::rweibull(n = n, shape = 1.77, scale = 1.86),
        \(n) stats::rweibull(
          n = n,
          shape = h5n1_weibull_params$shape,
          scale = h5n1_weibull_params$scale
        ),
        \(n) stats::rweibull(
          n = n,
          shape = h7n9_weibull_params$shape,
          scale = h7n9_weibull_params$scale
        )
      )
    )),
    r0_community = 2.5,
    r0_isolated = 0,
    disp_community = 0.8,
    disp_isolated = 1,
    prop_presymptomatic = c(0.15),
    prop_asymptomatic = c(0.1),
    prop_ascertain = 0.8,
    initial_cases = 5,
    quarantine = FALSE,
    cap_max_days = 140,
    cap_cases = 5000
  )
)

list_cols <- grep("_group", colnames(scenarios), value = TRUE)
non_list_cols <- setdiff(colnames(scenarios), list_cols)

expanded_groups <- scenarios[, rbindlist(delay_group), by = c(non_list_cols)]
expanded_incub <- scenarios[, rbindlist(incubation_period_group), by = c(non_list_cols)]

scenarios <- merge(
  expanded_groups, expanded_incub, by = non_list_cols, allow.cartesian = TRUE
)

scenarios[, scenario :=  1:.N]

n <- 1000

# try to find the optimal value of test_sensitivity
# define an objective function for optimization
objective_fn <- function(test_sensitivity, baseline_ext_prob) {
  message("Running simulations with test sensitivity: ", test_sensitivity)
  lft <- scenario_sim(
    n = n,
    initial_cases = x$initial_cases,
    offspring = offspring_opts(
      community = \(n) rnbinom(n = n, mu = x$r0_community, size = x$disp_community),
      isolated = \(n) rnbinom(n = n, mu = x$r0_isolated, size = x$disp_isolated)
    ),
    delays = delay_opts(
      incubation_period = x$incubation_period[[1]],
      onset_to_isolation = \(n) stats::rexp(n = n, rate = 1)
    ),
    event_probs = event_prob_opts(
      asymptomatic = x$prop_asymptomatic,
      presymptomatic_transmission = x$prop_presymptomatic,
      symptomatic_ascertained = x$prop_ascertain
    ),
    interventions = intervention_opts(
      quarantine = x$quarantine,
      test_sensitivity = test_sensitivity
    ),
    sim = sim_opts(cap_max_days = x$cap_max_days, cap_cases = x$cap_cases)
  )
  ext_prob <- extinct_prob(lft)

  # The optimizer minimizes this difference
  abs(ext_prob - baseline_ext_prob)
}

baseline_extinct_prob <- vector(mode = "numeric", length = nrow(scenarios))
opt_result <- vector(mode = "list", length = nrow(scenarios))

for (i in seq_along(baseline_extinct_prob)) {

  # run baseline simulation for fast and slow response for each subtype
  x <- scenarios[i, ]

  sim <- scenario_sim(
    n = n,
    initial_cases = x$initial_cases,
    offspring = offspring_opts(
      community = \(n) rnbinom(n = n, mu = x$r0_community, size = x$disp_community),
      isolated = \(n) rnbinom(n = n, mu = x$r0_isolated, size = x$disp_isolated)
    ),
    delays = delay_opts(
      incubation_period = x$incubation_period[[1]],
      onset_to_isolation = x$onset_to_isolation[[1]]
    ),
    event_probs = event_prob_opts(
      asymptomatic = x$prop_asymptomatic,
      presymptomatic_transmission = x$prop_presymptomatic,
      symptomatic_ascertained = x$prop_ascertain
    ),
    interventions = intervention_opts(quarantine = x$quarantine),
    sim = sim_opts(cap_max_days = x$cap_max_days, cap_cases = x$cap_cases)
  )

  baseline_extinct_prob[i] <- extinct_prob(sim)
  cat(
    "Baseline extinction probability for", scenarios$subtype[i], "&",
    scenarios$delay[i], ":", baseline_extinct_prob[[i]], "\n"
  )

  # optimise test_sensitivity to match extinction probability of fast and slow responses
  opt_result[[i]] <- optimize(
    objective_fn,
    interval = c(0, 1),
    baseline_ext_prob = baseline_extinct_prob[i]
  )
}

cat("Finished optimising test sensitivity \n")

lft_sensitivity <- vapply(opt_result, `[[`, FUN.VALUE = numeric(1), "minimum")
extinction_prob_delta <- vapply(opt_result, `[[`, FUN.VALUE = numeric(1), "objective")

scenarios[
  , c("lft_sensitivity", "extinction_prob_delta") := list(lft_sensitivity, extinction_prob_delta)
]

cat("Saving results... \n")

saveRDS(
  object = scenarios,
  file = file.path("inst", "extdata", "test_speed_sensitivity.rds")
)

cat("Finished \n")
