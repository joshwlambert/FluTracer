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

fast <-  epiparameter::convert_summary_stats_to_params("weibull", mean = 3, sd = 2)
slow <- epiparameter::convert_summary_stats_to_params("weibull", mean = 5, sd = 2)

# Put parameters that are grouped by disease into this data.table
scenarios <- data.table(
  expand.grid(
    delay_group = list(data.table(
      delay = c("fast", "slow", "lft"),
      onset_to_isolation = c(
        \(n) stats::rweibull(n = n, shape = fast$shape, scale = fast$scale),
        \(n) stats::rweibull(n = n, shape = slow$shape, scale = slow$scale),
        \(n) stats::rexp(n = n, rate = 1)
      )
    )),
    incubation_period_group = list(data.table(
      subtype = c("H5N1"),
      incubation_period = c(
        \(n) stats::rweibull(
          n = n,
          shape = h5n1_weibull_params$shape,
          scale = h5n1_weibull_params$scale
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
    initial_cases = 20,
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

n <- 100

# run baseline simulation with fast response
x <- scenarios[delay == "fast", ]

fast <- scenario_sim(
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

fast_extinct_prob <- extinct_prob(fast)
cat("Baseline fast extinction probability:", fast_extinct_prob, "\n")

# run baseline simulation with slow response
x <- scenarios[delay == "slow", ]

slow <- scenario_sim(
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

slow_extinct_prob <- extinct_prob(slow)
cat("Baseline slow extinction probability:", slow_extinct_prob, "\n")

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

# optimise test_sensitivity to match extinction probability of fast response
fast_opt_result <- optimize(
  objective_fn,
  interval = c(0, 1),
  baseline_ext_prob = fast_extinct_prob
)

# optimise test_sensitivity to match extinction probability of slow response
slow_opt_result <- optimize(
  objective_fn,
  interval = c(0, 1),
  baseline_ext_prob = slow_extinct_prob
)

cat("Finished optimising test sensitivity \n")

test_sensitivity <- data.frame(
  response = c("fast", "slow"),
  lft_test_sensivitiy = c(fast_opt_result$minimum, slow_opt_result$minimum),
  extinction_prob_delta = c(fast_opt_result$objective, slow_opt_result$objective)
)

cat("Saving results... \n")

saveRDS(
  object = test_sensitivity,
  file = file.path("inst", "extdata", "test_speed_sensitivity.rds")
)

cat("Finished \n")
