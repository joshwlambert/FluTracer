library(ringbp)
library(purrr)
library(future)
library(data.table)
library(epiparameter)

h5n1_weibull_params <- epiparameter::convert_summary_stats_to_params(
  "weibull", mean = 3.3, sd = 1.5
)
h7n9_weibull_params <- epiparameter::convert_summary_stats_to_params(
  "weibull", mean = 3.1, sd = 1.4
)

# Put parameters that are grouped by disease into this data.table
scenarios <- data.table(
  expand.grid(
    delay_group = list(data.table(
      delay = c("SARS", "Wuhan"),
      onset_to_isolation = c(
        \(x) stats::rweibull(n = x, shape = 1.651524, scale = 4.287786),
        \(x) stats::rweibull(n = x, shape = 2.305172, scale = 9.483875)
      )
    )),
    k_group = list(data.table(
      theta = c("<1%", "15%", "30%"),
      k = c(30, 1.95, 0.7)
    )),
    incubation_period_group = list(data.table(
      subtype = c("H1N1", "H5N1", "H7N9"),
      incubation_period = c(
        \(x) stats::rweibull(n = x, shape = 1.77, scale = 1.86),
        \(x) stats::rweibull(
          n = x,
          shape = h5n1_weibull_params$shape,
          scale = h5n1_weibull_params$scale
        ),
        \(x) stats::rweibull(
          n = x,
          shape = h7n9_weibull_params$shape,
          scale = h7n9_weibull_params$scale
        )
      )
    )),
    index_R0 = c(1.1, 1.5, 2.5, 3.5),
    prop.asym = c(0, 0.1),
    control_effectiveness = seq(0, 1, 0.2),
    num.initial.cases = c(5, 20, 40),
    quarantine = FALSE
  )
)

list_cols <- grep("_group", colnames(scenarios), value = TRUE)
non_list_cols <- setdiff(colnames(scenarios), list_cols)

expanded_groups <- scenarios[, rbindlist(delay_group), by = c(non_list_cols)]
expanded_k <- scenarios[, rbindlist(k_group), by = c(non_list_cols)]
expanded_incub <- scenarios[, rbindlist(incubation_period_group), by = c(non_list_cols)]

scenarios <- merge(
  expanded_groups, expanded_k, by = non_list_cols, allow.cartesian = TRUE
)
scenarios <- merge(
  scenarios, expanded_incub, by = non_list_cols, allow.cartesian = TRUE
)
scenarios[, scenario :=  1:.N]

# Parameterise fixed paramters
sim_with_params <- purrr::partial(
  ringbp::scenario_sim,
  cap_max_days = 365,
  cap_cases = 500,
  r0isolated = 0,
  disp.iso = 1,
  disp.subclin = 0.16,
  disp.com = 0.16
)

# Set up multicore if using see ?future::plan for details
# Use the workers argument to control the number of cores used.
future::plan("multisession", workers = 8)

# Run parameter sweep
sweep_results <- ringbp::parameter_sweep(
  scenarios,
  sim_fn = sim_with_params,
  samples = 100
)

saveRDS(sweep_results, file = file.path("inst", "extdata", "simulations.rds"))
