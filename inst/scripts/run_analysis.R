library(ringbp)
library(data.table)
library(epiparameter)
library(future)
library(future.apply)

# not exact, works for differentiating LSHTM HPC from running locally
on_hpc <- nchar(Sys.getenv("SLURM_CLUSTER_NAME")) > 0

cat("Running on HPC: ", on_hpc, "\n")

cat("Running interactively: ", interactive(), "\n")

# if run on HPC capture which pathogen subtype to run
if (!interactive() && on_hpc) {
  args <- commandArgs(TRUE)
} else {
  args <- c("H5N1", "no_quarantine")
}

# underscore for data.table subsetting
subtype_ <- match.arg(args[1], choices = c("H1N1", "H5N1", "H7N9"))
quarantine_ <- match.arg(args[2], choices = c("quarantine", "no_quarantine"))

cat("Running subtype: ", subtype_, "\n")

cat("Running with: ", quarantine_, "\n")

# convert character string to boolean logical
quarantine_ <- quarantine_ == "quarantine"

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
      delay = c("fast", "slow", "lft"),
      onset_to_isolation = c(
        \(n) stats::rweibull(n = n, shape = 1.651524, scale = 4.287786),
        \(n) stats::rweibull(n = n, shape = 2.305172, scale = 9.483875),
        \(n) stats::rexp(n = n, rate = 0.5)
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
    r0_community = c(1.1, 1.5, 2.5, 3.5),
    r0_isolated = 0,
    disp_community = 0.16,
    disp_isolated = 1,
    prop_presymptomatic = c(0.01, 0.15, 0.3),
    prop_asymptomatic = c(0, 0.1, 0.3),
    prop_ascertain = seq(0, 1, 0.2),
    initial_cases = c(5, 20, 40),
    quarantine = c(FALSE, TRUE),
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

# subset to subtype
scenarios <- scenarios[subtype == subtype_]
scenarios <- scenarios[quarantine == quarantine_]

scenario_sims <- scenarios[, list(data = list(.SD)), by = scenario]

n <- 100

# Set up multicore if using see ?future::plan for details
# Use the workers argument to control the number of cores used.
if (!interactive() && on_hpc) {
  future::plan("multicore", workers = 16)
} else {
  future::plan("multisession", workers = 4)
}

# Run parameter sweep
scenario_sims[, sims := future_lapply(data, \(x, n) {
  scenario_sim(
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
},
n = n,
future.seed = TRUE
)]

if (quarantine_) {
  file_suffix <- "Q"
} else {
  file_suffix <- "no_Q"
}

cat("Finished simulation... \n")

cat("Saving simulation results... \n")

saveRDS(
  object = scenario_sims,
  file = file.path(
    "inst", "extdata", paste0(subtype_, "_simulations_", file_suffix, ".rds")
  )
)

cat("Finished \n")
