library(ringbp)
library(dplyr)
library(tidyr)
library(purrr)
library(future)
library(tibble)
library(data.table)

scenarios <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("SARS", "Wuhan"),
    delay_shape = c(1.651524, 2.305172),
    delay_scale = c(4.287786, 9.483875)
  )),
  k_group = list(tibble::tibble(
    theta = c("<1%", "15%"),
    k = c(30, 1.95)
  )),
  index_R0 = c(1.1, 1.5),
  quarantine = FALSE,
  prop.asym = c(0, 0.1),
  control_effectiveness = seq(0, 1, 0.2),
  num.initial.cases = c(5, 10)) %>%
  tidyr::unnest("k_group") %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

## Parameterise fixed paramters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = 365,
                                  cap_cases = 500,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.16)


## Set up multicore if using see ?future::plan for details
## Use the workers argument to control the number of cores used.
future::plan("multisession")

scenarios <- as.data.table(scenarios)

## Run paramter sweep
run_time <- system.time(
  sweep_results <- ringbp::parameter_sweep(
    scenarios,
    sim_fn = sim_with_params,
    samples = 100
  )
)


saveRDS(sweep_results, file = file.path("inst", "extdata", "simulations.rds"))
