library(ggplot2)
library(data.table)

# read simulation results saved by running inst/scripts/run_analysis.R

sweep_results <- readRDS(file.path("inst", "extdata", "simulations.rds"))

res <- sweep_results %>%
  group_by(scenario) %>%
  mutate(pext = ringbp::extinct_prob(sims[[1]], cap_cases = 5000)) %>%
  ungroup(scenario)

dt <- as.data.table(res)
dt_data <- rbindlist(dt$data)
dt_data <- cbind(dt_data, scenario = dt$scenario, pext = dt$pext)

prop_outbreak_control_num_init_cases <- dt_data %>%
  dplyr::filter(delay == "SARS",
                theta == "15%",
                index_R0 == 2.5,
                prop.asym == 0) %>%
  ggplot2::ggplot(ggplot2::aes(x = control_effectiveness,
                               y = pext,
                               color = as.factor(num.initial.cases))) +
  ggplot2::geom_line(size = 0.75) +
  ggplot2::geom_point(shape = 21,
                      col = "black",
                      aes(fill = as.factor(num.initial.cases)),
                      size = 3) +
  ggplot2::scale_fill_manual(guide = "none",
                             values = c("dodgerblue", "black", "dodgerblue3")) +
  ggplot2::scale_color_manual(values = c("dodgerblue", "black", "dodgerblue3"),
                              name = "Number of\ninitial cases")  +
  cowplot::theme_cowplot()

ggplot2::ggsave(file.path("inst", "plots", "prop_outbreak_control_num_init_cases.png"), plot = prop_outbreak_control_num_init_cases)

