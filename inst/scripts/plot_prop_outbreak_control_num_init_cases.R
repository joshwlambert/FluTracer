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


prop_outbreak_control_num_init_cases <- dt_data[
  index_R0 == 1.5 & theta == "15%" & delay == "SARS" & prop.asym == 0,
  .(control_effectiveness, num.initial.cases, pext)
]

# convert to percentages for plotting
prop_outbreak_control_num_init_cases[, control_effectiveness := control_effectiveness * 100]
prop_outbreak_control_num_init_cases[, pext := pext * 100]

prop_outbreak_control_num_init_cases_plot <- ggplot2::ggplot(
  data = prop_outbreak_control_num_init_cases
) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(
      x = control_effectiveness,
      y = pext,
      colour = as.factor(num.initial.cases)
    ),
    size = 0.75
  ) +
  ggplot2::geom_point(
    mapping = ggplot2::aes(
      x = control_effectiveness,
      y = pext,
      fill = as.factor(num.initial.cases)
    ),
    shape = 21,
    size = 3,
    colour = "black"
  ) +
  ggplot2::scale_x_continuous(
    name = "Contacts traced (%)",
    limits = c(0, 100)
  ) +
  ggplot2::scale_y_continuous(
    name = "Simulated outbreaks controlled (%)",
    limits = c(0, 100)
  ) +
  ggplot2::scale_colour_manual(values = c("#ffbe6a", "#40b0a7")) +
  ggplot2::scale_fill_manual(values = c("#ffbe6a", "#40b0a7")) +
  ggplot2::labs(
    colour = "Number of initial cases",
    fill = "Number of initial cases"
  ) +
  ggplot2::theme_bw()

ggplot2::ggsave(
  file.path("inst", "plots", "prop_outbreak_control_num_init_cases.png"),
  plot = prop_outbreak_control_num_init_cases_plot,
  device = "png",
  width = 150,
  height = 150,
  units = "mm",
  dpi = 300
)
