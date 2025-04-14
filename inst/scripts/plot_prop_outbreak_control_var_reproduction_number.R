library(data.table)
library(ggplot2)
library(dplyr)

# read simulation results saved by running inst/scripts/run_analysis.R
sweep_results <- readRDS(file.path("inst", "extdata", "simulations.rds"))

res <- sweep_results %>%
  group_by(scenario) %>%
  mutate(pext = ringbp::extinct_prob(sims[[1]], cap_cases = 500)) %>%
  ungroup(scenario)

dt <- as.data.table(res)
dt_data <- rbindlist(dt$data)
dt_data <- cbind(dt_data, scenario = dt$scenario, pext = dt$pext)

# convert to percentages for plotting
dt_data[, control_effectiveness := control_effectiveness * 100]
dt_data[, pext := pext * 100]

# calculate sd by subtype, R and control
subtype_sd <- dt_data[
  , .(subtype_sd = sd(pext)),
  by = .(index_R0, prop.asym, control_effectiveness, num.initial.cases, delay, theta)
]

prop_outbreak_control_var_reproduction_number_plot <- ggplot2::ggplot(
  data = subtype_sd
) +
  ggplot2::geom_point(
    mapping = ggplot2::aes(
      x = as.factor(control_effectiveness),
      y = subtype_sd,
      fill = as.factor(index_R0)
    ),
    shape = 21,
    size = 2,
    stroke  = 0.75,
    position = ggplot2::position_jitter(width = 0.2, height = 0)
  ) +
  ggplot2::scale_x_discrete(
    name = "Contacts traced (%)"
  ) +
  ggplot2::scale_y_continuous(
    name = "Standard deviation between subtype\nsimulated outbreaks controlled (%)",
  ) +
  ggplot2::scale_fill_brewer(palette = "RdBu", direction = -1) +
  ggplot2::labs(
    fill = "Reproduction Number (R)"
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_legend(
      override.aes = list(shape = 21)
    )
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  file.path("inst", "plots", "prop_outbreak_control_var_reproduction_number.png"),
  plot = prop_outbreak_control_var_reproduction_number_plot,
  device = "png",
  width = 200,
  height = 150,
  units = "mm",
  dpi = 300
)
