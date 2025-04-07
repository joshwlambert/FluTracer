library(data.table)
library(ggplot2)

# read simulation results saved by running inst/scripts/run_analysis.R
sweep_results <- readRDS(file.path("inst", "extdata", "simulations.rds"))

res <- sweep_results %>%
  group_by(scenario) %>%
  mutate(pext = ringbp::extinct_prob(sims[[1]], cap_cases = 5000)) %>%
  ungroup(scenario)

dt <- as.data.table(res)
dt_data <- rbindlist(dt$data)
dt_data <- cbind(dt_data, scenario = dt$scenario, pext = dt$pext)

prop_outbreak_control <- dt_data[
  num.initial.cases == 10 & theta == "15%" & delay == "SARS" & prop.asym == 0,
  .(control_effectiveness, index_R0, pext)
]

# convert to percentages for plotting
prop_outbreak_control[, control_effectiveness := control_effectiveness * 100]
prop_outbreak_control[, pext := pext * 100]

prop_outbreak_control_plot <- ggplot2::ggplot(data = prop_outbreak_control) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(
      x = control_effectiveness,
      y = pext,
      colour = as.factor(index_R0)
    ),
    size = 0.75
  ) +
  ggplot2::geom_point(
    mapping = ggplot2::aes(
      x = control_effectiveness,
      y = pext,
      fill = as.factor(index_R0)
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
  ggplot2::scale_colour_manual(values = c("#f4b301", "#db1048")) +
  ggplot2::scale_fill_manual(values = c("#f4b301", "#db1048")) +
  ggplot2::labs(
    colour = "Reproduction Number (R)",
    fill = "Reproduction Number (R)"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  file.path("inst", "plots", "prop_outbreak_control_reproduction_number.png"),
  plot = prop_outbreak_control_plot,
  device = "png",
  width = 150,
  height = 150,
  units = "mm",
  dpi = 300
)

