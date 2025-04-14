library(ggplot2)
library(data.table)
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

prop_outbreak_control_onset_to_isolation <- dt_data[
  num.initial.cases == 20 & theta == "15%" & index_R0 == 1.5 & prop.asym == 0,
  .(control_effectiveness, onset_to_isolation, delay, pext, subtype)
]

# convert to percentages for plotting
prop_outbreak_control_onset_to_isolation[, control_effectiveness := control_effectiveness * 100]
prop_outbreak_control_onset_to_isolation[, pext := pext * 100]

prop_outbreak_control_onset_to_isolation_plot <- ggplot2::ggplot(
  data = prop_outbreak_control_onset_to_isolation
) +
  ggplot2::geom_point(
    mapping = ggplot2::aes(
      x = control_effectiveness,
      y = pext,
      fill = as.factor(delay),
      shape = as.factor(subtype)
    ),
    size = 3
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(
      x = control_effectiveness,
      y = pext,
      colour = as.factor(delay),
      linetype = as.factor(subtype)
    ),
    linewidth = 0.75
  ) +
  ggplot2::scale_x_continuous(
    name = "Contacts traced (%)",
    limits = c(0, 100)
  ) +
  ggplot2::scale_y_continuous(
    name = "Simulated outbreaks controlled (%)",
    limits = c(0, 100)
  ) +
  ggplot2::scale_colour_manual(values = c("#92C5DE", "#F4A582")) +
  ggplot2::scale_fill_manual(values = c("#92C5DE", "#F4A582")) +
  ggplot2::scale_shape_manual(values = c(21, 22, 24)) +
  ggplot2::scale_linetype_manual(values = c(1, 1, 1)) +
  ggplot2::labs(
    colour = "Onset-to-isolation",
    fill = "Onset-to-isolation",
    shape = "Pathogen Subtype",
    linetype = "Pathogen Subtype"
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_legend(
      override.aes = list(shape = 21)
    )
  ) +
  ggplot2::theme_bw()

ggplot2::ggsave(
  file.path("inst", "plots", "prop_outbreak_control_onset_to_isolation.png"),
  plot = prop_outbreak_control_onset_to_isolation_plot,
  device = "png",
  width = 150,
  height = 150,
  units = "mm",
  dpi = 300
)
