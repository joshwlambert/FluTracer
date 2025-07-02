library(data.table)
library(ggplot2)

# read simulation results saved by running inst/scripts/run_analysis.R
h5n1_results <- readRDS(file.path("inst", "extdata", "H5N1_simulations_no_Q.rds"))
h1n1_results <- readRDS(file.path("inst", "extdata", "H1N1_simulations_no_Q.rds"))
h7n9_results <- readRDS(file.path("inst", "extdata", "H7N9_simulations_no_Q.rds"))

h5n1_results[, pext := ringbp::extinct_prob(sims[[1]], cap_cases = 5000), by = scenario]
h1n1_results[, pext := ringbp::extinct_prob(sims[[1]], cap_cases = 5000), by = scenario]
h7n9_results[, pext := ringbp::extinct_prob(sims[[1]], cap_cases = 5000), by = scenario]

h5n1_data <- rbindlist(h5n1_results$data)
h5n1_data[, `:=`(scenario = h5n1_results$scenario, pext = h5n1_results$pext)]

h1n1_data <- rbindlist(h1n1_results$data)
h1n1_data[, `:=`(scenario = h1n1_results$scenario, pext = h1n1_results$pext)]

h7n9_data <- rbindlist(h7n9_results$data)
h7n9_data[, `:=`(scenario = h7n9_results$scenario, pext = h7n9_results$pext)]

rm(h5n1_results)
rm(h1n1_results)
rm(h7n9_results)

flu_data <- rbindlist(list(h5n1_data, h1n1_data, h7n9_data))

# convert to percentages for plotting
flu_data[, prop_ascertain := prop_ascertain * 100]
flu_data[, pext := pext * 100]

# calculate sd by subtype, R and control
subtype_sd <- flu_data[
  , .(subtype_sd = sd(pext)),
  by = .(r0_community, prop_asymptomatic, prop_ascertain, initial_cases, delay, prop_presymptomatic)
]

prop_outbreak_control_var_reproduction_number_plot <- ggplot2::ggplot(
  data = subtype_sd
) +
  ggplot2::geom_point(
    mapping = ggplot2::aes(
      x = as.factor(prop_ascertain),
      y = subtype_sd,
      fill = as.factor(r0_community)
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
