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

prop_outbreak_control <- flu_data[
  initial_cases == 20 & prop_presymptomatic == 0.15 & prop_asymptomatic == 0,
  .(prop_ascertain, r0_community, pext, subtype, delay)
]

# convert to percentages for plotting
prop_outbreak_control[, prop_ascertain := prop_ascertain * 100]
prop_outbreak_control[, pext := pext * 100]

# factorise and order delay type for plotting order
prop_outbreak_control[, delay := as.factor(delay)]

prop_outbreak_control[, delay := factor(delay, levels = c("slow", "fast", "lft"))]

delay_labels <- c(
  "slow" = "(A)          Slow",
  "fast" = "(B)          Fast",
  "lft" = "(C)          LFT"
)

prop_outbreak_control_plot <- ggplot2::ggplot(data = prop_outbreak_control) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(
      x = prop_ascertain,
      y = pext,
      colour = as.factor(r0_community),
      linetype = as.factor(subtype)
    ),
    linewidth = 0.75
  ) +
  ggplot2::geom_point(
    mapping = ggplot2::aes(
      x = prop_ascertain,
      y = pext,
      fill = as.factor(r0_community),
      shape = as.factor(subtype)
    ),
    size = 3,
    stroke = 0.75
  ) +
  ggplot2::facet_wrap(
    vars(delay),
    labeller = ggplot2::as_labeller(delay_labels)
  ) +
  ggplot2::scale_x_continuous(
    name = "Contacts traced (%)",
    limits = c(0, 100)
  ) +
  ggplot2::scale_y_continuous(
    name = "Simulated outbreaks controlled (%)",
    limits = c(0, 100)
  ) +
  ggplot2::scale_color_brewer(palette = "RdBu", direction = -1) +
  ggplot2::scale_fill_brewer(palette = "RdBu", direction = -1) +
  ggplot2::scale_shape_manual(values = c(21, 22, 24)) +
  ggplot2::scale_linetype_manual(values = c(1, 1, 1)) +
  ggplot2::labs(
    colour = "Reproduction Number (R)",
    fill = "Reproduction Number (R)",
    shape = "Pathogen Subtype",
    linetype = "Pathogen Subtype"
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_legend(
      override.aes = list(shape = 21)
    )
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "bottom",
    legend.box="vertical",
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold", size = 12, hjust = 0)
  )

ggplot2::ggsave(
  file.path("inst", "plots", "prop_outbreak_control_reproduction_number.png"),
  plot = prop_outbreak_control_plot,
  device = "png",
  width = 150,
  height = 150,
  units = "mm",
  dpi = 300
)

