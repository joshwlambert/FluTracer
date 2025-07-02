library(data.table)
library(ggplot2)

# read simulation results saved by running inst/scripts/run_r0_analysis.R
h5n1_results <- readRDS(file.path("inst", "extdata", "H5N1_r0_analysis_no_Q.rds"))
h1n1_results <- readRDS(file.path("inst", "extdata", "H1N1_r0_analysis_no_Q.rds"))
h7n9_results <- readRDS(file.path("inst", "extdata", "H7N9_r0_analysis_no_Q.rds"))

h5n1_Q_results <- readRDS(file.path("inst", "extdata", "H5N1_r0_analysis_Q.rds"))
h1n1_Q_results <- readRDS(file.path("inst", "extdata", "H1N1_r0_analysis_Q.rds"))
h7n9_Q_results <- readRDS(file.path("inst", "extdata", "H7N9_r0_analysis_Q.rds"))

h5n1_results <- rbindlist(list(h5n1_results, h5n1_Q_results))
h1n1_results <- rbindlist(list(h1n1_results, h1n1_Q_results))
h7n9_results <- rbindlist(list(h7n9_results, h7n9_Q_results))

rm(h5n1_Q_results)
rm(h1n1_Q_results)
rm(h7n9_Q_results)

# the effective R0 is the same for all weeks within each replicate so getting
# from week 20 is the same as getting from any other week
h5n1_median_r0 <- sapply(h5n1_results$sims, function(x) median(x[week == 20, effective_r0]))
h1n1_median_r0 <- sapply(h1n1_results$sims, function(x) median(x[week == 20, effective_r0]))
h7n9_median_r0 <- sapply(h7n9_results$sims, function(x) median(x[week == 20, effective_r0]))

h5n1_data <- rbindlist(h5n1_results$data)
h1n1_data <- rbindlist(h1n1_results$data)
h7n9_data <- rbindlist(h7n9_results$data)

rm(h5n1_results)
rm(h1n1_results)
rm(h7n9_results)

h5n1_data[, median_r0 := h5n1_median_r0]
h1n1_data[, median_r0 := h1n1_median_r0]
h7n9_data[, median_r0 := h7n9_median_r0]

flu_data <- rbindlist(list(h5n1_data, h1n1_data, h7n9_data))

# convert to percentages for plotting
flu_data[, prop_ascertain := prop_ascertain * 100]

flu_data[, delay := factor(delay, levels = c("slow", "fast", "lft"))]
flu_data[, prop_ascertain := factor(prop_ascertain)]
flu_data[, subtype := factor(subtype)]

reproduction_number_low_presym_asym <- flu_data[
   prop_presymptomatic == 0.01 & prop_asymptomatic == 0,
  .(r0_community, prop_ascertain, quarantine, subtype, delay, median_r0)
]

reproduction_number_low_presym_asym <- flu_data[
  prop_presymptomatic == 0.01 & prop_asymptomatic == 0,
  .(r0_community, prop_ascertain, quarantine, subtype, delay, median_r0)
]

reproduction_number_high_presym_asym <- flu_data[
  prop_presymptomatic == 0.3 & prop_asymptomatic == 0.3,
  .(r0_community, prop_ascertain, quarantine, subtype, delay, median_r0)
]

delay_labels <- c(
  "H5N1" = "H5N1",
  "H1N1" = "H1N1",
  "H7N9" = "H7N9",
  "slow" = "Slow",
  "fast" = "Fast",
  "lft" = "LFT"
)

r0_low_presym_asym_plot <- ggplot2::ggplot(
  data = reproduction_number_low_presym_asym
) +
  ggplot2::geom_smooth(
    mapping = ggplot2::aes(
      x = r0_community,
      y = median_r0,
      colour = prop_ascertain,
      linetype = quarantine
    ),
    se = FALSE
  ) +
  ggplot2::geom_abline(intercept = 0, slope = 1) +
  ggplot2::geom_hline(yintercept = 1, linetype = "dotted") +
  ggplot2::geom_vline(xintercept = 1, linetype = "dotted") +
  ggplot2::facet_grid(
    vars(delay), vars(subtype),
    labeller = ggplot2::as_labeller(delay_labels)
  ) +
  ggplot2::scale_y_continuous(
    name = "Median Rt",
    limits = c(0, 3.5)
  ) +
  ggplot2::scale_x_continuous(
    name = "Simulated community R0",
    limits = c(0, 3.5)
  ) +
  ggplot2::scale_color_brewer(palette = "Set1") +
  ggplot2::labs(colour = "Contacts traced (%)", linetype = "Quarantine") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_blank(),
    legend.position = "bottom",
    legend.box = "vertical"
  )

ggplot2::ggsave(
  file.path("inst", "plots", "r0_low_presym_asym.png"),
  plot = r0_low_presym_asym_plot,
  device = "png",
  width = 150,
  height = 150,
  units = "mm",
  dpi = 300
)

r0_high_presym_asym_plot <- ggplot2::ggplot(
  data = reproduction_number_high_presym_asym
) +
  ggplot2::geom_smooth(
    mapping = ggplot2::aes(
      x = r0_community,
      y = median_r0,
      colour = prop_ascertain,
      linetype = quarantine
    ),
    se = FALSE
  ) +
  ggplot2::geom_abline(intercept = 0, slope = 1) +
  ggplot2::geom_hline(yintercept = 1, linetype = "dotted") +
  ggplot2::geom_vline(xintercept = 1, linetype = "dotted") +
  ggplot2::facet_grid(
    vars(delay), vars(subtype),
    labeller = ggplot2::as_labeller(delay_labels)
  ) +
  ggplot2::scale_y_continuous(
    name = "Median Rt",
    limits = c(0, 3.5)
  ) +
  ggplot2::scale_x_continuous(
    name = "Simulated community R0",
    limits = c(0, 3.5)
  ) +
  ggplot2::scale_color_brewer(palette = "Set1") +
  ggplot2::labs(colour = "Contacts traced (%)", linetype = "Quarantine") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_blank(),
    legend.position = "bottom",
    legend.box = "vertical"
  )

ggplot2::ggsave(
  file.path("inst", "plots", "r0_high_presym_asym.png"),
  plot = r0_high_presym_asym_plot,
  device = "png",
  width = 150,
  height = 150,
  units = "mm",
  dpi = 300
)

