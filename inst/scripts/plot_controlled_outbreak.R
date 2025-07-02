library(data.table)
library(ggplot2)

# read simulation results saved by running inst/scripts/run_analysis.R
h5n1_results <- readRDS(file.path("inst", "extdata", "H5N1_simulations_no_Q.rds"))
h1n1_results <- readRDS(file.path("inst", "extdata", "H1N1_simulations_no_Q.rds"))
h7n9_results <- readRDS(file.path("inst", "extdata", "H7N9_simulations_no_Q.rds"))

h5n1_total_cases <- lapply(h5n1_results$sims, function(x) {
  total_cases <- x[, .SD[which.max(week)], by = sim][, cumulative]
  total_cases[total_cases > 5000] <- 5000
  return(total_cases)
})

h1n1_total_cases <- lapply(h1n1_results$sims, function(x) {
  total_cases <- x[, .SD[which.max(week)], by = sim][, cumulative]
  total_cases[total_cases > 5000] <- 5000
  return(total_cases)
})

h7n9_total_cases <- lapply(h7n9_results$sims, function(x) {
  total_cases <- x[, .SD[which.max(week)], by = sim][, cumulative]
  total_cases[total_cases > 5000] <- 5000
  return(total_cases)
})

median_h5n1_controlled_outbreak_size <- vapply(
  h5n1_total_cases,
  function(x) median(x[x != 5000]),
  FUN.VALUE = numeric(1)
)

median_h1n1_controlled_outbreak_size <- vapply(
  h1n1_total_cases,
  function(x) median(x[x != 5000]),
  FUN.VALUE = numeric(1)
)

median_h7n9_controlled_outbreak_size <- vapply(
  h7n9_total_cases,
  function(x) median(x[x != 5000]),
  FUN.VALUE = numeric(1)
)

prop_h5n1_controlled_outbreak <- vapply(
  h5n1_total_cases,
  function(x) sum(x < 5000),
  FUN.VALUE = numeric(1)
)

prop_h1n1_controlled_outbreak <- vapply(
  h1n1_total_cases,
  function(x) sum(x < 5000),
  FUN.VALUE = numeric(1)
)

prop_h7n9_controlled_outbreak <- vapply(
  h7n9_total_cases,
  function(x) sum(x < 5000),
  FUN.VALUE = numeric(1)
)

median_h5n1_control_outbreak <- rbindlist(h5n1_results$data)[, c("r0_community", "prop_ascertain", "delay")]
median_h5n1_control_outbreak[, median_outbreak := median_h5n1_controlled_outbreak_size]
median_h5n1_control_outbreak[, prop_controlled_outbreak := prop_h5n1_controlled_outbreak]
median_h5n1_control_outbreak[, subtype := as.factor("H5N1")]

median_h1n1_control_outbreak <- rbindlist(h1n1_results$data)[, c("r0_community", "prop_ascertain", "delay")]
median_h1n1_control_outbreak[, median_outbreak := median_h1n1_controlled_outbreak_size]
median_h1n1_control_outbreak[, prop_controlled_outbreak := prop_h1n1_controlled_outbreak]
median_h1n1_control_outbreak[, subtype := as.factor("H1N1")]

median_h7n9_control_outbreak <- rbindlist(h7n9_results$data)[, c("r0_community", "prop_ascertain", "delay")]
median_h7n9_control_outbreak[, median_outbreak := median_h7n9_controlled_outbreak_size]
median_h7n9_control_outbreak[, prop_controlled_outbreak := prop_h7n9_controlled_outbreak]
median_h7n9_control_outbreak[, subtype := as.factor("H7N9")]

rm(h5n1_results)
rm(h1n1_results)
rm(h7n9_results)

median_control_outbreak <- rbindlist(
  list(
    median_h5n1_control_outbreak,
    median_h1n1_control_outbreak,
    median_h7n9_control_outbreak
  )
)

rm(median_h5n1_control_outbreak)
rm(median_h1n1_control_outbreak)
rm(median_h7n9_control_outbreak)

# Convert columns to factors for plotting
cols_to_factor <- c("r0_community", "prop_ascertain", "delay")

# Convert in place (by reference)
median_control_outbreak[, (cols_to_factor) := lapply(.SD, as.factor), .SDcols = cols_to_factor]

median_control_outbreak[, delay := factor(delay, levels = c("slow", "fast", "lft"))]

delay_labels <- c(
  "H5N1" = "H5N1",
  "H1N1" = "H1N1",
  "H7N9" = "H7N9",
  "slow" = "Slow",
  "fast" = "Fast",
  "lft" = "LFT"
)

median_controlled_outbreak_size_plot <- ggplot2::ggplot(
  data = median_control_outbreak
) +
  ggplot2::geom_boxplot(
    mapping = ggplot2::aes(
      x = prop_ascertain,
      y = median_outbreak
    )
  ) +
  ggplot2::facet_grid(
    ggplot2::vars(delay), ggplot2::vars(subtype),
    labeller = ggplot2::as_labeller(delay_labels)
  ) +
  ggplot2::scale_x_discrete(name = "Contacts traced (%)") +
  ggplot2::scale_y_continuous(
    name = "Median cumulative cases in controlled outbreaks",
    limits = c(0, 5000)
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(strip.background = ggplot2::element_blank())

ggplot2::ggsave(
  file.path("inst", "plots", "median_controlled_outbreak_size.png"),
  plot = median_controlled_outbreak_size_plot,
  device = "png",
  width = 150,
  height = 150,
  units = "mm",
  dpi = 300
)

prop_controlled_outbreak_plot <- ggplot2::ggplot(data = median_control_outbreak) +
  ggplot2::geom_boxplot(
    mapping = ggplot2::aes(
      x = prop_ascertain,
      y = prop_controlled_outbreak
    )
  ) +
  ggplot2::facet_grid(
    ggplot2::vars(delay), ggplot2::vars(subtype),
    labeller = ggplot2::as_labeller(delay_labels)
  ) +
  ggplot2::scale_x_discrete(name = "Contacts traced (%)") +
  ggplot2::scale_y_continuous(
    name = "Proportion of simulated outbreaks controlled (%)"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(strip.background = ggplot2::element_blank())

ggplot2::ggsave(
  file.path("inst", "plots", "prop_controlled_outbreak.png"),
  plot = prop_controlled_outbreak_plot,
  device = "png",
  width = 150,
  height = 150,
  units = "mm",
  dpi = 300
)
