library(data.table)
library(ggplot2)

# read simulation results saved by running inst/scripts/run_analysis.R
h5n1_results <- readRDS(file.path("inst", "extdata", "h5n1_simulations.rds"))
h1n1_results <- readRDS(file.path("inst", "extdata", "h1n1_simulations.rds"))
h7n9_results <- readRDS(file.path("inst", "extdata", "h7n9_simulations.rds"))

h5n1_median_max_weekly_cases <- vapply(
  h5n1_results$sims,
  function(x) {
    median(x[, .SD[which.max(weekly_cases)], by = sim][, weekly_cases])
  },
  FUN.VALUE = numeric(1)
)

h1n1_median_max_weekly_cases <- vapply(
  h1n1_results$sims,
  function(x) {
    median(x[, .SD[which.max(weekly_cases)], by = sim][, weekly_cases])
  },
  FUN.VALUE = numeric(1)
)

h7n9_median_max_weekly_cases <- vapply(
  h7n9_results$sims,
  function(x) {
    median(x[, .SD[which.max(weekly_cases)], by = sim][, weekly_cases])
  },
  FUN.VALUE = numeric(1)
)

h5n1_max_weekly_cases <- rbindlist(h5n1_results$data)[, c("index_R0", "control_effectiveness", "delay")]
h5n1_max_weekly_cases[, max_weekly_cases := h5n1_median_max_weekly_cases]
h5n1_max_weekly_cases[, subtype := as.factor("H5N1")]

h1n1_max_weekly_cases <- rbindlist(h1n1_results$data)[, c("index_R0", "control_effectiveness", "delay")]
h1n1_max_weekly_cases[, max_weekly_cases := h1n1_median_max_weekly_cases]
h1n1_max_weekly_cases[, subtype := as.factor("H1N1")]

h7n9_max_weekly_cases <- rbindlist(h7n9_results$data)[, c("index_R0", "control_effectiveness", "delay")]
h7n9_max_weekly_cases[, max_weekly_cases := h7n9_median_max_weekly_cases]
h7n9_max_weekly_cases[, subtype := as.factor("H7N9")]

rm(h5n1_results)
rm(h1n1_results)
rm(h7n9_results)

max_weekly_cases <- rbindlist(
  list(
    h5n1_max_weekly_cases,
    h1n1_max_weekly_cases,
    h7n9_max_weekly_cases
  )
)

rm(h5n1_max_weekly_cases)
rm(h1n1_max_weekly_cases)
rm(h7n9_max_weekly_cases)

# Convert columns to factors for plotting
cols_to_factor <- c("index_R0", "control_effectiveness", "delay")
max_weekly_cases[, (cols_to_factor) := lapply(.SD, as.factor), .SDcols = cols_to_factor]

max_weekly_cases[, delay := factor(delay, levels = c("slow", "fast", "lft"))]

delay_labels <- c(
  "H5N1" = "H5N1",
  "H1N1" = "H1N1",
  "H7N9" = "H7N9",
  "slow" = "Slow",
  "fast" = "Fast",
  "lft" = "LFT"
)

max_weekly_cases_plot <- ggplot2::ggplot(data = max_weekly_cases) +
  ggplot2::geom_boxplot(
    mapping = ggplot2::aes(
      x = control_effectiveness,
      y = max_weekly_cases
    )
  ) +
  ggplot2::facet_grid(
    ggplot2::vars(delay), ggplot2::vars(subtype),
    labeller = ggplot2::as_labeller(delay_labels)
  ) +
  ggplot2::scale_x_discrete(name = "Contacts traced (%)") +
  ggplot2::scale_y_continuous(
    name = "Average maximum weekly cases",
    expand = c(0.1, 0.1)
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(strip.background = ggplot2::element_blank())


ggplot2::ggsave(
  file.path("inst", "plots", "max_weekly_cases.png"),
  plot = max_weekly_cases_plot,
  device = "png",
  width = 150,
  height = 150,
  units = "mm",
  dpi = 300
)
