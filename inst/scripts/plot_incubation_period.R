library(epiparameter)
library(data.table)
library(ggplot2)

h5n1_weibull_params <- epiparameter::convert_summary_stats_to_params(
  "weibull", mean = 3.3, sd = 1.5
)

h7n9_weibull_params <- epiparameter::convert_summary_stats_to_params(
  "weibull", mean = 3.1, sd = 1.4
)

density_eval <- seq(0, 10, 0.1)

incubation_period <- data.table(
  x = density_eval,
  H1N1 = dweibull(x = density_eval, shape = 1.77, scale = 1.86),
  H5N1 = dweibull(
    x = density_eval,
    shape = h5n1_weibull_params$shape,
    scale = h5n1_weibull_params$scale
  ),
  H7N9 = dweibull(
    x = density_eval,
    shape = h7n9_weibull_params$shape,
    scale = h7n9_weibull_params$scale
  )
)

incubation_period <- melt(
  data = incubation_period,
  id.vars = "x",
  measure.vars = c("H1N1", "H5N1", "H7N9"),
  variable.name = "pathogen",
  value.name = "incubation_period"
)

h1n1_median <- epiparameter::convert_params_to_summary_stats(
  "weibull", shape = 1.77, scale = 1.86
)$median
h5n1_median <- epiparameter::convert_params_to_summary_stats(
  "weibull",
  shape = h5n1_weibull_params$shape,
  scale = h5n1_weibull_params$scale
)$median
h7n9_median <- epiparameter::convert_params_to_summary_stats(
  "weibull",
  shape = h7n9_weibull_params$shape,
  scale = h7n9_weibull_params$scale
)$median

incubation_period_plot <- ggplot2::ggplot(data = incubation_period) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = x, y = incubation_period, col = pathogen)
  ) +
  ggplot2::geom_ribbon(
    mapping = ggplot2::aes(
      x = x,
      y = incubation_period,
      ymax = incubation_period,
      ymin = 0,
      fill = pathogen),
    alpha = 0.1
  ) +
  ggplot2::geom_vline(
    mapping = ggplot2::aes(xintercept = h1n1_median), lty = 2, col = "#FFA500"
  ) +
  ggplot2::geom_vline(
    mapping = ggplot2::aes(xintercept = h5n1_median), lty = 2, col = "#800080"
  ) +
  ggplot2::geom_vline(
    mapping = ggplot2::aes(xintercept = h7n9_median), lty = 2, col = "#90D5FF"
  ) +
  ggplot2::scale_x_continuous(name = "Days from infection to illness onset") +
  ggplot2::scale_y_continuous(name = "Density") +
  ggplot2::scale_colour_manual(values = c("#FFA500", "#800080", "#90D5FF")) +
  ggplot2::scale_fill_manual(values = c("#FFA500", "#800080", "#90D5FF")) +
  ggplot2::labs(colour = "Pathogen subtype", fill = "Pathogen subtype") +
  ggplot2::theme_bw()

ggplot2::ggsave(
  file.path("inst", "plots", "incubation_period.png"),
  plot = incubation_period_plot,
  device = "png",
  width = 160,
  height = 80,
  units = "mm",
  dpi = 300
)
