library(data.table)
library(ggplot2)

density_eval <- seq(0, 25, 0.1)

onset_to_isolation <- data.table(
  x = density_eval,
  SARS = dweibull(density_eval, shape = 1.651524, scale = 4.287786),
  COVID = dweibull(density_eval, shape = 2.305172, scale = 9.483875)
)

onset_to_isolation <- melt(
  data = onset_to_isolation,
  id.vars = "x",
  measure.vars = c("SARS", "COVID"),
  variable.name = "disease",
  value.name = "onset_to_isolation"
)

sars_median <- epiparameter::convert_params_to_summary_stats(
  "weibull", shape = 1.651524, scale = 4.287786
)$median
covid_median <- epiparameter::convert_params_to_summary_stats(
  "weibull", shape = 2.305172, scale = 9.483875
)$median


onset_to_isolation_plot <- ggplot2::ggplot(data = onset_to_isolation) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(
      x = x,
      y = onset_to_isolation,
      col = disease
    )
  ) +
  ggplot2::geom_ribbon(
    mapping = ggplot2::aes(
      x = x,
      y = onset_to_isolation,
      ymin = 0,
      ymax = onset_to_isolation,
      fill = disease
    ),
    alpha = 0.1
  ) +
  ggplot2::geom_vline(
    mapping = ggplot2::aes(xintercept = sars_median), lty = 2, col = "#1A85FF"
  ) +
  ggplot2::geom_vline(
    mapping = ggplot2::aes(xintercept = covid_median), lty = 2, col = "#D41159"
  ) +
  ggplot2::scale_x_continuous(name = "Days from symptom onset to isolation") +
  ggplot2::scale_y_continuous(name = "Density") +
  ggplot2::scale_colour_manual(values = c("#1A85FF", "#D41159")) +
  ggplot2::scale_fill_manual(values = c("#1A85FF", "#D41159")) +
  ggplot2::labs(colour = "Disease", fill = "Disease") +
  ggplot2::theme_bw()

ggplot2::ggsave(
  file.path("inst", "plots", "onset_to_isolation.png"),
  plot = onset_to_isolation_plot,
  device = "png",
  width = 160,
  height = 80,
  units = "mm",
  dpi = 300
)
