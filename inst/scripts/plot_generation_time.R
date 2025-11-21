library(epiparameter)
library(data.table)
library(ggplot2)

h5n1_weibull_params <- epiparameter::convert_summary_stats_to_params(
  "weibull", mean = 3.3, sd = 1.5
)

h7n9_weibull_params <- epiparameter::convert_summary_stats_to_params(
  "weibull", mean = 3.1, sd = 1.4
)

presymptomatic_transmission <- 0.15

H1N1_gt <- ringbp:::incubation_to_generation_time(
  symptom_onset_time = rweibull(n = 1e5, shape = 1.77, scale = 1.86),
  alpha = ringbp:::presymptomatic_transmission_to_alpha(
    presymptomatic_transmission = presymptomatic_transmission
  )
)

H1N1 <- density(H1N1_gt, bw = 0.5)

H5N1_gt <- ringbp:::incubation_to_generation_time(
  symptom_onset_time = rweibull(
    n = 1e5,
    shape = h5n1_weibull_params$shape,
    scale = h5n1_weibull_params$scale
  ),
  alpha = ringbp:::presymptomatic_transmission_to_alpha(
    presymptomatic_transmission = presymptomatic_transmission
  )
)

H5N1 <- density(H5N1_gt, bw = 0.5)

H7N9_gt <- ringbp:::incubation_to_generation_time(
  symptom_onset_time = rweibull(
    n = 1e5,
    shape = h7n9_weibull_params$shape,
    scale = h7n9_weibull_params$scale
  ),
  alpha = ringbp:::presymptomatic_transmission_to_alpha(
    presymptomatic_transmission = presymptomatic_transmission
  )
)

H7N9 <- density(H7N9_gt, bw = 0.5)

gt <- data.table(
  H1N1_x = pmax(H1N1$x, 0),
  H1N1_y = H1N1$y,
  H5N1_x = pmax(H5N1$x, 0),
  H5N1_y = H5N1$y,
  H7N9_x = pmax(H7N9$x, 0),
  H7N9_y = H7N9$y
)

gt[, id := 1:.N]

gt <- melt(
  gt,
  id.vars = "id",
  measure = data.table::patterns(x = "_x$", y = "_y$"),
  variable.name = "pathogen"
)

gt[, pathogen := fcase(
  pathogen == 1, "H1N1",
  pathogen == 2, "H5N1",
  pathogen == 3, "H7N9"
)]

generation_time_plot <- ggplot2::ggplot(data = gt) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = x, y = y, col = pathogen)) +
  ggplot2::geom_ribbon(
    mapping = ggplot2::aes(ymin = 0, ymax = y, x = x, fill = pathogen),
    alpha = 0.1
  ) +
  ggplot2::geom_vline(
    mapping = ggplot2::aes(xintercept = median(H1N1_gt)), lty = 2, col = "#FFA500"
  ) +
  ggplot2::geom_vline(
    mapping = ggplot2::aes(xintercept = median(H5N1_gt)), lty = 2, col = "#800080"
  ) +
  ggplot2::geom_vline(
    mapping = ggplot2::aes(xintercept = median(H7N9_gt)), lty = 2, col = "#90D5FF"
  ) +
  ggplot2::scale_x_continuous(
    name = "Generation time (days between infector and infectee exposure)"
  ) +
  ggplot2::scale_y_continuous(name = "Density") +
  ggplot2::scale_colour_manual(values = c("#FFA500", "#800080", "#90D5FF")) +
  ggplot2::scale_fill_manual(values = c("#FFA500", "#800080", "#90D5FF")) +
  ggplot2::labs(colour = "Pathogen subtype", fill = "Pathogen subtype") +
  ggplot2::theme_bw()

ggplot2::ggsave(
  file.path("inst", "plots", "generation_time.png"),
  plot = generation_time_plot,
  device = "png",
  width = 160,
  height = 80,
  units = "mm",
  dpi = 300
)
