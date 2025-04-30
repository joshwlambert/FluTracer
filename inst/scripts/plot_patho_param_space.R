# number of random samples from distribution
n <- 1000

flu_param_space <- data.frame(
  pathogen = "influenza",
  R = rnorm(n = 2000, mean = 5, sd = 2),
  prop_asym = rnorm(n = n, mean = 40, sd = 2)
)
sars_param_space <- data.frame(
  pathogen = "SARS-CoV-1",
  R = rnorm(n = n, mean = 3, sd = 1),
  prop_asym = rnorm(n = n, mean = 5, sd = 2)
)
smallpox_param_space <- data.frame(
  pathogen = "Smallpox",
  R = rnorm(n = n, mean = 8, sd = 1),
  prop_asym = rnorm(n = n, mean = 10, sd = 2)
)
hiv_param_space <- data.frame(
  pathogen = "HIV",
  R = rnorm(n = n, mean = 4, sd = 1),
  prop_asym = rnorm(n = n, mean = 90, sd = 2)
)
covid_param_space <- data.frame(
  pathogen = "SARS-CoV-2",
  R = rnorm(n = n, mean = 2, sd = 1),
  prop_asym = rnorm(n = n, mean = 40, sd = 2)
)

flu_coords <- c(x = mean(flu_param_space$prop_asym), y = mean(flu_param_space$R))
sars_coords <- c(x = mean(sars_param_space$prop_asym), y = mean(sars_param_space$R))
hiv_coords <- c(x = mean(hiv_param_space$prop_asym), y = mean(hiv_param_space$R))
smallpox_coords <- c(x = mean(smallpox_param_space$prop_asym), y = mean(smallpox_param_space$R))
covid_coords <- c(x = mean(covid_param_space$prop_asym), y = mean(covid_param_space$R))

param_space <- rbind(
  flu_param_space,
  sars_param_space,
  smallpox_param_space,
  hiv_param_space,
  covid_param_space
)

# density plot
ggplot2::ggplot(data = param_space) +
  ggplot2::stat_density_2d(
    mapping = ggplot2::aes(
      x = prop_asym,
      y = R,
      fill = ggplot2::after_stat(density)
    ),
    geom = "raster",
    contour = FALSE
  ) +
  ggplot2::annotate(
    geom = "rect",
    xmin = 0,
    xmax = 35,
    ymin = 1.1,
    ymax = 3.5,
    alpha = 0.2,
    color = "grey5",
    fill = "lightblue"
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 17.5,
    y = 2,
    label = "H1N1/H5N1/H7N9\n Scenarios"
  ) +
  ggplot2::annotate(
    geom = "text",
    x = flu_coords[["x"]],
    y = flu_coords[["y"]],
    label = "Influenza",
  ) +
  ggplot2::annotate(
    geom = "text",
    x = sars_coords[["x"]],
    y = sars_coords[["y"]],
    label = "SARS-CoV-1"
  ) +
  ggplot2::annotate(
    geom = "text",
    x = hiv_coords[["x"]],
    y = hiv_coords[["y"]],
    label = "HIV"
  ) +
  ggplot2::annotate(
    geom = "text",
    x = smallpox_coords[["x"]],
    y = smallpox_coords[["y"]],
    label = "Smallpox"
  ) +
  ggplot2::annotate(
    geom = "text",
    x = covid_coords[["x"]],
    y = covid_coords[["y"]],
    label = "SARS-CoV-2"
  ) +
  ggplot2::scale_x_continuous(name = "Percentage of pre- or asymtomatic infections (%)", limits = c(0, 100)) +
  ggplot2::scale_y_continuous(name = "Reproduction number (R)", limits = c(0, 15)) +
  ggplot2::scale_fill_gradient2(low = "#ffffff", high = "darkgreen") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::theme(panel.background = ggplot2::element_blank())

# contour plot
ggplot2::ggplot(param_space, ggplot2::aes(x = prop_asym, y = R, color = pathogen)) +
  ggplot2::geom_density_2d() +
  ggplot2::annotate(
    geom = "rect",
    xmin = 0,
    xmax = 35,
    ymin = 1.1,
    ymax = 3.5,
    alpha=0.2,
    color = "red",
    fill = "red"
  ) +
  ggplot2::scale_x_continuous(name = "Percentage of pre- or asymtomatic infections (%)", limits = c(0, 100)) +
  ggplot2::scale_y_continuous(name = "Reproduction number (R)", limits = c(0, 15)) +
  ggplot2::theme_bw()

# deterministic plot
patho_param_space_plot <- ggplot2::ggplot() +
    ggplot2::annotate(
      geom = "rect", xmin = 0, xmax = 35, ymin = 1.1, ymax = 3.5,
      fill = "coral", alpha = 0.5
  ) +
  ggplot2::annotate(
    geom = "text", x = 17.5, y = 2.3, label = "H1N1/H5N1/H7N9\n Scenarios",
    size = 3
  ) +
  ggfx::with_blur(
    ggplot2::annotate(
      geom = "rect", xmin = 35, xmax = 45, ymin = 1, ymax = 10,
      fill = "darkgreen", alpha = 0.5
    ),
    sigma = 7.5
  ) +
  ggplot2::annotate(
    geom = "text", x = 40, y = 4.5, label = "Influenza", size = 3
  ) +
  ggfx::with_blur(
    ggplot2::annotate(
      geom = "rect", xmin = 0, xmax = 10, ymin = 2, ymax = 4,
      fill = "darkgreen", alpha = 0.5
    ),
    sigma = 7.5
  ) +
  ggplot2::annotate(
    geom = "text", x = 5, y = 3, label = "SARS-CoV-1", size = 3
  ) +
  # SARS-CoV-2 doesn't need a box as it's a subset of flu
  # (overlapping boxes increases opacity)
  ggplot2::annotate(
    geom = "text", x = 40, y = 2, label = "SARS-CoV-2", size = 3
  ) +
  ggfx::with_blur(
    ggplot2::annotate(
      geom = "rect", xmin = 85, xmax = 95, ymin = 3, ymax = 5,
      fill = "darkgreen", alpha = 0.5
    ),
    sigma = 7.5
  ) +
  ggplot2::annotate(
    geom = "text", x = 90, y = 4, label = "HIV", size = 3
  ) +
  ggfx::with_blur(
    ggplot2::annotate(
      geom = "rect", xmin = 5, xmax = 15, ymin = 7, ymax = 9,
      fill = "darkgreen", alpha = 0.5
    ),
    sigma = 7.5
  ) +
  ggplot2::annotate(
    geom = "text", x = 10, y = 8, label = "Smallpox", size = 3
  ) +
  ggfx::with_blur(
    ggplot2::annotate(
      geom = "rect", xmin = 0, xmax = 5, ymin = 0, ymax = 3,
      fill = "darkgreen", alpha = 0.5
    ),
    sigma = 7.5
  ) +
  ggplot2::annotate(
    geom = "text", x = 2.5, y = 0.5, label = "Ebola &\nMarburg", size = 3
  ) +
  ggplot2::scale_x_continuous(
    name = "Percentage of pre- or asymtomatic infections (%)",
    limits = c(0, 100)
  ) +
  ggplot2::scale_y_continuous(
    name = "Reproduction number (R)",
    limits = c(0, 10)
  ) +
  ggplot2::theme_bw()

ggplot2::ggsave(
  file.path("inst", "plots", "patho_param_space.png"),
  plot = patho_param_space_plot,
  device = "png",
  width = 160,
  height = 80,
  units = "mm",
  dpi = 300
)
