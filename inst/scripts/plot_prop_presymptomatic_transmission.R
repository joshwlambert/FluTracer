library(data.table)
library(ggplot2)
library(sn)

density_eval <- seq(0, 15, 0.1)

prop_presymptomatic_transmission <- data.table(
  x = density_eval,
  prop_presym_trans_30 = sn::dsn(
    x = density_eval, xi = 5, omega = 2, alpha = 0.7
  ),
  prop_presym_trans_15 = sn::dsn(
    x = density_eval, xi = 5, omega = 2, alpha = 1.95
  ),
  prop_presym_trans_lt1 = sn::dsn(
    x = density_eval, xi = 5, omega = 2, alpha = 30
  )
)

prop_presymptomatic_transmission <- melt(
  data = prop_presymptomatic_transmission,
  id.vars = "x",
  measure.vars = c("prop_presym_trans_30", "prop_presym_trans_15", "prop_presym_trans_lt1"),
  variable.name = "prop",
  value.name = "prop_presym_trans"
)

incubation_period <- 5

prop_presymptomatic_transmission_plot <- ggplot2::ggplot(
  data = prop_presymptomatic_transmission
) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(
      x = x,
      y = prop_presym_trans,
      col = prop
    )
  ) +
  ggplot2::geom_ribbon(
    mapping = ggplot2::aes(
      x = x,
      y = prop_presym_trans,
      ymin = 0,
      ymax = prop_presym_trans,
      fill = prop
    ),
    alpha = 0.1
  ) +
  ggplot2::geom_vline(
    mapping = ggplot2::aes(xintercept = incubation_period),
    lty = 2
  ) +
  ggplot2::scale_x_continuous(name = "Days since infection") +
  ggplot2::scale_y_continuous(name = "Density") +
  ggplot2::labs(colour = "Proportion presymptomatic transmission", fill = "Proportion presymptomatic transmission") +
  ggplot2::scale_colour_manual(values = c("#009e74", "#56b3e9", "#cc79a7"), labels = c("30%", "15%", "<1%")) +
  ggplot2::scale_fill_manual(values = c("#009e74", "#56b3e9", "#cc79a7"), labels = c("30%", "15%", "<1%")) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  file.path("inst", "plots", "prop_presymptomatic_transmission.png"),
  plot = prop_presymptomatic_transmission_plot,
  device = "png",
  width = 160,
  height = 80,
  units = "mm",
  dpi = 300
)
