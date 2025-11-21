library(data.table)
library(ggplot2)
library(ggtext)

# read simulation results saved by running inst/scripts/run_analysis.R
results <- readRDS(file.path("inst", "extdata", "simulations_iso_disp.rds"))

results[, pext := ringbp::extinct_prob(sims[[1]], extinction_week = 12), by = scenario]

flu_data <- rbindlist(results$data)
flu_data[, `:=`(scenario = results$scenario, pext = results$pext)]

rm(results)

prop_outbreak_control <- flu_data[,
  .(prop_ascertain, r0_community, r0_isolated, disp_community, pext, subtype, delay)
]

# convert to percentages for plotting
prop_outbreak_control[, prop_ascertain := prop_ascertain * 100]
prop_outbreak_control[, pext := pext * 100]

# factorise and order delay type for plotting order
prop_outbreak_control[, delay := as.factor(delay)]

prop_outbreak_control[, delay := factor(delay, levels = c("slow", "fast", "lft"))]

delay_labels <- c(
  "slow" = "Slow",
  "fast" = "Fast",
  "lft" = "LFT",
  "0" = "Perfect Isolation (<i>R</i><sub>0</sub><sup>iso</sup> = 0)",
  "0.5" = "Imperfect Isolation (<i>R</i><sub>0</sub><sup>iso</sup> = 0.5)"
)

prop_outbreak_control_iso_plot <- ggplot2::ggplot(
  data = prop_outbreak_control[disp_community == 0.8, ]
) +
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
  ggplot2::facet_grid(
    vars(delay), vars(r0_isolated),
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
    strip.text = ggtext::element_markdown(size = 12, hjust = 0)
  )

delay_labels <- c(
  "slow" = "Slow",
  "fast" = "Fast",
  "lft" = "LFT",
  "0.8" = "Moderate heterogeneity (<i>k</i> = 0.8)",
  "5" = "Homogeneous (<i>k</i> = 5)"
)

prop_outbreak_control_disp_plot <- ggplot2::ggplot(
  data = prop_outbreak_control[r0_isolated == 0, ]
) +
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
  ggplot2::facet_grid(
    vars(delay), vars(disp_community),
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
    strip.text = ggtext::element_markdown(size = 12)
  )

ggplot2::ggsave(
  file.path("inst", "plots", "prop_outbreak_control_iso.png"),
  plot = prop_outbreak_control_iso_plot,
  device = "png",
  width = 250,
  height = 150,
  units = "mm",
  dpi = 300
)

ggplot2::ggsave(
  file.path("inst", "plots", "prop_outbreak_control_disp.png"),
  plot = prop_outbreak_control_disp_plot,
  device = "png",
  width = 250,
  height = 150,
  units = "mm",
  dpi = 300
)

