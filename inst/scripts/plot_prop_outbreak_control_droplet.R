library(data.table)
library(ggplot2)

# read simulation results saved by running inst/scripts/run_droplet_analysis.R

results <- readRDS(file.path("inst", "extdata", "simulations_droplet.rds"))

results[, pext := ringbp::extinct_prob(sims[[1]], extinction_week = 12), by = scenario]

flu_data <- rbindlist(results$data)
flu_data[, `:=`(scenario = results$scenario, pext = results$pext)]

rm(results)

# no subsetting needed as droplet analysis only explores the plotted parameter space
prop_outbreak_control <- flu_data[,
  .(prop_ascertain, r0_community, pext, subtype, delay, prop_asymptomatic)
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
  "0" = "0% asymptomatic",
  "0.1" = "10% asymptomatic",
  "0.3" = "30% asymptomatic"
)

prop_outbreak_control_droplet <- ggplot2::ggplot(data = prop_outbreak_control) +
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
    vars(delay), vars(prop_asymptomatic),
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
    linetype = "Pathogen Subtype",
    title = expression("Droplet Transmission (" * R[0]^asym * " = 0.5)")
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_legend(
      override.aes = list(shape = 21)
    )
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "bottom",
    legend.box = "vertical",
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold", size = 12)
  )

ggplot2::ggsave(
  file.path("inst", "plots", "prop_outbreak_control_droplet.png"),
  plot = prop_outbreak_control_droplet,
  device = "png",
  width = 250,
  height = 250,
  units = "mm",
  dpi = 300
)

