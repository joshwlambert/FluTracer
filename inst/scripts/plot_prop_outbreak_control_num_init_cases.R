library(ggplot2)
library(data.table)
library(dplyr)

# read simulation results saved by running inst/scripts/run_analysis.R
sweep_results <- readRDS(file.path("inst", "extdata", "simulations.rds"))

res <- sweep_results %>%
  group_by(scenario) %>%
  mutate(pext = ringbp::extinct_prob(sims[[1]], cap_cases = 500)) %>%
  ungroup(scenario)

dt <- as.data.table(res)
dt_data <- rbindlist(dt$data)
dt_data <- cbind(dt_data, scenario = dt$scenario, pext = dt$pext)

prop_outbreak_control_num_init_cases <- dt_data[
  theta == "15%" & delay == "SARS" & prop.asym == 0,
  .(index_R0, control_effectiveness, num.initial.cases, pext, subtype)
]

# convert to percentages for plotting
prop_outbreak_control_num_init_cases[, control_effectiveness := control_effectiveness * 100]
prop_outbreak_control_num_init_cases[, pext := pext * 100]

prop_outbreak_control_num_init_cases_plot <- ggplot2::ggplot(
  data = prop_outbreak_control_num_init_cases
) +
  ggplot2::geom_point(
    mapping = ggplot2::aes(
      x = control_effectiveness,
      y = pext,
      fill = as.factor(num.initial.cases),
      shape = as.factor(subtype)
    ),
    size = 3,
    stroke = 0.75
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(
      x = control_effectiveness,
      y = pext,
      colour = as.factor(num.initial.cases),
      linetype = as.factor(subtype)
    ),
    linewidth = 0.75
  ) +
  ggplot2::scale_x_continuous(
    name = "Contacts traced (%)",
    limits = c(0, 100)
  ) +
  ggplot2::scale_y_continuous(
    name = "Simulated outbreaks controlled (%)",
    limits = c(0, 100)
  ) +
  ggplot2::scale_colour_manual(values = c("#92C5DE", "#F4A582", "#CA0020")) +
  ggplot2::scale_fill_manual(values = c("#92C5DE", "#F4A582", "#CA0020")) +
  ggplot2::scale_shape_manual(values = c(21, 22, 24)) +
  ggplot2::scale_linetype_manual(values = c(1, 1, 1)) +
  ggplot2::facet_wrap(
    facets = ggplot2::vars(index_R0),
    labeller = ggplot2::as_labeller(c(
        `1.1` = "R = 1.1",
        `1.5` = "R = 1.5",
        `2.5` = "R = 2.5",
        `3.5` = "R = 3.5"
    ))
  ) +
  ggplot2::labs(
    colour = "Number of initial cases",
    fill = "Number of initial cases",
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
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold")
  )

ggplot2::ggsave(
  file.path("inst", "plots", "prop_outbreak_control_num_init_cases.png"),
  plot = prop_outbreak_control_num_init_cases_plot,
  device = "png",
  width = 250,
  height = 150,
  units = "mm",
  dpi = 300
)
