library(patchwork)

source(file.path("inst", "scripts", "plot_incubation_period.R"))
source(file.path("inst", "scripts", "plot_generation_time.R"))
source(file.path("inst", "scripts", "plot_onset_to_isolation.R"))
source(file.path("inst", "scripts", "plot_prop_presymptomatic_transmission.R"))

delay_distributions_plot <- prop_presymptomatic_transmission_plot +
  onset_to_isolation_plot +
  incubation_period_plot +
  generation_time_plot +
  plot_annotation(tag_levels = 'A')

ggplot2::ggsave(
  file.path("inst", "plots", "delay_distributions.png"),
  plot = delay_distributions_plot,
  device = "png",
  width = 300,
  height = 175,
  units = "mm",
  dpi = 300
)
