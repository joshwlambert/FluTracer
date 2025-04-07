# read simulation results saved by running inst/scripts/run_analysis.R

sweep_results <- readRDS(file.path("inst", "extdata", "simulations.rds"))

res <- sweep_results %>%
  group_by(scenario) %>%
  mutate(pext = ringbp::extinct_prob(sims[[1]], cap_cases = 5000)) %>%
  ungroup(scenario)

dt <- as.data.table(res)

dt_data <- rbindlist(dt$data)

dt_data <- cbind(dt_data, scenario = dt$scenario, pext = dt$pext)

prop_outbreaks_control <- dt_data %>%
  dplyr::filter(num.initial.cases == 20,
                theta == "15%",
                delay == "SARS",
                prop.asym == 0) %>%
  dplyr::select(control_effectiveness, index_R0, pext) %>%
  ggplot2::ggplot(ggplot2::aes(x = control_effectiveness,
                               y = pext,
                               color = as.factor(index_R0))) +
  ggplot2::geom_line(size = 0.75) +
  ggplot2::geom_point(shape = 21,
                      col = "black",
                      ggplot2::aes(fill = as.factor(index_R0)),
                      size = 3) +
  ggplot2::scale_fill_manual(guide = "none",
                             values = c("red", "black", "firebrick4")) +
  ggplot2::scale_color_manual(values = c("red", "black", "firebrick4"),
                              name = "Reproduction\nnumber")  +
  cowplot::theme_cowplot() +
  ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.2),
                              labels = seq(0, 100, 20)) +
  ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.2),
                              labels = seq(0, 100, 20)) +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::labs(tag = "A",
                x = "Contacts traced (%)",
                y = "Simulated outbreaks controlled (%)")

ggplot2::ggsave(file.path("inst", "plots", "prop_outbreaks_control.png"), plot = prop_outbreaks_control)

