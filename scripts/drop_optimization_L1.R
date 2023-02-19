# First fixation (L1) ---------------------------------------------------------------------------------------------
firstfix_models_L1 <- future_map(L1_data_list, ~fit_firstfix_model(.x))
firstfix_summaries_L1 <- map(firstfix_models_L1, ~tidy(.x, conf.int = TRUE))

firstfix_est_L1 <-
  data.frame("drop" = drop_vec) %>%
  mutate(est = map_dbl(firstfix_summaries_L1, ~.x$estimate[2]),
         ci_lower = map_dbl(firstfix_summaries_L1, ~.x$conf.low[2]),
         ci_upper = map_dbl(firstfix_summaries_L1, ~.x$conf.high[2]))

ggplot(firstfix_est_L1, aes(x = drop, y = est)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
  labs(x = "Drop", y = "Estimate", title = "First fixation entropy coefficient, by drop",
       subtitle = sprintf("First fixation model, skew = %s, L1 (%s)", skew, language)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  theme(aspect.ratio = 1)
ggsave(sprintf("./Figures/Drop/L1/%s/drop_opt_firstfix_skew_%s_L1_%s.pdf", language, skew, language))


# First run (L1) --------------------------------------------------------------------------------------------------
firstrun_models_L1 <- future_map(L1_data_list, ~fit_firstrun_model(.x))
firstrun_summaries_L1 <- map(firstrun_models_L1, ~tidy(.x, conf.int = TRUE))

firstrun_est_L1 <-
  data.frame("drop" = drop_vec) %>%
  mutate(est = map_dbl(firstrun_summaries_L1, ~.x$estimate[2]),
         ci_lower = map_dbl(firstrun_summaries_L1, ~.x$conf.low[2]),
         ci_upper = map_dbl(firstrun_summaries_L1, ~.x$conf.high[2]))

ggplot(firstrun_est_L1, aes(x = drop, y = est)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
  labs(x = "Drop", y = "Estimate", title = "First fixation entropy coefficient, by drop",
       subtitle = sprintf("First run model, skew = %s, L1 (%s)", skew, language)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  theme(aspect.ratio = 1)
ggsave(sprintf("./Figures/Drop/L1/%s/drop_opt_firstrun_skew_%s_L1_%s.pdf", language, skew, language))


# Refixation (L1) -------------------------------------------------------------------------------------------------
refix_models_L1 <- future_map(L1_data_list, ~fit_refix_model(.x))
refix_summaries_L1 <- map(refix_models_L1, ~tidy(.x, conf.int = TRUE))

refix_est_L1 <- 
  data.frame("drop" = drop_vec) %>%
  mutate(est = map_dbl(refix_summaries_L1, ~.x$estimate[2]),
         ci_lower = map_dbl(refix_summaries_L1, ~.x$conf.low[2]),
         ci_upper = map_dbl(refix_summaries_L1, ~.x$conf.high[2]))

ggplot(refix_est_L1, aes(x = drop, y = est)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
  labs(x = "Drop", y = "Estimate", title = "First fixation entropy coefficient, by drop",
       subtitle = sprintf("Refixation model, skew = %s, L1 (%s)", skew, language)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  theme(aspect.ratio = 1)
ggsave(sprintf("./Figures/Drop/L1/%s/drop_opt_refix_skew_%s_L1_%s.pdf", language, skew, language))
