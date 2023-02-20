pacman::p_load(tidyverse, furrr, lme4, lmerTest, broom, broom.mixed, viridis)
source("./scripts/utils.R")
source("./scripts/analysis_funcs.R")
plan(multisession)

# Plotting options
theme_set(theme_bw())
theme_update(aspect.ratio = 1)
FIG_W <- 4.5
FIG_H <- 4.5

# Parameters
DROPS <- seq(0.1, 0.9, 0.1)
DROP_TYPE <- "linear"
LANG <- "en"
LENGTHS <- 4:12

# Load MECO L1 data
load("./data/meco/joint_data_L1_trimmed.rda")
L1_data <- filter_L1_data(joint.data, LANG) %>% filter(length %in% LENGTHS)

# Load entropy data
word_entropy_vectors_by_drop <-
  map(DROPS, function(drop){
    readRDS(sprintf("./output/hangman_entropy/meco_L1/entropy_data_drop_%s_%s_lang_%s.rds", drop, DROP_TYPE, LANG)) %>%
      filter(length %in% LENGTHS)
  })

# Append entropy estimates
data_by_drop <- future_map(word_entropy_vectors_by_drop, ~join_entropy_data(L1_data, .x))


# First fixation duration ------------------------------------------------------

firstfix_models <-
  future_map(data_by_drop,
             ~lmer(log_firstfix_dur ~ firstfix_entropy_z + log_freq + length + firstfix_center_diff_fct + (1|uniform_id) + (1|word),
                   data = .x))

firstfix_summaries <- map(firstfix_models, ~tidy(.x, conf.int = TRUE))

firstfix_perf <-
  data.frame("drop" = DROPS) %>%
  mutate("statistic" = map_dbl(firstfix_summaries, ~.x$statistic[2]),
         "est" = map_dbl(firstfix_summaries, ~.x$estimate[2]),
         "ci_lower" = map_dbl(firstfix_summaries, ~.x$conf.low[2]),
         "ci_upper" = map_dbl(firstfix_summaries, ~.x$conf.high[2]),
         "se" = map_dbl(firstfix_summaries, ~.x$std.error[2]))

ggplot(firstfix_perf, aes(x = drop, y = est)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
  scale_x_continuous(breaks = seq(0, 0.9, 0.1)) +
  labs(x = "drop",
       y = bquote(beta),
       title = "Drop optimization",
       subtitle = sprintf("First fixation duration (MECO L1 %s)", LANG))
ggsave(sprintf("./figures/drop_optimization/meco_L1/firstfix_coefficient_by_drop_%s.png", LANG),
       width = FIG_W + 1, height = FIG_H, units = "in")

ggplot(firstfix_perf, aes(x = drop, y = statistic)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 0.9, 0.1)) +
  labs(x = "drop",
       y = "t",
       title = "Drop optimization",
       subtitle = sprintf("First fixation duration (MECO L1 %s)", LANG))
ggsave(sprintf("./figures/drop_optimization/meco_L1/firstfix_statistic_by_drop_%s.png", LANG),
       width = FIG_W + 1, height = FIG_H, units = "in")


# Refixation rate --------------------------------------------------------------

refix_models <-
  future_map(data_by_drop,
             ~glmer(refix ~ firstfix_entropy_z + log_freq + length + firstfix_center_diff_fct + (1|uniform_id) + (1|word),
                    data = .x, family = "binomial", nAGQ = 0))

refix_summaries <- map(refix_models, ~tidy(.x, conf.int = TRUE))

refix_perf <-
  data.frame("drop" = DROPS) %>%
  mutate("statistic" = map_dbl(refix_summaries, ~.x$statistic[2]),
         "est" = map_dbl(refix_summaries, ~.x$estimate[2]),
         "ci_lower" = map_dbl(refix_summaries, ~.x$conf.low[2]),
         "ci_upper" = map_dbl(refix_summaries, ~.x$conf.high[2]),
         "se" = map_dbl(refix_summaries, ~.x$std.error[2]))

ggplot(refix_perf, aes(x = drop, y = est)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
  scale_x_continuous(breaks = seq(0, 0.9, 0.1)) +
  labs(x = "drop",
       y = bquote(beta),
       title = "Drop optimization",
       subtitle = sprintf("Refixation rate (MECO L1 %s)", LANG))
ggsave(sprintf("./figures/drop_optimization/meco_L1/refix_coefficient_by_drop_%s.png", LANG),
       width = FIG_W + 1, height = FIG_H, units = "in")

ggplot(refix_perf, aes(x = drop, y = statistic)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 0.9, 0.1)) +
  labs(x = "drop",
       y = "z",
       title = "Drop optimization",
       subtitle = sprintf("Refixation rate (MECO L1 %s)", LANG))
ggsave(sprintf("./figures/drop_optimization/meco_L1/refix_statistic_by_drop_%s.png", LANG),
       width = FIG_W + 1, height = FIG_H, units = "in")


# First fixation (by length) ---------------------------------------------------

firstfix_models_by_length <-
  future_map(data_by_drop, function(x){
    future_map(4:9, function(y){
      data <- filter(x, length == y)
      lmer(log_firstfix_dur ~ firstfix_entropy_z + log_freq + firstfix_center_diff_fct + (1|uniform_id) + (1|word), data)
    })
  })

firstfix_summaries_by_length <-
  map(firstfix_models_by_length, function(x){
    map(x, function(y) tidy(y, conf.int = TRUE))
  })

firstfix_perf_by_length <-
  data.frame(expand.grid("length" = 4:9,
                         "drop" = DROPS)) %>%
  mutate("statistic" = as.numeric(map_dfr(firstfix_summaries_by_length, function(x) data.frame(map_dbl(x, function(y) y$statistic[2])))[,1]),
         "est" = as.numeric(map_dfr(firstfix_summaries_by_length, function(x) data.frame(map_dbl(x, function(y) y$estimate[2])))[,1]),
         "ci_lower" = as.numeric(map_dfr(firstfix_summaries_by_length, function(x) data.frame(map_dbl(x, function(y) y$conf.low[2])))[,1]),
         "ci_upper" = as.numeric(map_dfr(firstfix_summaries_by_length, function(x) data.frame(map_dbl(x, function(y) y$conf.high[2])))[,1]),
         "se" = as.numeric(map_dfr(firstfix_summaries_by_length, function(x) data.frame(map_dbl(x, function(y) y$std.error[2])))[,1]))

ggplot(firstfix_perf_by_length, aes(x = drop, y = est)) +
  facet_wrap(~length, scales = "free_y") +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
  scale_x_continuous(breaks = seq(0, 0.9, 0.1)) +
  labs(x = "drop",
       y = bquote(beta),
       title = "Drop optimization",
       subtitle = sprintf("First fixation duration (MECO L1 %s)", LANG))

ggplot(firstfix_perf_by_length, aes(x = drop, y = statistic)) +
  facet_wrap(~length, scales = "free_y") +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 0.9, 0.1)) +
  labs(x = "drop",
       y = "t",
       title = "Drop optimization",
       subtitle = sprintf("First fixation duration (MECO L1 %s)", LANG))


# Refixation (by length) -------------------------------------------------------

refix_models_by_length <-
  future_map(data_by_drop, function(x){
    future_map(4:9, function(y){
      data <- filter(x, length == y)
      glmer(refix ~ firstfix_entropy_z + log_freq + firstfix_center_diff_fct + (1|uniform_id) + (1|word),
            data, family = "binomial", nAGQ = 0)
    })
  })

refix_summaries_by_length <-
  map(refix_models_by_length, function(x){
    map(x, function(y) tidy(y, conf.int = TRUE))
  })

refix_perf_by_length <-
  data.frame(expand.grid("length" = 4:9,
                         "drop" = DROPS)) %>%
  mutate("statistic" = as.numeric(map_dfr(refix_summaries_by_length, function(x) data.frame(map_dbl(x, function(y) y$statistic[2])))[,1]),
         "est" = as.numeric(map_dfr(refix_summaries_by_length, function(x) data.frame(map_dbl(x, function(y) y$estimate[2])))[,1]),
         "ci_lower" = as.numeric(map_dfr(refix_summaries_by_length, function(x) data.frame(map_dbl(x, function(y) y$conf.low[2])))[,1]),
         "ci_upper" = as.numeric(map_dfr(refix_summaries_by_length, function(x) data.frame(map_dbl(x, function(y) y$conf.high[2])))[,1]),
         "se" = as.numeric(map_dfr(refix_summaries_by_length, function(x) data.frame(map_dbl(x, function(y) y$std.error[2])))[,1]))

ggplot(refix_perf_by_length, aes(x = drop, y = est)) +
  facet_wrap(~length, scales = "free_y") +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
  scale_x_continuous(breaks = seq(0, 0.9, 0.1)) +
  labs(x = "drop",
       y = bquote(beta),
       title = "Drop optimization",
       subtitle = sprintf("Refixation rate (MECO L1 %s)", LANG))


ggplot(refix_perf_by_length, aes(x = drop, y = statistic)) +
  facet_wrap(~length, scales = "free_y") +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 0.9, 0.1)) +
  labs(x = "drop",
       y = "z",
       title = "Drop optimization",
       subtitle = sprintf("Refixation rate (MECO L1 %s)", LANG))
