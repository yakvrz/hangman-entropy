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

# Load MECO L2 data
load("./data/meco/joint_data_L1_trimmed.rda")
L1_data <- filter_L1_data(joint.data, LANG) %>% filter(length %in% LENGTHS)

# Load entropy data
word_entropy_vectors_by_drop <-
  map(DROPS, function(DROP){
    readRDS(sprintf("./output/hangman_entropy/meco_L1/entropy_data_drop_%s_%s_lang_%s.rds", DROP, DROP_TYPE, LANG)) %>%
      filter(length %in% LENGTHS)
  })

# Append entropy estimates
data_by_drop <- future_map(word_entropy_vectors_by_drop, ~join_entropy_data(L1_data, .x))


# First fixation (fct center diff) ---------------------------------------------------------------------------------------------

firstfix_models <-
  future_map(data_by_drop,
             ~lmer(log_firstfix_dur ~ firstfix_entropy + log_freq + length + firstfix_center_diff_fct + (1|uniform_id) + (1|word),
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
  labs(x = "Drop",
       y = "Estimate",
       title = "Drop optimization",
       subtitle = sprintf("First fixation duration (MECO L1 %s)", LANG),
       caption = as.character(firstfix_models[[1]]@call$formula)[3]) +
  theme(aspect.ratio = 1,
        plot.caption = element_text(size = 7, hjust = 0))
ggsave(sprintf("./figures/drop_optimization/meco_L1/firstfix_coefficient_by_drop_diff_fct_L1_%s.png", LANG),
       width = FIG_W + 1, height = FIG_H, units = "in")

ggplot(firstfix_perf, aes(x = drop, y = statistic)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 0.9, 0.1)) +
  labs(x = "Drop",
       y = "t",
       title = "Drop optimization",
       subtitle = sprintf("First fixation duration (MECO L1 %s)", LANG),
       caption = as.character(firstfix_models[[1]]@call$formula)[3]) +
  theme(aspect.ratio = 1,
        plot.caption = element_text(size = 7, hjust = 0))
ggsave(sprintf("./figures/drop_optimization/meco_L1/firstfix_statistic_by_drop_diff_fct_L1_%s.png", LANG),
       width = FIG_W + 1, height = FIG_H, units = "in")


# First fixation (numeric center diff) ---------------------------------------------------------------------------------------------

firstfix_models <-
  future_map(data_by_drop,
             ~lmer(log_firstfix_dur ~ firstfix_entropy + log_freq + length + firstfix_center_diff + (1|uniform_id) + (1|word),
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
  labs(x = "Drop",
       y = "Estimate",
       title = "Drop optimization",
       subtitle = sprintf("First fixation duration (MECO L1 %s)", LANG),
       caption = as.character(firstfix_models[[1]]@call$formula)[3]) +
  theme(aspect.ratio = 1,
        plot.caption = element_text(size = 7, hjust = 0))
ggsave(sprintf("./figures/drop_optimization/meco_L1/firstfix_coefficient_by_drop_diff_numeric_L1_%s.png", LANG),
       width = FIG_W + 1, height = FIG_H, units = "in")

ggplot(firstfix_perf, aes(x = drop, y = statistic)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 0.9, 0.1)) +
  labs(x = "Drop",
       y = "t",
       title = "Drop optimization",
       subtitle = sprintf("First fixation duration (MECO L1 %s)", LANG),
       caption = as.character(firstfix_models[[1]]@call$formula)[3]) +
  theme(aspect.ratio = 1,
        plot.caption = element_text(size = 7, hjust = 0))
ggsave(sprintf("./figures/drop_optimization/meco_L1/firstfix_statistic_by_drop_diff_numeric_L1_%s.png", LANG),
       width = FIG_W + 1, height = FIG_H, units = "in")


# Refixation (center diff fct) -------------------------------------------------------------------------------------------------

refix_models <-
  future_map(data_by_drop,
             ~glmer(refix ~ firstfix_entropy + log_freq + length + firstfix_center_diff_fct + (1|uniform_id) + (1|word),
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
  labs(x = "Drop",
       y = "t",
       title = "Drop optimization",
       subtitle = sprintf("Refixation (MECO L1 %s)", LANG),
       caption = as.character(refix_models[[1]]@call$formula)[3]) +
  theme(aspect.ratio = 1,
        plot.caption = element_text(size = 7, hjust = 0))
ggsave(sprintf("./figures/drop_optimization/meco_L1/refix_coefficient_by_drop_diff_fct_L1_%s.png", LANG),
       width = FIG_W + 1, height = FIG_H, units = "in")

ggplot(refix_perf, aes(x = drop, y = statistic)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 0.9, 0.1)) +
  labs(x = "Drop",
       y = "z",
       title = "Drop optimization",
       subtitle = sprintf("Refixation (MECO L1 %s)", LANG),
       caption = as.character(refix_models[[1]]@call$formula)[3]) +
  theme(aspect.ratio = 1,
        plot.caption = element_text(size = 7, hjust = 0))
ggsave(sprintf("./figures/drop_optimization/meco_L1/refix_statistic_by_drop_diff_fct_L1_%s.png", LANG),
       width = FIG_W + 1, height = FIG_H, units = "in")

# Refixation (center diff numeric) -------------------------------------------------------------------------------------------------

refix_models <-
  future_map(data_by_drop,
             ~glmer(refix ~ firstfix_entropy + log_freq + length + firstfix_center_diff + (1|uniform_id) + (1|word),
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
  labs(x = "Drop",
       y = "t",
       title = "Drop optimization",
       subtitle = sprintf("Refixation (MECO L1 %s)", LANG),
       caption = as.character(refix_models[[1]]@call$formula)[3]) +
  theme(aspect.ratio = 1,
        plot.caption = element_text(size = 7, hjust = 0))
ggsave(sprintf("./figures/drop_optimization/meco_L1/refix_coefficient_by_drop_diff_numeric_L1_%s.png", LANG),
       width = FIG_W + 1, height = FIG_H, units = "in")

ggplot(refix_perf, aes(x = drop, y = statistic)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 0.9, 0.1)) +
  labs(x = "Drop",
       y = "z",
       title = "Drop optimization",
       subtitle = sprintf("Refixation (MECO L1 %s)", LANG),
       caption = as.character(refix_models[[1]]@call$formula)[3]) +
  theme(aspect.ratio = 1,
        plot.caption = element_text(size = 7, hjust = 0))
ggsave(sprintf("./figures/drop_optimization/meco_L1/refix_statistic_by_drop_diff_numeric_L1_%s.png", LANG),
       width = FIG_W + 1, height = FIG_H, units = "in")



# First fixation (by length) ---------------------------------------------------

firstfix_models <-
  future_map(data_by_drop, function(x){
    future_map(LENGTHS, function(y){
      data <- filter(x, length == y)
      lmer(log_firstfix_dur ~ firstfix_entropy + log_freq + firstfix_center_diff + (1|uniform_id) + (1|word), data)
    })
  })

firstfix_summaries <-
  map(firstfix_models, function(x){
    map(x, function(y) tidy(y, conf.int = TRUE))
  })

firstfix_perf <-
  data.frame(expand.grid("length" = LENGTHS,
                         "drop" = DROPS)) %>%
  mutate("statistic" = as.numeric(map_dfr(firstfix_summaries, function(x) data.frame(map_dbl(x, function(y) y$statistic[2])))[,1]),
         "est" = as.numeric(map_dfr(firstfix_summaries, function(x) data.frame(map_dbl(x, function(y) y$estimate[2])))[,1]),
         "ci_lower" = as.numeric(map_dfr(firstfix_summaries, function(x) data.frame(map_dbl(x, function(y) y$conf.low[2])))[,1]),
         "ci_upper" = as.numeric(map_dfr(firstfix_summaries, function(x) data.frame(map_dbl(x, function(y) y$conf.high[2])))[,1]),
         "se" = as.numeric(map_dfr(firstfix_summaries, function(x) data.frame(map_dbl(x, function(y) y$std.error[2])))[,1]))

ggplot(firstfix_perf, aes(x = drop, y = est)) +
  facet_wrap(~length, scales = "free_y") +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
  labs(x = "Drop",
       y = "Estimate",
       title = "Drop optimization on first fixation duration (MECO L2)") +
  scale_x_continuous(breaks = seq(0, 0.9, 0.1)) +
  theme(aspect.ratio = 1)

ggplot(firstfix_perf, aes(x = drop, y = statistic)) +
  facet_wrap(~length, scales = "free_y") +
  geom_line() +
  geom_point() +
  labs(x = "Drop",
       y = "t",
       title = "Drop optimization on first fixation duration (MECO L2)") +
  scale_x_continuous(breaks = seq(0, 0.9, 0.1)) +
  theme(aspect.ratio = 1)


# Refixation (by length) -------------------------------------------------------

refix_models <-
  future_map(data_by_drop, function(x){
    future_map(LENGTHS, function(y){
      data <- filter(x, length == y)
      glmer(refix ~ firstfix_entropy + log_freq + firstfix_center_diff + (1|uniform_id) + (1|word),
            data, family = "binomial", nAGQ = 0)
    })
  })

refix_summaries <-
  map(refix_models, function(x){
    map(x, function(y) tidy(y, conf.int = TRUE))
  })

refix_perf <-
  data.frame(expand.grid("length" = LENGTHS,
                         "drop" = DROPS)) %>%
  mutate("statistic" = as.numeric(map_dfr(refix_summaries, function(x) data.frame(map_dbl(x, function(y) y$statistic[2])))[,1]),
         "est" = as.numeric(map_dfr(refix_summaries, function(x) data.frame(map_dbl(x, function(y) y$estimate[2])))[,1]),
         "ci_lower" = as.numeric(map_dfr(refix_summaries, function(x) data.frame(map_dbl(x, function(y) y$conf.low[2])))[,1]),
         "ci_upper" = as.numeric(map_dfr(refix_summaries, function(x) data.frame(map_dbl(x, function(y) y$conf.high[2])))[,1]),
         "se" = as.numeric(map_dfr(refix_summaries, function(x) data.frame(map_dbl(x, function(y) y$std.error[2])))[,1]))

ggplot(refix_perf, aes(x = drop, y = est)) +
  facet_wrap(~length, scales = "free_y") +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
  labs(x = "Drop",
       y = "Estimate",
       title = "Drop optimization on refixation (MECO L2)") +
  scale_x_continuous(breaks = seq(0, 0.9, 0.1)) +
  theme(aspect.ratio = 1)

ggplot(refix_perf, aes(x = drop, y = statistic)) +
  facet_wrap(~length, scales = "free_y") +
  geom_line() +
  geom_point() +
  labs(x = "Drop",
       y = "z",
       title = "Drop optimization on refixation (MECO L2)") +
  scale_x_continuous(breaks = seq(0, 0.9, 0.1)) +
  theme(aspect.ratio = 1)
