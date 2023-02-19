setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); rm(list=ls()); gc() 
pacman::p_load(tidyverse, furrr, lme4, lmerTest, broom, broom.mixed, viridis)
source("utils.R")
source("analysis_funcs.R")

# Plotting options
theme_set(theme_bw())
theme_update(aspect.ratio = 1)
FIG_W <- 4.5
FIG_H <- 4.5

# Parameters
DROPS <- seq(0.1, 0.9, 0.1)
DROP_TYPE <- "quadratic"
LANG <- "en"
LENGTHS <- 4:12

# Load MECO L2 data
load("./Datasets/MECO/joint_data_L2_trimmed.rda")
L2_data <- filter_L2_data(joint.data) %>% filter(length %in% LENGTHS)

# Load entropy data
word_entropy_vectors_by_drop <-
  map(DROPS, function(drop){
    readRDS(sprintf("./Output/HangmanEntropy/%s/entropy_drop_%s_%s_lang_%s.rds", LANG, drop, DROP_TYPE, LANG)) %>%
      filter(length %in% LENGTHS) %>%
      rowwise() %>%
      mutate(entropy = list(as.vector(na.omit(as.numeric(entropy))))) %>%
      ungroup()
  })

# Append entropy estimates
plan(multisession)
data_by_drop <- future_map(word_entropy_vectors_by_drop, ~join_entropy_data(L2_data, .x))


# First fixation ---------------------------------------------------------------------------------------------

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
  labs(x = "Drop",
       y = "Estimate",
       title = "Drop optimization on first fixation duration (MECO L2)") +
  scale_x_continuous(breaks = seq(0, 0.9, 0.1)) +
  theme(aspect.ratio = 1)

ggplot(firstfix_perf, aes(x = drop, y = statistic)) +
  geom_line() +
  geom_point() +
  labs(x = "Drop",
       y = "t",
       title = "Drop optimization on first fixation duration (MECO L2)") +
  scale_x_continuous(breaks = seq(0, 0.9, 0.1)) +
  theme(aspect.ratio = 1)


# Refixation -------------------------------------------------------------------------------------------------

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
  labs(x = "Drop",
       y = "Estimate",
       title = "Drop optimization on refixation (MECO L2)") +
  scale_x_continuous(breaks = seq(0, 0.9, 0.1)) +
  theme(aspect.ratio = 1)

ggplot(refix_perf, aes(x = drop, y = statistic)) +
  geom_line() +
  geom_point() +
  labs(x = "Drop",
       y = "t",
       title = "Drop optimization on refixation (MECO L2)") +
  scale_x_continuous(breaks = seq(0, 0.9, 0.1)) +
  theme(aspect.ratio = 1)


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
