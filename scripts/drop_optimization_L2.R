pacman::p_load(tidyverse, furrr, lme4, lmerTest, broom, broom.mixed, viridis)
source("./scripts/utility_functions.R")
source("./scripts/analysis_functions.R")
plan(multisession)

# Plotting options
theme_set(theme_bw())
theme_update(aspect.ratio = 1)
FIG_W <- 4.5
FIG_H <- 4.5

# Parameters
DROPS <- seq(0.05, 0.95, 0.05)
DROP_TYPE <- "linear"
LENGTHS <- 7:12

# Load MECO L2 data
load("./data/meco/joint_data_L2_trimmed.rda")
L2_data <- filter_L2_data(joint.data) %>% filter(length %in% LENGTHS)

# Load entropy data
word_entropy_vectors_by_drop <-
  map(DROPS, function(drop){
    readRDS(sprintf("./output/hangman_entropy/meco_words/entropy_data_drop_%s_%s_lang_en.rds", drop, DROP_TYPE)) %>%
      filter(length %in% LENGTHS)
  })

# Append entropy estimates
data_by_drop <- future_map(word_entropy_vectors_by_drop, ~join_entropy_data(L2_data, .x))


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
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  labs(x = "drop",
       y = bquote(beta),
       title = "Drop optimization",
       subtitle = "First fixation duration (MECO L2)")
ggsave(sprintf("./figures/drop_optimization/meco_L2/firstfix_coefficient_by_drop_lengths_%s-%s.png", min(LENGTHS), max(LENGTHS)),
       width = FIG_W + 1, height = FIG_H, units = "in")

ggplot(firstfix_perf, aes(x = drop, y = statistic)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  labs(x = "drop",
       y = "t",
       title = "Drop optimization",
       subtitle = "First fixation duration (MECO L2)")
ggsave(sprintf("./figures/drop_optimization/meco_L2/firstfix_statistic_by_drop_lengths_%s-%s.png", min(LENGTHS), max(LENGTHS)),
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
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  labs(x = "drop",
       y = bquote(beta),
       title = "Drop optimization",
       subtitle = "Refixation rate (MECO L2)")
ggsave(sprintf("./figures/drop_optimization/meco_L2/refix_coefficient_by_drop_lengths_%s-%s.png", min(LENGTHS), max(LENGTHS)),
       width = FIG_W + 1, height = FIG_H, units = "in")

ggplot(refix_perf, aes(x = drop, y = statistic)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  labs(x = "drop",
       y = "z",
       title = "Drop optimization",
       subtitle = "Refixation rate (MECO L2)")
ggsave(sprintf("./figures/drop_optimization/meco_L2/refix_statistic_by_drop_lengths_%s-%s.png", min(LENGTHS), max(LENGTHS)),
       width = FIG_W + 1, height = FIG_H, units = "in")


# # First fixation (by length) ---------------------------------------------------
# 
# firstfix_models <-
#   future_map(data_by_drop, function(x){
#     future_map(LENGTHS, function(y){
#       data <- filter(x, length == y)
#       lmer(log_firstfix_dur ~ firstfix_entropy + log_freq + firstfix_center_diff + (1|uniform_id) + (1|word), data)
#     })
#   })
# 
# firstfix_summaries <-
#   map(firstfix_models, function(x){
#     map(x, function(y) tidy(y, conf.int = TRUE))
#   })
# 
# firstfix_perf <-
#   data.frame(expand.grid("length" = LENGTHS,
#                          "drop" = DROPS)) %>%
#   mutate("statistic" = as.numeric(map_dfr(firstfix_summaries, function(x) data.frame(map_dbl(x, function(y) y$statistic[2])))[,1]),
#          "est" = as.numeric(map_dfr(firstfix_summaries, function(x) data.frame(map_dbl(x, function(y) y$estimate[2])))[,1]),
#          "ci_lower" = as.numeric(map_dfr(firstfix_summaries, function(x) data.frame(map_dbl(x, function(y) y$conf.low[2])))[,1]),
#          "ci_upper" = as.numeric(map_dfr(firstfix_summaries, function(x) data.frame(map_dbl(x, function(y) y$conf.high[2])))[,1]),
#          "se" = as.numeric(map_dfr(firstfix_summaries, function(x) data.frame(map_dbl(x, function(y) y$std.error[2])))[,1]))
# 
# ggplot(firstfix_perf, aes(x = drop, y = est)) +
#   facet_wrap(~length, scales = "free_y") +
#   geom_line() +
#   geom_point() +
#   geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
#   labs(x = "Drop",
#        y = "Estimate",
#        title = "Drop optimization on first fixation duration (MECO L2)") +
#   scale_x_continuous(breaks = seq(0, 0.9, 0.1)) +
#   theme(aspect.ratio = 1)
# 
# ggplot(firstfix_perf, aes(x = drop, y = statistic)) +
#   facet_wrap(~length, scales = "free_y") +
#   geom_line() +
#   geom_point() +
#   labs(x = "Drop",
#        y = "t",
#        title = "Drop optimization on first fixation duration (MECO L2)") +
#   scale_x_continuous(breaks = seq(0, 0.9, 0.1)) +
#   theme(aspect.ratio = 1)
# 
# 
# # Refixation (by length) -------------------------------------------------------
# 
# refix_models <-
#   future_map(data_by_drop, function(x){
#     future_map(LENGTHS, function(y){
#       data <- filter(x, length == y)
#       glmer(refix ~ firstfix_entropy + log_freq + firstfix_center_diff + (1|uniform_id) + (1|word),
#             data, family = "binomial", nAGQ = 0)
#     })
#   })
# 
# refix_summaries <-
#   map(refix_models, function(x){
#     map(x, function(y) tidy(y, conf.int = TRUE))
#   })
# 
# refix_perf <-
#   data.frame(expand.grid("length" = LENGTHS,
#                          "drop" = DROPS)) %>%
#   mutate("statistic" = as.numeric(map_dfr(refix_summaries, function(x) data.frame(map_dbl(x, function(y) y$statistic[2])))[,1]),
#          "est" = as.numeric(map_dfr(refix_summaries, function(x) data.frame(map_dbl(x, function(y) y$estimate[2])))[,1]),
#          "ci_lower" = as.numeric(map_dfr(refix_summaries, function(x) data.frame(map_dbl(x, function(y) y$conf.low[2])))[,1]),
#          "ci_upper" = as.numeric(map_dfr(refix_summaries, function(x) data.frame(map_dbl(x, function(y) y$conf.high[2])))[,1]),
#          "se" = as.numeric(map_dfr(refix_summaries, function(x) data.frame(map_dbl(x, function(y) y$std.error[2])))[,1]))
# 
# ggplot(refix_perf, aes(x = drop, y = est)) +
#   facet_wrap(~length, scales = "free_y") +
#   geom_line() +
#   geom_point() +
#   geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
#   labs(x = "Drop",
#        y = "Estimate",
#        title = "Drop optimization on refixation (MECO L2)") +
#   scale_x_continuous(breaks = seq(0, 0.9, 0.1)) +
#   theme(aspect.ratio = 1)
# 
# ggplot(refix_perf, aes(x = drop, y = statistic)) +
#   facet_wrap(~length, scales = "free_y") +
#   geom_line() +
#   geom_point() +
#   labs(x = "Drop",
#        y = "z",
#        title = "Drop optimization on refixation (MECO L2)") +
#   scale_x_continuous(breaks = seq(0, 0.9, 0.1)) +
#   theme(aspect.ratio = 1)
