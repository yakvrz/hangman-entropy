pacman::p_load(tidyverse, furrr, lme4, lmerTest, broom, broom.mixed, viridis)
source("./scripts/utility_functions.R")
source("./scripts/analysis_functions.R")
plan(multisession)
options(future.globals.maxSize = 8000 * 1024^2)

# Plotting options
theme_set(theme_bw())
theme_update(aspect.ratio = 1)
FIG_W <- 4.5
FIG_H <- 4.5

# Parameters
DROPS <- seq(0.1, 0.9, 0.1)
DROP_TYPE <- "linear"
LENGTHS <- 4:12

# Load MECO L2 data
load("./data/meco/joint_data_L2_trimmed.rda")
L2_data <- filter_L2_data(joint.data) %>% filter(length %in% LENGTHS)
rm(joint.data)


# Subject-level drop optimization ----------------------------------------------

# Load entropy data
word_entropy_vectors_by_drop <-
  map(DROPS, function(drop){
    readRDS(sprintf("./output/hangman_entropy/meco_words/entropy_data_drop_%s_%s_lang_en.rds", drop, DROP_TYPE)) %>%
      filter(length %in% LENGTHS)
  }, .progress = TRUE)

# Append entropy estimates
plan(multisession)
data_by_drop <- future_map(word_entropy_vectors_by_drop, ~join_entropy_data(L2_data, .x))

# Calculate optimal drop per subject
subject_drop_optim <-
  data.frame("uniform_id" = unique(L2_data$uniform_id)) %>%
  mutate(model_summaries = map(uniform_id, ~optimize_subject_drop(data_by_drop, DROPS, .x, "firstfix"), .progress = TRUE),
         firstfix_perf = map2(uniform_id, model_summaries, function(id, summary){
           data.frame("drop" = DROPS,
                      "uniform_id" = id) %>%
             mutate("statistic" = map_dbl(summary, ~.x$statistic[2]),
                    "est" = map_dbl(summary, ~.x$estimate[2]),
                    "ci_lower" = map_dbl(summary, ~.x$conf.low[2]),
                    "ci_upper" = map_dbl(summary, ~.x$conf.high[2]),
                    "se" = map_dbl(summary, ~.x$std.error[2]))
         }))

subject_firstfix_perf <-
  map_dfr(subject_drop_optim$firstfix_perf, ~.x) %>%
  filter(uniform_id %in% unique(uniform_id)[which(str_detect(unique(uniform_id), "en_"))]) %>%
  filter(uniform_id %in% head(unique(uniform_id), 12))

ggplot(subject_firstfix_perf, aes(x = drop, y = statistic)) +
  facet_wrap(~uniform_id) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  labs(x = "drop",
       y = "t statistic",
       title = "Drop optimization",
       subtitle = "First fixation duration (MECO L2)")
ggsave("./figures/drop_optimization/meco_L2/firstfix_coefficient_by_drop.png", width = FIG_W + 1, height = FIG_H, units = "in")


# Individual differences analysis ----------------------------------------------

# Load MECO L2 individual differences data
load("./data/meco/joint_ind_diff_L2.rda")
ind_diff_data <-
  joint_id %>%
  unique() %>%
  drop_na() %>%
  mutate(across(c(TOWRE_word, TOWRE_nonword), as.numeric),
         across(c(spelling, motiv, lextale, TOWRE_word, TOWRE_nonword, vocab, vocab.t2.5, cft20), my_scale)) %>%
  rowwise() %>%
  mutate(comp_score = mean(c(spelling, motiv, lextale, TOWRE_word, TOWRE_nonword, vocab, vocab.t2.5, cft20)),
         vocab_score = mean(c(vocab, vocab.t2.5)),
         nonvocab_score = mean(c(spelling, motiv, lextale, TOWRE_word, TOWRE_nonword, cft20))) %>%
  ungroup() %>%
  select(-subid) %>%
  inner_join(subject_drop_optim)

m <- lm(optim_drop_firstfix ~ .,
        data = ind_diff_data %>% select(-uniform_id, -lang, -comp_score, -vocab_score, -nonvocab_score))
summary(m)
plot_model(m, type = "eff", terms = "vocab.t2.5")


data_1 <-
  data_by_drop[[1]] %>%
  inner_join(ind_diff_data)

m1 <- glmer(refix ~ firstfix_entropy + log_freq + length + firstfix_center_diff
            + (1|word) + (1|uniform_id),
            data = data_1, family = "binomial", nAGQ = 0)
summary(m1)

m2 <- glmer(refix ~ firstfix_entropy * vocab.t2.5 + log_freq + length + firstfix_center_diff
            + (1|word) + (1|uniform_id),
            data = data_1, family = "binomial", nAGQ = 0)
summary(m2)

m3 <- glmer(refix ~ firstfix_entropy + vocab_score + nonvocab_score + log_freq + length
            + firstfix_center_diff + (1|word) + (1|uniform_id),
            data = data_1, family = "binomial", nAGQ = 0)
summary(m3)

m4 <- glmer(refix ~ firstfix_entropy * (vocab_score + nonvocab_score) + log_freq + length
            + firstfix_center_diff + (1|word) + (1|uniform_id),
            data = data_1, family = "binomial", nAGQ = 0)
summary(m4)


