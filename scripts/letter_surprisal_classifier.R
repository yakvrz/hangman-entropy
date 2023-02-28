pacman::p_load(tidyverse, furrr, data.table, viridis)
source("./scripts/classifier_functions.R")
options(future.rng.onMisuse = "ignore")

# Plotting options
theme_set(theme_bw())
theme_update(aspect.ratio = 1)
FIG_H <- 3
FIG_W <- 4

# Parameters
LANGS <- c("en", "he", "sp")
LENGTHS <- 4:9
SAMPLE_SIZES <- c(10, 25, 50, 100, 200, 300)
ITERATIONS <- 5000
CENTERING <- c("none", "length")

surp_long <-
  map_dfr(LANGS, ~readRDS(sprintf("./output/letter_surprisal/letter_surprisal_lang_%s.rds", .x))) %>%
  filter(length %in% LENGTHS) %>%
  relocate(lang) %>%
  pivot_longer(starts_with("V"), names_to = "pos", values_to = "surp", names_prefix = "V") %>%
  mutate(pos = as.numeric(pos)) %>%
  drop_na() %>%
  group_by(lang, length) %>%
  mutate(centered_surp = surp - mean(surp)) %>%
  ungroup() %>%
  pivot_longer(c("surp", "centered_surp"), names_to = "centering", values_to = "surp") %>%
  mutate(centering = factor(centering, labels = c("length", "none")))

word_surp_vectors <-
  surp_long %>%
  pivot_wider(names_from = "pos", values_from = "surp", names_prefix = "V") %>%
  mutate(value = asplit(select(., starts_with("V")), 1)) %>%
  rowwise() %>%
  mutate(value = list(as.vector(na.omit(as.numeric((value)))))) %>%
  ungroup() %>%
  select(lang, word, length, centering, value)

mean_surp_vectors <-
  surp_long %>%
  group_by(lang, length, pos, centering) %>%
  summarize(value = mean(surp)) %>%
  ungroup() %>%
  pivot_wider(names_from = "pos", values_from = "value", names_prefix = "V") %>%
  mutate(value = asplit(select(., starts_with("V")), 1)) %>%
  rowwise() %>%
  mutate(value = list(as.vector(na.omit(as.numeric(value))))) %>%
  ungroup() %>%
  select(lang, length, centering, value)


# Evaluation across languages --------------------------------------------------------------------------------

grid <- expand_grid(language = LANGS, target_length = LENGTHS, centering = CENTERING, sample_size = SAMPLE_SIZES)
plan(multisession)
perf <- future_pmap(as.list(grid), ~language_classifier(word_surp_vectors,
                                                        mean_surp_vectors,
                                                        target_lang = ..1,
                                                        target_length = ..2,
                                                        center = ..3,
                                                        sample_size = ..4,
                                                        iterations = ITERATIONS))
saveRDS(perf, sprintf("./output/classifier/letter_surprisal_classifier_perf_iter_%s.rds", ITERATIONS))
perf <- readRDS(sprintf("./output/classifier/letter_surprisal_classifier_perf_iter_%s.rds", ITERATIONS))

summarized_perf <-
  data.frame(grid) %>%
  mutate(accuracy = map2_dbl(perf, grid$language, function(errors, curr_lang){ 
    map_lgl(errors, function(error_by_lang){
      LANGS[which_min_break_ties(error_by_lang)] == curr_lang
    }) %>% mean()
  })) %>%
  group_by(target_length, sample_size, centering) %>%
  summarize(mean_accuracy = mean(accuracy)) %>%
  ungroup()

# Plot accuracy by sample size (non-centered)
summarized_perf %>%
  mutate(target_length = as.factor(target_length)) %>%
  filter(centering == "none") %>%
  ggplot(aes(x = sample_size, y = mean_accuracy, color = target_length)) +
  geom_line(linewidth = .4) +
  geom_point(size = .6) +
  scale_color_discrete(name = "length") +
  ylim(0.6, 1) +
  labs(
    # title = "Classifier performance",
    # subtitle = sprintf("predictor: letter transition surprisal\niterations: %s", ITERATIONS),
    x = "sample size",
    y = "mean accuracy")
ggsave(sprintf("./figures/classifier/letter_surprisal_classifier_iter_%s_noncentered.svg", ITERATIONS),
       height = FIG_H, width = FIG_W, units = "in", scale = 0.9)

# Plot accuracy by sample size (length-centered)
summarized_perf %>%
  mutate(target_length = as.factor(target_length)) %>%
  filter(centering == "length") %>%
  ggplot(aes(x = sample_size, y = mean_accuracy, color = target_length)) +
  geom_line(linewidth = .4) +
  geom_point(size = .6) +
  scale_color_discrete(name = "length") +
  ylim(0.6, 1) +
  labs(title = "Classifier performance",
       subtitle = sprintf("predictor: letter transition surprisal (length-centered)\niterations: %s", ITERATIONS),
       x = "sample size",
       y = "mean accuracy")
ggsave("./Figures/Classifier/letter_surprisal_classifier_centered.pdf", height = FIG_H, width = FIG_W, units = "in")
