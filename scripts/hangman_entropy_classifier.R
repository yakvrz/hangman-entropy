setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); rm(list=ls()); gc()
pacman::p_load(tidyverse, furrr, data.table, viridis)
source("classifier_funcs.R")
options(future.rng.onMisuse = "ignore")

# Plotting options
theme_set(theme_bw())
theme_update(aspect.ratio = 1)
FIG_H <- 4.5
FIG_W <- 6.5

# Parameters
LANGS <- c("en", "he", "sp")
LENGTHS <- 4:9
SAMPLE_SIZES <- c(10, 25, 50, 100, 200, 300)
ITERATIONS <- 5000
DROP <- 0.1
CENTERING <- c("none", "length")

entropy_long <-
  readRDS(sprintf("./Output/HangmanEntropy/entropy_long_drop_%s.rds", DROP)) %>%
  filter(centering %in% CENTERING,
         length %in% LENGTHS)

word_entropy_vectors <-
  entropy_long %>%
  pivot_wider(names_from = "pos", values_from = "entropy", names_prefix = "V") %>%
  mutate(value = asplit(select(., starts_with("V")), 1)) %>%
  rowwise() %>%
  mutate(value = list(as.vector(na.omit(as.numeric(value))))) %>%
  ungroup() %>%
  select(lang, length, word, centering, value)

mean_entropy_vectors <-
  entropy_long %>%
  group_by(lang, length, pos, centering) %>%
  summarize(value = mean(entropy)) %>%
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
perf <- future_pmap(as.list(grid), ~language_classifier(word_entropy_vectors,
                                                        mean_entropy_vectors,
                                                        target_lang = ..1,
                                                        target_length = ..2,
                                                        center = ..3,
                                                        sample_size = ..4,
                                                        iterations = ITERATIONS))
saveRDS(perf, "./Output/Classifier/hangman_entropy_classifier_perf.rds")

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
  labs(title = "Classifier performance",
       subtitle = sprintf("predictor: orthographic-lexical entropy\ndrop: %s\niterations: %s", DROP, ITERATIONS),
       x = "sample size",
       y = "mean accuracy")
ggsave("./Figures/Classifier/hangman_entropy_classifier.pdf", height = FIG_H, width = FIG_W, units = "in")

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
       subtitle = sprintf("predictor: orthographic-lexical entropy (length-centered)\ndrop: %s\niterations: %s", DROP, ITERATIONS),
       x = "sample size",
       y = "mean accuracy")
ggsave("./Figures/Classifier/hangman_entropy_classifier_centered.pdf", height = FIG_H, width = FIG_W, units = "in")


# Diagnostics ------------------------------------------------------------------------------------------------

grid <- expand_grid(language = LANGS, target_length = LENGTHS, centering = CENTERING, sample_size = SAMPLE_SIZES)
plan(multisession)
perf <- future_pmap(as.list(grid), ~language_classifier_diagnostics(word_entropy_vectors,
                                                       mean_entropy_vectors,
                                                       target_lang = ..1,
                                                       target_length = ..2,
                                                       center = ..3,
                                                       sample_size = ..4,
                                                       iterations = ITERATIONS))

accuracy_by_error_type <-
  map2(perf, grid$language, function(all_errors, curr_lang){
    map(all_errors, function(errors){
      map_lgl(errors, function(error_by_lang){
        LANGS[which_min_break_ties(error_by_lang)] == curr_lang
      })
    }) %>% as.data.frame() %>% rowMeans()
  })

mean_accuracy_by_error_type <- data.frame("word_MSE" = map_dbl(accuracy_by_error_type, ~.x[["word_MSE"]]),
                                          "sample_MSE" = map_dbl(accuracy_by_error_type, ~.x[["sample_MSE"]]),
                                          "word_MAE" = map_dbl(accuracy_by_error_type, ~.x[["word_MAE"]]),
                                          "sample_MAE" = map_dbl(accuracy_by_error_type, ~.x[["sample_MAE"]]))

perf_by_params <-
  cbind(data.frame(grid), mean_accuracy_by_error_type) %>%
  pivot_longer(c("word_MSE", "sample_MSE", "word_MAE", "sample_MAE"), names_to = "error_type", values_to = "accuracy")

saveRDS(perf_by_params, "./Output/Classifier/perf_by_params.rds")
perf_by_params <- readRDS("./Output/Classifier/perf_by_params.rds")

summarized_perf <-
  perf_by_params %>%
  group_by(target_length, sample_size, centering, error_type) %>%
  summarize(mean_accuracy = mean(accuracy)) %>%
  ungroup()
write_csv(summarized_perf, "./Output/Classifier/summarized_perf.csv")

# Plot accuracy by sample size
summarized_perf %>%
  mutate(target_length = as.factor(target_length),
         centering = factor(centering,
                            levels = c("none", "length"),
                            labels = c("non-centered", "length-centered"))) %>%
  ggplot(aes(x = sample_size, y = mean_accuracy, color = target_length)) +
  facet_grid(cols = vars(error_type), rows = vars(centering)) +
  geom_line(linewidth = .4) +
  geom_point(size = .6) +
  scale_color_discrete(name = "length") +
  labs(title = "Classification via orthographic-lexical entropy",
       subtitle = sprintf("drop = %s, iterations = %s", DROP, ITERATIONS),
       x = "sample size",
       y = "mean accuracy")

ggsave("./Figures/Classifier/hangman_entropy_classifier_error_types.pdf", height = FIG_H, width = FIG_H * 2, units = "in")
