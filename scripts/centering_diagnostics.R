setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); rm(list=ls()); gc()
library(tidyverse)
options(digits = 15)

entropy_long <- readRDS("./Output/HangmanEntropy/entropy_long_drop_0.1.rds")

word_trajectories <-
  entropy_long %>%
  filter(length == 4, lang == "sp", centering %in% c("none", "word")) %>%
  filter(word %in% sample(unique(word), 5))

mean_trajectories_wide <-
  entropy_long %>%
  filter(length == 4, lang != "he", centering %in% c("none", "word")) %>%
  group_by(lang, length, pos, centering) %>%
  summarize(mean_entropy = mean(entropy)) %>%
  pivot_wider(names_from = "lang", values_from = "mean_entropy", names_prefix = "mean_")

errors <-
  word_trajectories %>%
  left_join(mean_trajectories_wide) %>%
  group_by(word, centering, pos) %>%
  summarize(abs_error_sp = mean(abs(entropy - mean_sp)),
            abs_error_en = mean(abs(entropy - mean_en)),
            sq_error_sp = mean((entropy - mean_sp)^2),
            sq_error_en = mean((entropy - mean_en)^2))

mean_errors <-
  errors %>%
  group_by(word, centering) %>%
  summarize(mean_abs_error_sp = mean(abs_error_sp),
            mean_abs_error_en = mean(abs_error_en),
            mean_sq_error_sp = mean(sq_error_sp),
            mean_sq_error_en = mean(sq_error_en))

### plot
target <- "roja"
word_trajectories %>%
  bind_rows(entropy_long %>%
              filter(length == 4, lang != "he", centering %in% c("none", "word")) %>%
              group_by(lang, length, pos, centering) %>%
              summarize(entropy = mean(entropy)) %>%
              mutate(word = "MEAN")) %>%
  filter(word %in% c("MEAN", target)) %>%
  ggplot(aes(x = pos, y = entropy, color = lang, linetype = word)) +
  facet_wrap(~centering, scales = "free_y") +
  geom_line()


###
mean_none_en <- mean_trajectories_wide %>% filter(centering == "none") %>% .$mean_en
mean_none_sp <- mean_trajectories_wide %>% filter(centering == "none") %>% .$mean_sp
mean_centered_en <- mean_trajectories_wide %>% filter(centering == "word") %>% .$mean_en
mean_centered_sp <- mean_trajectories_wide %>% filter(centering == "word") %>% .$mean_sp

word_none <- word_trajectories %>% filter(word == "roja", centering == "none") %>% .$entropy
word_centered <- word_trajectories %>% filter(word == "roja", centering == "word") %>% .$entropy

word_none
word_centered

mean(abs(word_none - mean_none_sp))
mean(abs(word_none - mean_none_en))

mean(abs(word_centered - mean_centered_sp))
mean(abs(word_centered - mean_centered_en))

mean((word_none - mean_none_sp)^2)
mean((word_none - mean_none_en)^2)

mean((word_centered - mean_centered_sp)^2)
mean((word_centered - mean_centered_en)^2)
