setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); rm(list=ls()); gc()
pacman::p_load(tidyverse, viridis)

# Plotting options
theme_set(theme_bw())
theme_update(aspect.ratio = 1)
fig_w <- 6.5
fig_h <- 6.5

# Data parameters
LANGS <- c("en", "he", "sp")
DROP <- 0.3

# Combine entropy data and transform to long format
entropy_long <-
  map_dfr(LANGS, function(x){
    readRDS(sprintf("./Output/HangmanEntropy/entropy_drop_%s_lang_%s.rds", DROP, x)) %>%
      select(-freq) %>%
      mutate(lang = as.factor(x)) %>%
      relocate(lang) %>%
      bind_cols(.$entropy) %>%
      as.data.frame() %>%
      select(-entropy) %>%
      pivot_longer(starts_with("V"), names_to = "pos", values_to = "entropy",
                   names_prefix = "V", names_transform = list(pos = as.numeric)) %>%
      drop_na() %>%
      mutate("1" = entropy - mean(entropy)) %>%
      group_by(length) %>%
      mutate("2" = entropy - mean(entropy)) %>%
      group_by(word) %>%
      mutate("3" = entropy - mean(entropy)) %>%
      ungroup() %>%
      rename("0" = entropy) %>%
      pivot_longer(c("0", "1", "2", "3"), names_to = "centering", values_to = "entropy") %>%
      mutate(centering = factor(centering, labels = c("none", "lang", "length", "word")))
  })
saveRDS(entropy_long, sprintf("./Output/HangmanEntropy/entropy_long_drop_%s.rds", DROP))


# Plot entropy by position, length and language (for a chosen drop) ------------------------------------------

LENGTHS <- 4:7
CENTERING <- "none"

# Calculate mean entropy by language, length, and position
mean_entropy_long <-
  entropy_long %>%
  group_by(lang, length, pos, centering) %>%
  summarize(mean_entropy = mean(entropy)) %>%
  ungroup() %>%
  filter(length %in% LENGTHS,
         centering == CENTERING)

# Plot
mean_entropy_long %>%
  mutate(lang = factor(lang, labels = c("English", "Hebrew", "Spanish"))) %>%
  ggplot(aes(x = pos, y = mean_entropy, color = lang)) +
  facet_wrap(~length, scales = "free_x") +
  geom_line(linewidth = .4) +
  geom_point(size = .8) +
  labs(title = "Orthographic-lexical entropy by position",
       # subtitle = sprintf("drop = %s, centering = %s", DROP, CENTERING),
       y = "Mean entropy (bit)",
       x = "Fixation position") +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  scale_color_discrete(name = "Language")
# ggsave(sprintf("./Output/HangmanEntropy/Figures/mean_hangman_entropy_by_position_drop_%s.pdf", fig_drop), width = fig_w, height = fig_h)


# Plot entropy by position, length, and drop (for a chosen language) -----------------------------------------

# Figure language
fig_lang <- "Spanish"

# Un-centered entropy
mean_entropy_long %>%
  filter(between(length, fig_lengths[1], fig_lengths[2]),
         lang == fig_lang) %>%
  mutate(pos_num = as.numeric(as.character(pos)),
         length = as.factor(length),
         drop = as.factor(drop)) %>%
  ggplot(aes(x = pos_num, y = mean_entropy, linetype = drop)) +
  facet_wrap(~length, scales = fig_facet_scales) +
  geom_line(linewidth = .4) +
  geom_point(size = .8) +
  labs(title = "Orthographic-lexical entropy by fixation position",
       subtitle = sprintf("language: %s", fig_lang),
       y = "Mean entropy (bit)",
       x = "Position") +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  scale_linetype_discrete(name = "Drop")
ggsave(sprintf("./Output/Entropy/Figures/mean_entropy_by_position_lang_%s_length_%s-%s.pdf", fig_lang, fig_lengths[1], fig_lengths[2]),
       width = fig_w, height = fig_h)

# Centered entropy (by length and language)
mean_entropy_long %>%
  filter(between(length, fig_lengths[1], fig_lengths[2]),
         lang == fig_lang) %>%
  mutate(pos_num = as.numeric(as.character(pos)),
         length = as.factor(length),
         drop = as.factor(drop)) %>%
  ggplot(aes(x = pos_num, y = mean_centered_entropy, linetype = drop)) +
  facet_wrap(~length, scales = fig_facet_scales) +
  geom_line(linewidth = .4) +
  geom_point(size = .8) +
  labs(title = "Orthographic-lexical entropy by fixation position",
       subtitle = sprintf("centered, language: %s", fig_lang),
       y = "Mean entropy (bit)",
       x = "Position") +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  scale_color_discrete(name = "Language", labels = c("English", "Hebrew", "Spanish")) +
  scale_linetype_discrete(name = "Drop")
ggsave(sprintf("./Output/Entropy/Figures/mean_centered_entropy_by_position_lang_%s_length_%s-%s.pdf", fig_lang, fig_lengths[1], fig_lengths[2]),
       width = fig_w, height = fig_h)

