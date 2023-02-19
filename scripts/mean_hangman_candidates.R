
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); rm(list=ls()); gc()
packages <- c("tidyverse", "viridis")
for (pkg in packages){library(pkg, character.only = TRUE)}; rm(packages, pkg)

# Plot options
theme_set(theme_bw())
fig_w <- 6.5
fig_h <- 6.5
fig_scales <- "free_y"

# Parameters
drop <- 0.4
min_length <- 4
max_length <- 12
min_freq <- 0.5
languages <- c("en", "he", "sp")

# Load and reformat hangman candidate data
candidates_by_word <-
  map_dfr(languages, function(language){
  readRDS(sprintf("./Output/Candidates/candidates_drop_%s_lang_%s.rds", drop, language)) %>%
    select(-freq) %>%
    mutate(lang = language) %>%
    relocate(lang) %>%
    bind_cols(.$candidates) %>%
    as.data.frame() %>%
    select(-candidates) %>%
    pivot_longer(starts_with("V"), names_to = "pos", values_to = "candidates", names_prefix = "V") %>%
    mutate(pos = as.numeric(pos)) %>%
    drop_na() %>%
    group_by(length) %>%
    mutate(candidates_centered_by_length = candidates - mean(candidates)) %>%
    ungroup()
})

# Calculate mean [length-centered] number of candidates, by position
candidates_by_pos <-
  candidates_by_word %>%
  group_by(lang, length, pos) %>%
  summarize(mean_candidates = mean(candidates),
            mean_candidates_centered_by_length = mean(candidates_centered_by_length)) %>%
  ungroup()


# Plots ---------------------------------------------------------------------------------------

# Mean number of candidates by position, length and language
candidates_by_pos %>%
  mutate(pos_num = as.numeric(as.character(pos)),
         length = as.factor(length)) %>%
  ggplot(aes(x = pos_num, y = mean_candidates, color = lang)) +
  facet_wrap(~length, scales = fig_scales) +
  geom_line(linewidth = .4) +
  geom_point(size = .8) +
  labs(title = "Orthographic-lexical candidates by fixation position",
       subtitle = sprintf("drop = %s", drop),
       y = "Mean number of candidates",
       x = "Position") +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  scale_color_discrete(name = "Language", labels = c("English", "Hebrew", "Spanish")) +
  theme(aspect.ratio = 1)
ggsave(sprintf("./Output/Candidates/Figures/mean_candidates_by_position_drop_%s.pdf", drop), width = fig_w, height = fig_h)

# Mean length-centered number of candidates by position, length and language
candidates_by_pos %>%
  mutate(pos_num = as.numeric(as.character(pos)),
         length = as.factor(length)) %>%
  ggplot(aes(x = pos_num, y = mean_candidates_centered_by_length, color = lang)) +
  facet_wrap(~length, scales = fig_scales) +
  geom_line(linewidth = .4) +
  geom_point(size = .8) +
  labs(title = "Orthographic-lexical candidates by fixation position",
       subtitle = sprintf("drop = %s | centered by length", drop),
       y = "Mean number of candidates (centered)",
       x = "Position") +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  scale_color_discrete(name = "Language", labels = c("English", "Hebrew", "Spanish")) +
  theme(aspect.ratio = 1)
ggsave(sprintf("./Output/Candidates/Figures/mean_centered_candidates_by_position_drop_%s.pdf", drop), width = fig_w, height = fig_h)

