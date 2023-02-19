setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); rm(list=ls()); gc()
packages <- c("tidyverse", "viridis", "furrr")
for (pkg in packages){library(pkg, character.only = TRUE)}; rm(packages, pkg)

# Plot options
theme_set(theme_bw())
theme_update(aspect.ratio = 1)
fig_w <- 6.5
fig_h <- 6.5
fig_scales <- "free_x"
fig_lengths <- c(4, 9)

# Parameters
languages <- c("en", "he", "sp")
min_length <- 4
max_length <- 12


# Un-centered estimates -------------------------------------------------------------------------------------------

surp_long <-
  map_dfr(languages, ~readRDS(sprintf("./Output/LetterSurprisal/letter_surprisal_lang_%s.rds", .x))) %>%
  relocate(lang) %>%
  pivot_longer(starts_with("V"), names_to = "pos", values_to = "surp", names_prefix = "V") %>%
  mutate(pos = as.numeric(pos),
         lang = factor(lang, labels = c("English", "Hebrew", "Spanish"))) %>%
  drop_na() %>%
  group_by(lang, length) %>%
  mutate(centered_surp = surp - mean(surp)) %>%
  ungroup()

mean_surp_by_pos <-
  surp_long %>%
  group_by(lang, length, pos) %>%
  summarize(mean_surp = mean(surp),
            mean_centered_surp = mean(centered_surp)) %>%
  ungroup()


# Plot surprisal by position and language --------------------------------------------------------------------

# Un-centered entropy
mean_surp_by_pos %>%
  filter(between(length, fig_lengths[1], fig_lengths[2])) %>%
  mutate(pos_num = as.numeric(as.character(pos)),
         length = as.factor(length)) %>%
  ggplot(aes(x = pos_num, y = mean_surp, color = lang)) +
  facet_wrap(~length, scales = fig_scales) +
  geom_line(linewidth = .4) +
  geom_point(size = .8) +
  labs(subtitle = "Letter transition surprisal by position",
       y = "Mean surprisal (bits)",
       x = "Position") +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  scale_color_discrete(name = "Language")
ggsave("./Output/LetterSurprisal/mean_surprisal_by_position.pdf", width = fig_w, height = fig_h)

# Centered entropy (by length and language)
mean_surp_by_pos %>%
  filter(between(length, fig_lengths[1], fig_lengths[2])) %>%
  mutate(pos_num = as.numeric(as.character(pos)),
         length = as.factor(length)) %>%
  ggplot(aes(x = pos_num, y = mean_centered_surp, color = lang)) +
  facet_wrap(~length, scales = fig_scales) +
  geom_line(linewidth = .4) +
  geom_point(size = .8) +
  labs(subtitle = "Mean-centered letter transition surprisal position",
       y = "Mean-centered mean surprisal (bits)",
       x = "Position") +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  scale_color_discrete(name = "Language")
ggsave("./Output/LetterSurprisal/mean_surprisal_by_position_centered.pdf", width = fig_w, height = fig_h)
