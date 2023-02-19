setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); rm(list=ls()); gc()
pacman::p_load(tidyverse, furrr, data.table, viridis)
source("hangman_funcs.R")
source("utils.R")
options(future.rng.onMisuse = "ignore")

# Plotting options
theme_set(theme_bw())

# Parameters
LANG <- "en"
DROP <- 0.1
LENGTH <- 7
CENTERING <- "none"

corpus_data <-
  read_tsv(sprintf("./Datasets/OpenSubtitles/opensub_unigrams_%s.tsv", LANG), locale = locale(encoding = "UTF-8")) %>%
  rename(word = unigram,
         count = unigram_freq) %>%
  mutate(freq = count/get_corpus_size(LANG),
         length = nchar(word)) %>%
  slice_head(n = 30000) %>%
  filter(str_detect(word, sprintf("[^%s]", str_flatten(get_alphabet(LANG))), negate = TRUE)) %>%
  select(-count)

bool_masks <- readRDS("./Output/Masks/bool_masks.rds")
bool_mask_probs <- calculate_bool_mask_probs(DROP, LENGTH)
hangman_entropy("journey", corpus_data, DROP, bool_masks, bool_mask_probs)

entropy_long <-
  readRDS(sprintf("./Output/HangmanEntropy/entropy_long_drop_%s.rds", DROP)) %>%
  filter(centering %in% CENTERING,
         length %in% LENGTH)

entropy_long %>%
  filter(word == "journey") %>%
  ggplot(aes(x = pos, y = entropy)) +
  geom_point() +
  geom_line() +
  labs(y = "Entropy (bit)",
       x = "Fixation position",
       subtitle = "(1)") +
  ylim(0, NA) +
  scale_x_continuous(breaks = 1:7) +
  theme_bw()

ggsave("C:/Users/yakvrz/iCloudDrive/Documents/Research/ISCOP 2023/Poster/journey_traj_1.svg",
       width = 4, height = 2.25, units = "in", scale = 1.1)
  
