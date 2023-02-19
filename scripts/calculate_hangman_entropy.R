pacman::p_load(tidyverse, furrr)
source("./scripts/utils.R")
source("./scripts/hangman_funcs.R")
options(future.rng.onMisuse = "ignore")
plan(multisession)

# Parameters
LANG <- "en"
DROPS <- seq(0.1, 0.9, 0.1)
DROP_TYPE <- "linear"
MIN_FREQ <- 1e-7
LENGTHS <- 4:12

# Preprocess word frequency tables
word_data <-
  read_tsv(sprintf("./data/opensubtitles/opensub_unigrams_%s.tsv", LANG), locale = locale(encoding = "UTF-8")) %>%
  rename(word = unigram,
         count = unigram_freq) %>%
  mutate(freq = count/get_corpus_size(LANG),
         length = nchar(word)) %>%
  filter(freq >= MIN_FREQ,
         length %in% LENGTHS,
         str_detect(word, sprintf("[^%s]", str_flatten(get_alphabet(LANG))), negate = TRUE),
         str_detect(word, "^[ךםןףץ]\\w*", negate = TRUE)) %>%
  select(-count)

# Calculate hangman letter masks
# bool_masks <- calculate_bool_masks(max(lengths))
bool_masks <- readRDS("./output/hangman_masks/bool_masks.rds")

# Calculate orthographic-lexical entropy by position
for (DROP in DROPS){
  # bool_mask_probs <- calculate_bool_mask_probs(DROP, MAX_LENGTH)
  bool_mask_probs <- readRDS(sprintf("./output/hangman_masks/bool_mask_probs_drop_%s.rds", DROP))
  
  word_entropy_data <-
    word_data %>%
    mutate(entropy = future_map_dfr(word,
                                    ~hangman_entropy(.x, word_data, DROP, DROP_TYPE, bool_masks, bool_mask_probs),
                                    .progress = TRUE)) %>%
    rowwise() %>%
    mutate(entropy = list(as.vector(na.omit(as.numeric(entropy))))) %>%
    ungroup()
  
  saveRDS(word_entropy_data, sprintf("./output/hangman_entropy/entropy_data_drop_%s_%s_lang_%s.rds", DROP, DROP_TYPE, LANG))
}
