setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); rm(list=ls()); gc() 
packages <- c("tidyverse", "data.table", "furrr", "abind", "corrr", "tictoc")
for (pkg in packages){library(pkg, character.only = TRUE)}; rm(packages, pkg)
source("utils.R")
source("letter_surprisal_funcs.R")
options(future.rng.onMisuse = "ignore")

# Parameters
language <- "he"
min_freq <- .5e-6
min_length <- 4
max_length <- 12

# Load and pre-process OpenSubtitles unigram frequency tables
word_data <-
    fread(sprintf("./Data/OpenSubtitles/opensub_unigrams_%s.tsv", language),
          sep = "\t",
          encoding = "UTF-8") %>%
    rename(word = unigram,
           count = unigram_freq) %>%
    mutate(freq = count/get_corpus_size(language),
           length = nchar(word)) %>%
    filter(freq >= min_freq,
           between(length, min_length, max_length),
           str_detect(word, sprintf("[^%s]", str_flatten(get_alphabet(language))), negate = TRUE),
           str_detect(word, "^[ךםןףץ]\\w*", negate = TRUE)) %>%
    rowwise() %>%
    mutate(letter_unigrams = list(str_split(word, "", simplify = TRUE)),
           letter_bigrams = list(extract_letter_bigrams(word))) %>%
    ungroup()

# Calculate letter bigram frequencies
plan(multisession)
letter_bigram_freq <-
    future_map2(word_data$letter_bigrams, word_data$count, ~ table(.x) * .y) %>%
    unlist() %>%
    stack() %>%
    rename(bigram = ind, count = values) %>%
    mutate(bigram = as.character(bigram)) %>%
    group_by(bigram) %>%
    mutate(letter_1 = str_split(bigram, "", simplify = TRUE)[,1],
           letter_2 = str_split(bigram, "", simplify = TRUE)[,2]) %>%
    filter(!(str_detect(letter_1, "ךםןףץ") & (letter_2 == 1))) %>%
    group_by(bigram, letter_1, letter_2) %>%
    summarize(count = sum(count)) %>%
    ungroup() %>%
    mutate(freq = count/sum(count)) %>%
    ungroup() %>%
    as.data.frame()

# Compute letter frequencies
letter_freq <-
    letter_bigram_freq %>%
    rename(letter = letter_1) %>%
    group_by(letter) %>%
    filter(letter != "0") %>%
    summarize(count = sum(count)) %>%
    ungroup() %>%
    mutate(freq = count/sum(count)) %>%
    as.data.frame()

# Calculate letter transition matrix
col_names <- unique(letter_bigram_freq$letter_2)
letter_transition_freq <-
    letter_bigram_freq %>%
    select(-bigram, -freq) %>%
    pivot_wider(names_from = letter_2, values_from = count) %>%
    arrange(letter_1) %>%
    relocate(letter_1, 0, all_of(col_names), 1) %>%
    data.table() %>%
    as.matrix(rownames = "letter_1") %>%
    replace(is.na(.), min(., na.rm = TRUE))
letter_transition_freq["0", "1"] <- 0
letter_transition_freq <- proportions(letter_transition_freq, margin = 1)
letter_transition_freq["0", "1"] <- NA

# Compute letter transition surprisal and entropy
plan(multisession)
letter_surprisal <- future_map_dfr(word_data$word, ~as.data.frame(t(calculate_letter_surprisal(.x, letter_transition_freq))))

# Calculate surprisal by position
surp_by_pos <-
    word_data %>%
    select(word, length) %>%
    mutate(lang = language) %>%
    bind_cols(letter_surprisal)
saveRDS(surp_by_pos, sprintf("./Output/LetterSurprisal/letter_surprisal_%s.RDS", language))
