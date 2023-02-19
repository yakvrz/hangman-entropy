setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); rm(list=ls()); gc() 
pacman::p_load(tidyverse, furrr, lme4, lmerTest, sjPlot, sjmisc, broom,
               broom.mixed, viridis)
source("utils.R")
source("analysis_funcs.R")

# Plotting options
theme_set(theme_bw())
theme_update(aspect.ratio = 1)
FIG_W <- 4.5
FIG_H <- 4.5

# Parameters
DROPS <- seq(0.1, 0.9, 0.1)
LANG <- "en"
LENGTHS <- 4:12

# Load MECO L2 data
load("./Datasets/MECO/joint_data_L2_trimmed.rda")
L2_data <- filter_L2_data(joint.data) %>%
  filter(length %in% LENGTHS)
rm(joint.data)


# Subject-level drop optimization ----------------------------------------------

# Load entropy data
word_entropy_vectors_by_drop <-
  map(DROPS, function(drop){
    readRDS(sprintf("./Output/HangmanEntropy/%s/entropy_drop_%s_lang_%s.rds",
                    LANG, drop, LANG)) %>%
      filter(length %in% LENGTHS) %>%
      rowwise() %>%
      mutate(entropy = list(as.vector(na.omit(as.numeric(entropy))))) %>%
      ungroup()
  }, .progress = TRUE)

# Append entropy estimates
plan(multisession)
data_by_drop <- future_map(word_entropy_vectors_by_drop, ~join_entropy_data(L2_data, .x))

subj_drop_opt <- data.frame("uniform_id" = unique(L2_data$uniform_id))
subj_drop_opt$opt_drop_refix <-
  map_dbl(subj_drop_opt$uniform_id, 
          ~optimize_subject_drop(data_by_drop, .x, "firstfix"),
          .progress = TRUE)

# Individual differences analysis ----------------------------------------------

# Load MECO L2 individual differences data
load("./Datasets/MECO/joint_ind_diff_L2.rda")
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
  inner_join(subj_drop_opt)

m <- lm(opt_drop_refix ~ .,
        select(ind_diff_data, -uniform_id, -lang, -comp_score, -vocab_score, -nonvocab_score))
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


