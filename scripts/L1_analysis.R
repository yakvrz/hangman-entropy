setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); rm(list=ls()); gc() 
pacman::p_load(tidyverse, furrr, lme4, lmerTest, sjPlot, sjmisc, broom, broom.mixed, viridis)
source("utils.R")

# Plotting options
theme_set(theme_bw())
theme_update(aspect.ratio = 1)
FIG_W <- 4.5
FIG_H <- 4.5

# Parameters
LANGS <- c("en", "he", "sp")
DROP <- 0.3

# Load MECO L1 data
load("./Datasets/MECO/joint_data_L1_trimmed.rda")
L1_data <-
  joint.data %>%
  filter(lang %in% LANGS) %>%
  rename(word = ia) %>%
  filter(str_detect(word, glob2rx("*’s"), negate = TRUE)) %>%
  mutate(word = case_when(lang %in% c("en", "sp") ~ str_to_lower(word), TRUE ~ word),
         length = nchar(word)) %>%
  filter(between(dur, 80, quantile(dur, 0.99, na.rm = TRUE)) | is.na(dur)) %>%
  rowwise() %>%
  filter(between(firstfix.land, 1, length)) %>%
  mutate(word = str_replace_all(word, sprintf("[^%s]", str_flatten(get_alphabet(lang))), "")) %>%
  ungroup()

# Load entropy data
word_entropy_vectors <-
  map_dfr(LANGS, function(LANG){
    readRDS(sprintf("./Output/HangmanEntropy/entropy_drop_%s_lang_%s.rds", DROP, LANG)) %>%
      mutate(lang = LANG,
             drop = as.factor(DROP)) %>%
      relocate(lang, drop) %>%
      rowwise() %>%
      mutate(entropy = list(as.vector(na.omit(as.numeric(entropy))))) %>%
      ungroup()
  })

# Append entropy estimates
data <-
  L1_data %>%
  inner_join(word_entropy_vectors %>% select(lang, length, word, freq, entropy)) %>%
  rename(wordnum = ianum) %>%
  select(lang, uniform_id, itemid, sentnum, wordnum, word, freq, length,
         dur, firstfix.dur, firstrun.dur, refix, firstrun.refix,
         skip, length, firstfix.land, entropy) %>%
  # Add OLP columns
  mutate(center = if_else(length %% 2 == 1, length / 2 + 0.5, length / 2),
         firstfix_center_diff = firstfix.land - center) %>%
  # Exclude fixations beyond word boundary
  rowwise() %>%
  filter(between(firstfix.land, 1, length) | is.na(firstfix.land)) %>%
  # Extract entropy at fixated position
  mutate(firstfix_entropy = ifelse(!is.na(firstfix.land), entropy[[firstfix.land]], NA)) %>%
  ungroup() %>%
  # Log-transform frequency and duration
  mutate(log_freq = log(freq),
         log_dur = log(dur),
         log_firstfix_dur = log(firstfix.dur),
         log_firstrun_dur = log(firstrun.dur)) %>%
  # Convert refixation, length, distance from OLP to factors
  mutate(firstfix_center_diff_fct = factor(firstfix_center_diff),
         length_fct = factor(length),
         refix = factor(refix)) %>%
  # Scale numeric predictors
  # mutate(length_z = my_scale(length),
  #        log_freq_z = my_scale(log_freq)) %>%
  droplevels()



# First fixation duration ------------------------------------------------------------------------------------

# English
m_firstfix_dur_en <-
  lmer(log_firstfix_dur ~ firstfix_entropy + length + firstfix_center_diff_fct + log_freq + (1|uniform_id) + (1|word),
       data %>% filter(lang == "en"))

summary(m_firstfix_dur_en)

plot_model(m_firstfix_dur_en, type = "eff", terms = "firstfix_entropy") +
  labs(title = "English",
       x = "First fixation entropy (bit)",
       y = "First fixation duration (log ms)")

# Hebrew
m_firstfix_dur_he <-
  lmer(log_firstfix_dur ~ firstfix_entropy + length + firstfix_center_diff_fct + log_freq + (1|uniform_id) + (1|word),
       data %>% filter(lang == "he"))

summary(m_firstfix_dur_he)

plot_model(m_firstfix_dur_he, type = "eff", terms = "firstfix_entropy") +
  labs(title = "Hebrew",
       x = "First fixation entropy (bit)",
       y = "First fixation duration (log ms)")

# Spanish
m_firstfix_dur_sp <-
  lmer(log_firstfix_dur ~ firstfix_entropy + length + firstfix_center_diff_fct + log_freq + (1|uniform_id) + (1|word),
       data %>% filter(lang == "sp"))

summary(m_firstfix_dur_sp)

plot_model(m_firstfix_dur_sp, type = "eff", terms = "firstfix_entropy") +
  labs(title = "Spanish",
       x = "First fixation entropy (bit)",
       y = "First fixation duration (log ms)")



# Refixation rate --------------------------------------------------------------------------------------------

# English
m_refix_en <-
  glmer(refix ~ firstfix_entropy + length + firstfix_center_diff_fct + log_freq + (1|uniform_id) + (1|word),
       data %>% filter(lang == "en"), family = "binomial", nAGQ = 0)

summary(m_refix_en)

plot_model(m_refix_en, type = "eff", terms = "firstfix_entropy") +
  labs(title = "English",
       x = "First fixation entropy (bit)",
       y = "Refixation rate")

# Hebrew
m_refix_he <-
  glmer(refix ~ firstfix_entropy + length + firstfix_center_diff_fct + log_freq + (1|uniform_id) + (1|word),
       data %>% filter(lang == "he"), family = "binomial", nAGQ = 0)

summary(m_refix_he)

plot_model(m_refix_he, type = "eff", terms = "firstfix_entropy") +
  labs(title = "Hebrew",
       x = "First fixation entropy (bit)",
       y = "Refixation rate")

# Spanish
m_refix_sp <-
  glmer(refix ~ firstfix_entropy + length + firstfix_center_diff_fct + log_freq + (1|uniform_id) + (1|word),
       data %>% filter(lang == "sp"), family = "binomial", nAGQ = 0)

summary(m_refix_sp)

plot_model(m_refix_sp, type = "eff", terms = "firstfix_entropy") +
  labs(title = "Spanish",
       x = "First fixation entropy (bit)",
       y = "Refixation rate")
