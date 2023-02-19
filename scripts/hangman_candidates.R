pacman::p_load(tidyverse, furrr)
source("./scripts/utils.R")
source("./scripts/hangman_funcs.R")
options(future.rng.onMisuse = "ignore")

# Calculate orthographic-lexical candidate numbers by position
# plan(multisession)
# candidate_data <-
#   corpus_data %>%
#   mutate(candidates = future_map_dfr(word, ~hangman_candidates(.x, corpus_data, DROP, bool_masks, bool_mask_probs)))
# plan(sequential)
# saveRDS(candidate_data, sprintf("./Output/Candidates/candidates_drop_%s_lang_%s.rds", DROP, LANG))

# Comparison of precise and estimated entropy\candidate numbers
n_iters <- c(50, 100, 250, 500, 1000)
target <- sample(corpus_data$word, 1)
true_entropy <- hangman_entropy(target, corpus_data, DROP, bool_masks, bool_mask_probs)
estimated_entropy <- map_dfr(n_iters, function(n_iter) hangman_entropy(target, corpus_data, DROP, bool_masks, bool_mask_probs, method = "sample", n_iter))

estimated_entropy %>%
  bind_rows(true_entropy) %>%
  mutate(n_iter = c(as.character(n_iters), "precise"),
         n_iter = factor(n_iter, levels = c(as.character(n_iters), "precise"))) %>%
  pivot_longer(starts_with("V"), names_prefix = "V", names_to = "pos", values_to = "entropy") %>%
  mutate(pos = as.numeric(pos)) %>%
  ggplot(aes(x = pos, y = entropy, linetype = n_iter, size = n_iter, color = n_iter)) +
  geom_line() +
  scale_color_discrete(name = "", type = viridis::turbo(length(n_iters) + 1, begin = 0, end = 0.8, direction = -1)) +
  scale_linetype_manual(name = "", values = c(rep("dashed", length(n_iters)), "solid")) +
  scale_size_manual(name = "", values = c(rep(0.4, length(n_iters)), 0.8)) +
  labs(title = "Comparison of precise and estimated entropy",
       subtitle = sprintf("drop = %s | target = '%s'", DROP, target)) +
  theme(aspect.ratio = 1)
# ggsave(sprintf("./Output/Diagnostics/entropy_estimate_precision_drop_%s.pdf", drop), width = 4.5, height = 4.5)

# Precise vs. estimated candidates
true_candidates <- hangman_candidates(target, corpus_data, DROP, bool_masks, bool_mask_probs)
estimated_candidates <- map_dfr(n_iters, function(n_iter) hangman_candidates(target, corpus_data, DROP, bool_masks, bool_mask_probs, method = "sample", n_iter))

estimated_candidates %>%
  bind_rows(true_candidates) %>%
  mutate(n_iter = c(as.character(n_iters), "precise"),
         n_iter = factor(n_iter, levels = c(as.character(n_iters), "precise"))) %>%
  pivot_longer(starts_with("V"), names_prefix = "V", names_to = "pos", values_to = "candidates") %>%
  mutate(pos = as.numeric(pos)) %>%
  ggplot(aes(x = pos, y = candidates, linetype = n_iter, size = n_iter, color = n_iter)) +
  geom_line() +
  scale_color_discrete(name = "", type = viridis::turbo(length(n_iters) + 1, begin = 0, end = 0.8, direction = -1)) +
  scale_linetype_manual(name = "", values = c(rep("dashed", length(n_iters)), "solid")) +
  scale_size_manual(name = "", values = c(rep(0.4, length(n_iters)), 0.8)) +
  labs(title = "Comparison of precise and estimated candidate numbers",
       subtitle = sprintf("drop = %s | target = '%s'", DROP, target)) +
  theme(aspect.ratio = 1)
# ggsave(sprintf("./Output/Diagnostics/candidates_estimate_precision_drop_%s.pdf", drop), width = 4.5, height = 4.5)