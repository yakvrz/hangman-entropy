pacman::p_load(tidyverse, furrr, lme4, lmerTest, broom, broom.mixed, interactions, performance, viridis)
source("./scripts/utils.R")
source("./scripts/analysis_funcs.R")
plan(multisession)

# Plotting options
theme_set(theme_bw())
theme_update(aspect.ratio = 1)
FIG_W <- 4.5
FIG_H <- 4.5

# Parameters
DROPS <- seq(0.1, 0.9, 0.1)
DROP_TYPE <- "linear"
LANG <- "en"
LENGTHS <- 4:12

# Load MECO L2 data
load("./data/meco/joint_data_L2_trimmed.rda")
L2_data <- filter_L2_data(joint.data) %>% filter(length %in% LENGTHS)

# Load entropy data
word_entropy_vectors_by_drop <-
  map(DROPS, function(drop){
    readRDS(sprintf("./output/hangman_entropy/meco_L1/entropy_data_drop_%s_%s_lang_%s.rds", drop, DROP_TYPE, LANG)) %>%
      filter(length %in% LENGTHS)
  })

# Append entropy estimates
data_by_drop <- future_map(word_entropy_vectors_by_drop, ~join_entropy_data(L2_data, .x))


# First fixation duration ------------------------------------------------------------------------------------

plan(multisession)
firstfix_1a %<-% lmer(log_firstfix_dur ~ firstfix_entropy + log_freq + length + firstfix_center_dist_fct + (1|uniform_id) + (1|word), data_by_drop[[1]])
firstfix_1b %<-% lmer(log_firstfix_dur ~ firstfix_entropy + log_freq + length + firstfix_center_diff_fct + (1|uniform_id) + (1|word), data_by_drop[[1]])
firstfix_1c %<-% lmer(log_firstfix_dur ~ firstfix_entropy * firstfix_center_dist_fct + log_freq + length + (1|uniform_id) + (1|word), data_by_drop[[1]])
firstfix_1d %<-% lmer(log_firstfix_dur ~ firstfix_entropy * firstfix_center_diff_fct + log_freq + length + (1|uniform_id) + (1|word), data_by_drop[[1]])

firstfix_2a %<-% lmer(log_firstfix_dur ~ firstfix_entropy + log_freq + length + firstfix_center_dist_fct + (1|uniform_id) + (1|word), data_by_drop[[3]])
firstfix_2b %<-% lmer(log_firstfix_dur ~ firstfix_entropy + log_freq + length + firstfix_center_diff_fct + (1|uniform_id) + (1|word), data_by_drop[[3]])
firstfix_2c %<-% lmer(log_firstfix_dur ~ firstfix_entropy * firstfix_center_dist_fct + log_freq + length + (1|uniform_id) + (1|word), data_by_drop[[3]])
firstfix_2d %<-% lmer(log_firstfix_dur ~ firstfix_entropy * firstfix_center_diff_fct + log_freq + length + (1|uniform_id) + (1|word), data_by_drop[[3]])

firstfix_list <- list(firstfix_1a, firstfix_1b, firstfix_1c, firstfix_1d, firstfix_2a, firstfix_2b, firstfix_2c, firstfix_2d)

map(firstfix_list, summary)
map(firstfix_list, anova)
map(firstfix_list, BIC)
map(firstfix_list, check_collinearity)

firstfix_eff <-
  map(firstfix_list, function(m){
    ggeffect(m, terms = "firstfix_entropy") %>%
      ggplot(aes(x, predicted)) +
      geom_line() +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
      labs(x = "First fixation entropy (bit)",
           y = "First fixation duration (log ms)",
           subtitle = as.character(m@call$formula)[3])
  })
firstfix_eff

firstfix_emm_by_length <-
  map2(firstfix_list, rep(c(0.1, 0.3), each = 4), function(m, d){
    map(4:9, function(l){
      ggemmeans(m, terms = "firstfix_entropy", condition = c(length = l)) %>%
        ggplot(aes(x, predicted)) +
        geom_line() +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
        labs(x = "", y = "", subtitle = paste("length:", l, "drop:", d))
    })
  })

ggarrange(plotlist = firstfix_emm_by_length[[1]])
ggarrange(plotlist = firstfix_emm_by_length[[2]])
ggarrange(plotlist = firstfix_emm_by_length[[3]])
ggarrange(plotlist = firstfix_emm_by_length[[4]])

ggarrange(plotlist = firstfix_emm_by_length[[4]])
ggarrange(plotlist = firstfix_emm_by_length[[5]])
ggarrange(plotlist = firstfix_emm_by_length[[6]])
ggarrange(plotlist = firstfix_emm_by_length[[8]])

interact_plot(firstfix_list[[3]], pred = "firstfix_entropy", modx = "firstfix_center_dist_fct", modx.values = as.character(seq(-3, 3, 1)))
interact_plot(firstfix_list[[4]], pred = "firstfix_entropy", modx = "firstfix_center_diff_fct", modx.values = as.character(seq(-3, 3, 1)))

interact_plot(firstfix_list[[7]], pred = "firstfix_entropy", modx = "firstfix_center_dist_fct", modx.values = as.character(seq(-3, 3, 1)))
interact_plot(firstfix_list[[8]], pred = "firstfix_entropy", modx = "firstfix_center_diff_fct", modx.values = as.character(seq(-3, 3, 1)))

# ggsave(sprintf("./Figures/Behavioral/L2/firstfix_dur_by_firstfix_entropy_drop_%s_%s.png", DROP, DROP_TYPE),
# width = FIG_W, height = FIG_H, units = "in")


# Refixation rate --------------------------------------------------------------------------------------------

refix_1a %<-% glmer(refix ~ firstfix_entropy + log_freq + length + firstfix_center_dist + (1|uniform_id) + (1|word), data_by_drop[[1]], family = "binomial", nAGQ = 0)
refix_1b %<-% glmer(refix ~ firstfix_entropy + log_freq + length + firstfix_center_dist_sq + (1|uniform_id) + (1|word), data_by_drop[[1]], family = "binomial", nAGQ = 0)
refix_1c %<-% glmer(refix ~ firstfix_entropy + log_freq + length + firstfix_center_diff_fct + (1|uniform_id) + (1|word), data_by_drop[[1]], family = "binomial", nAGQ = 0)
refix_1d %<-% glmer(refix ~ firstfix_entropy * firstfix_center_diff_fct + log_freq + length + (1|uniform_id) + (1|word), data_by_drop[[1]], family = "binomial", nAGQ = 0)

refix_2a %<-% glmer(refix ~ firstfix_entropy + log_freq + length + firstfix_center_dist + (1|uniform_id) + (1|word), data_by_drop[[3]], family = "binomial", nAGQ = 0)
refix_2b %<-% glmer(refix ~ firstfix_entropy + log_freq + length + firstfix_center_dist_sq + (1|uniform_id) + (1|word), data_by_drop[[3]], family = "binomial", nAGQ = 0)
refix_2c %<-% glmer(refix ~ firstfix_entropy + log_freq + length + firstfix_center_diff_fct + (1|uniform_id) + (1|word), data_by_drop[[3]], family = "binomial", nAGQ = 0)
refix_2d %<-% glmer(refix ~ firstfix_entropy * firstfix_center_diff_fct + log_freq + length + (1|uniform_id) + (1|word), data_by_drop[[3]], family = "binomial", nAGQ = 0)

refix_list <- list(refix_1a, refix_1b, refix_1c, refix_1d, refix_2a, refix_2b, refix_2c, refix_2d)

refix_summaries <- map(refix_list, summary)
refix_BIC <- map(refix_list, BIC)
refix_collinearity <- map(refix_list, check_collinearity)

refix_eff <-
  future_map(refix_list, function(m){
    ggeffect(m, terms = "firstfix_entropy") %>%
      ggplot(aes(x, predicted)) +
      geom_line() +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
      labs(x = "First fixation entropy (bit)",
           y = "Refixation rate",
           subtitle = as.character(m@call$formula)[3])
  })
refix_eff

refix_emm_by_length <-
  future_map2(refix_list, rep(c(0.1, 0.3), each = 4), function(m, d){
    map(4:9, function(l){
      ggemmeans(m, terms = "firstfix_entropy", condition = c(length = l, length_fct = l)) %>%
        ggplot(aes(x, predicted)) +
        geom_line() +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
        labs(x = "", y = "", subtitle = paste("length:", l, "drop:", d))
    })
  })

ggarrange(plotlist = refix_emm_by_length[[1]])
ggarrange(plotlist = refix_emm_by_length[[2]])
ggarrange(plotlist = refix_emm_by_length[[3]])
ggarrange(plotlist = refix_emm_by_length[[4]])

ggarrange(plotlist = refix_emm_by_length[[5]])
ggarrange(plotlist = refix_emm_by_length[[6]])
ggarrange(plotlist = refix_emm_by_length[[7]])
ggarrange(plotlist = refix_emm_by_length[[8]])

# ggsave(sprintf("./Figures/Behavioral/L2/refix_by_firstfix_entropy_drop_%s_%s.png", DROP, DROP_TYPE),
# width = FIG_W, height = FIG_H, units = "in")


# # Fixation-entropy distributions ----------------------------------------------------------------------------------

data <- data_by_drop[[1]]

data %>%
  mutate(length = factor(length)) %>%
  group_by(firstfix_land, length) %>%
  summarize(mean_firstfix_entropy = mean(firstfix_entropy)) %>%
  ggplot(aes(x = firstfix_land, y = mean_firstfix_entropy, color = length)) +
  geom_smooth() +
  labs(title = "Distribution of first fixation positions",
       y = "Proportion",
       x = "First fixation position") +
  scale_x_continuous(breaks = seq(0, MAX_LENGTH, 1)) +
  scale_color_discrete(name = "Length", type = inferno(10)) +
  theme_bw() +
  theme(aspect.ratio = 1)


# First fixation position distribution by length
data %>%
  group_by(length, firstfix_land) %>%
  tally() %>%
  drop_na() %>%
  group_by(length) %>%
  mutate(proportion = n/sum(n),
         length = factor(length)) %>%
  ggplot(aes(x = firstfix_land, y = proportion, color = length)) +
  geom_point() +
  geom_line() +
  labs(title = "Distribution of first fixation positions",
       y = "Proportion",
       x = "First fixation position") +
  scale_x_continuous(breaks = seq(0, MAX_LENGTH, 1)) +
  scale_color_discrete(name = "Length", type = inferno(10)) +
  theme_bw() +
  theme(aspect.ratio = 1)

# Second fixation position distribution by length
data %>%
  group_by(length, secondfix_land) %>%
  tally() %>%
  drop_na() %>%
  group_by(length) %>%
  mutate(proportion = n/sum(n),
         length = factor(length)) %>%
  ggplot(aes(x = secondfix_entropy, y = proportion, color = length)) +
  geom_point() +
  geom_line() +
  labs(title = "Distribution of second fixation positions",
       y = "Proportion",
       x = "Second fixation position") +
  scale_x_continuous(breaks = seq(0, MAX_LENGTH, 1)) +
  scale_color_discrete(name = "Length", type = inferno(10)) +
  theme_bw() +
  theme(aspect.ratio = 1)


data1 <-
  data %>%
  drop_na(secondfix_entropy) %>%
  pivot_longer(cols = c("firstfix_entropy", "secondfix_entropy"),
               names_to = "fixation",
               values_to = "fixation_entropy")

data2 <-
  data %>%
  drop_na(secondfix_entropy) %>%
  pivot_longer(cols = c("firstfix_land", "secondfix_land"),
               names_to = "fixation",
               values_to = "fixation_land")

data1 %>% group_by(fixation) %>% tally()

data2 %>%
  ggplot(aes(x = fixation_land, y = after_stat(density), color = fixation)) +
  facet_wrap(~length) +
  geom_histogram(position = "dodge")

hist(data1$firstfix_entropy)
hist(data$secondfix_entropy)

# Statistics
t.test(data$firstfix_land, data$secondfix_land)
t.test(data$firstfix_entropy, data$secondfix_entropy)
var.test(data$firstfix_land, data$secondfix_land)
var.test(data$firstfix_entropy, data$secondfix_entropy)


