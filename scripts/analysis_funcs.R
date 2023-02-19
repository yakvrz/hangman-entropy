filter_L1_data <- function(data, language){
  
  out <-
    data %>%
    rename(word = ia) %>%
    filter(lang == language,
           str_detect(word, glob2rx("*’s"), negate = TRUE)) %>%
    mutate(word = case_when(language %in% c("en", "sp") ~str_to_lower(word), TRUE ~ word),
           word = str_replace_all(word, sprintf("[^%s]", str_flatten(get_alphabet(language))), ""),
           length = nchar(word)) %>%
    filter(between(dur, 80, quantile(dur, 0.99, na.rm = TRUE)) | is.na(dur)) %>%
    rowwise() %>%
    filter(between(firstfix.land, 1, length)) %>%
    ungroup()
  
  return(out)
}


filter_L2_data <- function(data){
  
  out <-
    data %>%
    rename(word = ia) %>%
    filter(str_detect(word, glob2rx("*’s"), negate = TRUE)) %>%
    mutate(word = str_to_lower(word),
           word = str_replace_all(word, sprintf("[^%s]", str_flatten(get_alphabet("en"))), ""),
           length = nchar(word)) %>%
    filter(between(dur, 80, quantile(dur, 0.99, na.rm = TRUE)) | is.na(dur)) %>%
    rowwise() %>%
    filter(between(firstfix.land, 1, length)) %>%
    ungroup()
  
  return(out)
}


join_entropy_data <- function(meco_data, entropy_data){
  
  out <-
    meco_data %>%
    select(-length) %>%
    inner_join(entropy_data, by = c("word")) %>%
    # Exclude fixations beyond word boundary
    rowwise() %>%
    filter(between(firstfix.land, 1, length) | is.na(firstfix.land)) %>%
    # Extract entropy at fixated position
    mutate(firstfix_entropy = ifelse(!is.na(firstfix.land), entropy[[firstfix.land]], NA)) %>%
    ungroup() %>%
    # Create OLP columns
    mutate(center = if_else(length %% 2 == 1, length / 2 + 0.5, length / 2),
           firstfix_center_diff = firstfix.land - center,
           firstfix_center_dist = abs(firstfix_center_diff),
           firstfix_center_dist_sq = firstfix_center_diff ^ 2) %>%
    
    rename(entropy_vec = entropy,
           firstfix_land = firstfix.land,
           firstfix_dur = firstfix.dur,
           firstrun_dur = firstrun.dur) %>%
    
    mutate(log_freq = log(freq),
           log_dur = log(dur),
           log_firstfix_dur = log(firstfix_dur),
           log_firstrun_dur = log(firstrun_dur),
           
           refix = as.factor(refix),
           length_fct = as.factor(length),
           firstfix_land_fct = as.factor(firstfix_land),
           firstfix_center_diff_fct = as.factor(firstfix_center_diff),
           firstfix_center_dist_fct = as.factor(firstfix_center_dist),
           
           freq_z = my_scale(freq),
           log_freq_z = my_scale(log_freq),
           length_z = my_scale(length),
           firstfix_entropy_z = my_scale(firstfix_entropy)) %>%
    
    select(uniform_id, word,
           dur, log_dur,
           firstfix_dur, log_firstfix_dur,
           firstrun_dur, log_firstrun_dur,
           refix,
           length, length_fct, length_z,
           freq, freq_z, log_freq, log_freq_z,
           firstfix_center_diff, firstfix_center_diff_fct,
           firstfix_center_dist, firstfix_center_dist_fct,
           firstfix_center_dist_sq,
           firstfix_entropy, firstfix_entropy_z,
           entropy_vec) %>%
    
    droplevels()
  
  return(out)
}


optimize_subject_drop <- function(data_by_drop, id, measure){
  
  subj_data_by_drop <- map(data_by_drop, ~filter(.x, uniform_id == id))
  
  if(measure == "firstfix"){
    model_by_drop <-
      map(subj_data_by_drop,
          ~lm(log_firstfix_dur ~ firstfix_entropy + log_freq + length + firstfix_center_diff,
              data = .x))
    
  } else if(measure == "refix"){
    model_by_drop <-
      map(subj_data_by_drop,
          ~glm(refix ~ firstfix_entropy + log_freq + length + firstfix_center_diff,
               data = .x, family = "binomial", nAGQ = 0))
  }
  
  summary_by_drop <- map(model_by_drop, ~tidy(.x))
  statistic_by_drop <- map_dbl(summary_by_drop, ~.x$statistic[2])
  optimal_drop <- seq(0.1, 0.9, 0.1)[which.max(abs(statistic_by_drop))]
  return(optimal_drop)
}



