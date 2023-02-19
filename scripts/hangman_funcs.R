calculate_bool_mask_probs <- function(drop, max_length){
  # Calculate the probabilities of all letter masking combinations by fixation position,
  # for the given drop parameter and up to the given sequence length.
  
  mask_prob_lists <- map(1:max_length, function(len){
    masks <- calculate_bool_masks(len)
    mask_probs <- map(1:len, function(pos){
      map(masks, function(mask){
        acuity <- pmax(1 - (abs(1:len - pos) * drop), 0)
        mask_prob <- prod(map2_dbl(mask, acuity, function(seen, prob) if_else(seen, prob, 1 - prob)))
        return(mask_prob)
      })
    })
    return(mask_probs)
  })
  return(mask_prob_lists)
}


calculate_bool_masks <- function(len){
  # Return all positional masking combinations for a given sequence length.
  
  binary_sequences <- R.utils::intToBin(0:(2 ^ len - 1))
  masks <- map(str_split(binary_sequences, pattern = ""), ~as.logical(as.numeric(.x)))
  return(masks)
}


hangman_entropy <- function(target, corpus, drop, drop_type = "linear", bool_masks_by_length, bool_mask_probs_by_length, method = "precise", n_iter = NULL){
  # Calculate the mean orthographic-lexical (i.e., "hangman") entropy in each fixation position of a target word.
  
  len <- nchar(target) # Get target length
  corpus <- filter(corpus, length == len) # Filter corpus by target length
  target_letters <- str_split(target, "", simplify = TRUE) # Split target into letters
  bool_masks <- bool_masks_by_length[[len]] # Get all letter masking combinations
  
  # Apply masks to the target word and translate to regular expressions
  regex_masks <- map(bool_masks, ~str_c("\\A", str_flatten(if_else(.x, target_letters, ".")), "\\Z", sep = ""))
  
  # Diagnostics
  # candidates <- map(regex_masks, ~corpus$word[which(str_detect(corpus$word, .x))])
  # masked <- map_chr(bool_masks, ~str_flatten(if_else(.x, target_letters, ".")))
  # view(masked)
  # View(candidates)
  
  # Calculate the orthographic-lexical entropy for all possible letter-masking combinations
  candidate_freq_by_mask <- map(regex_masks, ~corpus$freq[which(str_detect(corpus$word, .x))])
  candidate_relative_freq_by_mask <- map(candidate_freq_by_mask, ~.x / sum(.x))
  entropy_by_mask <- map(candidate_relative_freq_by_mask, ~-sum(.x * log2(.x)))
  
  # Calculate the mean entropy in each fixation position
  mask_probs_by_pos <- bool_mask_probs_by_length[[len]]
  mean_entropy_by_pos <- map_dbl(1:len, function(pos){
    sum(as.numeric(mask_probs_by_pos[[pos]]) * as.numeric(entropy_by_mask))
  })
  
  # Alternatively, estimate via sampling
  if (method == "sample"){
    mean_entropy_by_pos <- map_dbl(1:len, function(pos){
      acuity <- case_when(drop_type == "linear" ~ pmax(1 - (abs(1:len - pos) * drop), 0),
                          drop_type == "quadratic" ~ pmax(1 - (abs(1:len - pos) * drop)^2, 0))
      pos_entropy <- map_dbl(1:n_iter, \(i){
        bool_mask <- runif(length(acuity)) < acuity
        match_ind <- which(map_dbl(bool_masks, ~prod(.x == bool_mask)) == 1)
        return(entropy_by_mask[[match_ind]])
      })
      return(mean(pos_entropy))
    })
  }
  
  return(as.data.frame(t(mean_entropy_by_pos)))
}


hangman_candidates <- function(target, corpus, drop, drop_type = "linear", bool_masks_by_length, bool_mask_probs_by_length, method = "precise", n_iter = NULL){
  # Calculate the mean number of orthographic-lexical (i.e., "hangman") candidates in each fixation position of a target word.
  
  len <- nchar(target) # Get target length
  corpus <- filter(corpus, length == len) # Filter corpus by target length
  target_letters <- str_split(target, "", simplify = TRUE) # Split target into letters
  bool_masks <- bool_masks_by_length[[len]] # Get all letter masking combinations
  
  # Apply masks to the target word and translate to regular expressions
  regex_masks <- map(bool_masks, ~str_c("\\A", str_flatten(if_else(.x, target_letters, ".")), "\\Z", sep = ""))
  
  # Calculate the number of orthographic-lexical candidates for all possible letter-masking combinations
  candidate_indices_by_mask <- map(regex_masks, ~which(str_detect(corpus$word, .x)))
  candidate_numbers_by_mask <- map(candidate_indices_by_mask, ~length(.x))
  
  # Calculate the mean number of candidates in each fixation position
  mask_probs_by_pos <- bool_mask_probs_by_length[[len]]
  mean_candidates_by_pos <- map_dbl(1:len, function(pos){
    sum(as.numeric(mask_probs_by_pos[[pos]]) * as.numeric(candidate_numbers_by_mask))
  })
  
  # Alternatively, estimate via sampling
  if (method == "sample"){
    mean_candidates_by_pos <- map_dbl(1:len, function(pos){
      acuity <- case_when(drop_type == "linear" ~ pmax(1 - (abs(1:len - pos) * drop), 0),
                          drop_type == "quadratic" ~ pmax(1 - (abs(1:len - pos) * drop)^2, 0))
      pos_candidates <- map_dbl(1:n_iter, \(i){
        bool_mask <- runif(length(acuity)) < acuity
        match_ind <- which(map_dbl(bool_masks, ~prod(.x == bool_mask)) == 1)
        return(candidate_numbers_by_mask[[match_ind]])
      })
      return(mean(pos_candidates))
    })
  }
  
  return(as.data.frame(t(mean_candidates_by_pos)))
}



