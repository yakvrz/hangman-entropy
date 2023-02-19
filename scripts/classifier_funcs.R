language_classifier <- function(word_entropy_vectors, mean_entropy_vectors, languages = c("en", "he", "sp"),
                                target_lang, target_length, center, sample_size, iterations){
  
  # Extract data and reference trajectory for target length
  filtered_word_entropy_vectors <-
    word_entropy_vectors %>%
    filter(lang == target_lang,
           length == target_length,
           centering == center)
  
  filtered_mean_entropy_vectors <-
    mean_entropy_vectors %>%
    filter(length == target_length,
           centering == center) %>%
    .$value
  
  # Sample and classify
  out <-
    map(1:iterations, \(i){
      # randomly choose sample language
      # target_lang <- sample(c("en", "he", "sp"), 1)
      
      # sample word trajectories
      sample_word_entropy_vectors <-
        filtered_word_entropy_vectors %>%
        slice_sample(n = sample_size, replace = TRUE) %>%
        .$value
      
      # calculate error w.r.t to language-mean trajectories
      mean_sample_trajectory <- rowMeans(as.data.frame(sample_word_entropy_vectors))
      sample_MAE <- map_dbl(filtered_mean_entropy_vectors, function(X){
        mean(abs(X - mean_sample_trajectory))
      })
      
      return(sample_MAE)
    })
  
  return(out)
}

language_classifier_diagnostics <- function(word_entropy_vectors, mean_entropy_vectors, languages = c("en", "he", "sp"),
                                            target_lang, target_length, center, sample_size, iterations){
  
  # Extract data and reference trajectory for target length
  filtered_word_entropy_vectors <-
    word_entropy_vectors %>%
    filter(lang == target_lang,
           length == target_length,
           centering == center)
  
  filtered_mean_entropy_vectors <-
    mean_entropy_vectors %>%
    filter(length == target_length,
           centering == center) %>%
    .$value
  
  # Sample and classify
  out <-
    map(1:iterations, \(i){
      # randomly choose sample language
      # target_lang <- sample(c("en", "he", "sp"), 1)
      
      # sample word trajectories
      sample_word_entropy_vectors <-
        filtered_word_entropy_vectors %>%
        slice_sample(n = sample_size, replace = TRUE) %>%
        .$value
      
      # calculate error w.r.t to language-mean trajectories
      mean_sample_trajectory <- rowMeans(as.data.frame(sample_word_entropy_vectors))
      
      # by-word MSE
      word_MSE <- map_dbl(filtered_mean_entropy_vectors, function(X){
        mean(map_dbl(sample_word_entropy_vectors, function(x) mean((X - x)^2)))
      })
      
      # sample MSE
      sample_MSE <- map_dbl(filtered_mean_entropy_vectors, function(X){
        mean((X - mean_sample_trajectory)^2)
      })
      
      # by-word MAE
      word_MAE <- map_dbl(filtered_mean_entropy_vectors, function(X){
        mean(map_dbl(sample_word_entropy_vectors, function(x) mean(abs(X - x))))
      })
      
      # sample MAE
      sample_MAE <- map_dbl(filtered_mean_entropy_vectors, function(X){
        mean(abs(X - mean_sample_trajectory))
      })
      
      error_list <-
        list("word_MSE" = word_MSE,
             "sample_MSE" = sample_MSE, 
             "word_MAE" = word_MAE,
             "sample_MAE" = sample_MAE)
      
      return(error_list)
    })
  
  return(out)
}


which_min_break_ties <- function(x){
  
  y <- seq_along(x)[x == min(x)]
  if (length(y) > 1L)
    sample(y, 1L)
  else y
}