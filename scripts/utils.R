my_scale <- function(x){
  # Center and scale numeric variables.
  
  return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}

get_corpus_size <- function(lang){
  # Get OpenSubtitles corpus size for given language.
  
  corpus_size <- switch(lang,
                        "en" = eval(L <- 751e6),
                        "sp" = eval(L <- 514e6),
                        "he" = eval(L <- 170e6),
                        "fi" = eval(L <- 117e6),
                        "du" = eval(L <- 265e6),
                        "fr" = eval(L <- 336e6))
  return(corpus_size)
}

get_alphabet <- function(lang){
  # Get alphabet vector for given language.
  
  alphabet <- switch(lang,
                     "en" = eval(alphabet <- str_split("abcdefghijklmnopqrstuvwxyz", pattern=boundary(type="character"), simplify=T)),
                     "sp" = eval(alphabet <- str_split("abcdefghijklmnopqrstuvwxyzáéíñóúü", pattern=boundary(type="character"), simplify=T)),
                     "he" = eval(alphabet <- str_split("אבגדהוזחטיכךלמםנןסעפףצץקרשת", pattern=boundary(type="character"), simplify=T)),
                     "fi" = eval(alphabet <- str_split("abcdefghijklmnopqrstuvwxyzäöå", pattern=boundary(type="character"), simplify=T)),
                     "du" = eval(alphabet <- str_split("abcdefghijklmnopqrstuvwxyz", pattern=boundary(type="character"), simplify=T)),
                     "fr" = eval(alphabet <- str_split("abcdefghijklmnopqrstuvwxyzàâçéèêëîïôûùüÿñæœ", pattern=boundary(type="character"), simplify=T)))
  return(as.character(alphabet))
}
