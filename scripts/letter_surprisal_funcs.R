extract_letter_bigrams <- function(x){
    
    x_padded <- str_c("0", x, "1")
    chars <- str_split(x_padded, "", simplify = TRUE)
    y <- c()
    for (i in 1:(length(chars) - 1)){
        y[i] <- str_c(chars[i], chars[i+1])
    }
    return(y)
}

calculate_letter_surprisal <- function(x, tm){
    
    x_padded <- str_c("0", x, "1")
    chars <- str_split(x_padded, "", simplify = TRUE)
    y <- map2_dbl(chars[-length(chars)],
                  chars[-1],
                  ~ -log2(tm[.x, .y]))
    
    return(y)
}

calculate_letter_entropy <- function(x, tm){
    
    chars <- str_split(x, "", simplify = TRUE)
    y <- map_dbl(chars[-length(chars)],
                 ~ -sum(tm[.x, ] * log2(tm[.x, ])))
    return(y)
}