#' A function to clean a corpus
#' 
#' This function takes a tm corpus and does some standard cleaning
#' 
#' 
#' 
#' 


clean_corpus <- function(corp) {
  corp %>%
    tm::tm_map(content_transformer(tolower)) %>%
    tm::tm_map(tm::removePunctuation) %>%
    tm::tm_map(tm::stripWhitespace)
}