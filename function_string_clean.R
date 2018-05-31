StringClean = function(text, digit.del = 1, keep.only.char = 1,
                       stop.lan = "french", 
                       custom.stop.words = NULL){
  #lower case
  text = tolower(text)
  
  #remove stop words
  stop_words <- tm::stopwords(kind = stop.lan) #french
  stop_words <- c(stop_words, custom.stop.words)
  text = removeWords(text,stop_words)
  
  #remove double quote
  text <- gsub(x = text, pattern = '"', replacement = " ")
  
  #remove simple quote
  text <- gsub(x = text, pattern = "'", replacement = " ")
  
  #remove backslash
  text <- gsub(x = text, pattern = "\\\\", replacement = " ")
  
  # remove digits and punctuation
  if (digit.del == 1){
    text <- gsub(x = text, pattern = "[[:digit:]]|[[:punct:]]", replacement = " ")
  } else if (digit.del ==0) {
    text <- gsub(x = text, pattern = "[[:punct:]]", replacement = " ")
  }
  
  # keep only alphabet characters
  if (keep.only.char == 1){text <- str_replace_all(text, "[^[:alpha:]]", " ")}
  
  # remove tabulation
  text <- str_replace_all(text, "\t", " ")
  
  # remove white space
  whitespace_rm <- function(vector){
    count=1
    while(length(grep("  ",vector))>0){
      vector <- str_replace_all(vector, "  ", " ")
      count=count+1
    }
    return(vector)
  }
  text <- whitespace_rm(text)
  return(text)
}
