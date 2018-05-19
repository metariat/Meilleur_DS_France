WordFrequency = function(text){
  #----------------------------------------------------# 
  # This function calculate the frequency of each word in a string in 
  #   descending order
  # @text: a input character vector
  # @d : data.frame output containing each word and its frequency in 
  #   the vecter @text
  #----------------------------------------------------# 
  
  #trainsform the original text
  text_ = VectorSource(text)
  
  #intermediate function: replace a word by space in a string
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x)) 
  
  docs = Corpus(text_)
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  
  #calculate the frequency
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  return(d)
}


