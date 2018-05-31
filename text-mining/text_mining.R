TextFeatureGeneraator = function(df, feature, id,
                                 pruning.level, 
                                 lan = "french"){
  train.token <- df[, get(feature)] %>%word_tokenizer
  
  NLP.train <- itoken(train.token, ids = df[, get(id)], progressbar = FALSE)
  
  NLP.dictionary <- create_vocabulary(NLP.train, stopwords = tm::stopwords(kind = lan), 
                                      ngram = c(ngram_min = 1L, ngram_max = 4L))
  
  
  NLP.dictionary.pruned <- prune_vocabulary(NLP.dictionary,
                                            term_count_min = pruning.level,
                                            doc_proportion_max = 0.5)
  nrow(NLP.dictionary.pruned)
  
  NLP.vectorizer <- vocab_vectorizer(NLP.dictionary.pruned)
  
  NLP.matrix.train <- create_dtm(NLP.train, NLP.vectorizer)
  message(sprintf("%s more features have been created.", dim(NLP.matrix.train)[2]))
  
  test = as.matrix(NLP.matrix.train) %>% data.table()
  return(test)
}
