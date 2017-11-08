library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

makeWordCloud <- function(inFile, outFile) {
  text <- readLines(inFile)
  docs <- Corpus(VectorSource(text))
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, removeWords, stopwords("english"))
  docs <- tm_map(docs, removeWords, c("will", "shall", "hath", "may"))
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, stripWhitespace)
  
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d, 10)
  
  set.seed(46191)
  
  png(filename=outFile)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  dev.off()
}

makeWordCloud('./data/usdeclar.txt', './output/usdeclar.png')
makeWordCloud('./data/const.txt', './output/const.png')
makeWordCloud('./data/common-sense.txt', './output/common-sense.png')
makeWordCloud('./data/federalist.txt', './output/federalist.png')