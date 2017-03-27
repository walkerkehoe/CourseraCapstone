library(tm)
library(RWeka)
library(parallel)

#Function that cleanses data
cleanse <- function (dirtyText) {
  cleanText <- tm_map(dirtyText,content_transformer(function(x) iconv(x, to="UTF-8", sub="byte")))
  cleanText <- tm_map(cleanText, content_transformer(tolower))
  cleanText <- tm_map(cleanText, removeNumbers)
  cleanText <- tm_map(cleanText, removePunctuation)
  removeURL <- function(x) gsub("http[[:alnum:]]*", "", x) 
  cleanText <- tm_map(cleanText, content_transformer(removeURL))
  #cleanText <- tm_map(cleanText, removeWords, stopwords("english"))
  cleanText <- tm_map(cleanText, stripWhitespace)
  cleanText <- tm_map(cleanText, stemDocument)
  cleanText <- tm_map(cleanText, PlainTextDocument)
  cleanText
}

#Function that creates N-grams
Ngramize <- function(cleanText, n) {
  ng <- NGramTokenizer(cleanText, Weka_control(min = n, max = n, delimiters = " \\r\\n\\t.,;:\"()?!"))
  ng <- data.frame(table(ng))
  ng <- ng[order(ng$Freq, decreasing = TRUE),]
  colnames(ng) <- c("String","Count")
  ng
}

##End functions#################

options(mc.cores=1)

#Importing data
file.list = c("final/en_US/en_US.blogs.txt", "final/en_US/en_US.news.txt", "final/en_US/en_US.twitter.txt")
docs <- list(blogs = "", news = "", twitter = "")
for (i in 1:3) {
  con <- file(file.list[i], "rb")
  docs[[i]] <- readLines(con, encoding = "UTF-8",skipNul = TRUE)
  close(con)
}

#Sample 10000 words from each document and write to .RData files
docs_sample <- NULL
for (i in 1:3) {
  docs_sample <- append(docs_sample,sample(docs[[i]],5000))
}
corpus <- VCorpus(VectorSource(docs_sample))
corpus <- cleanse(corpus)
dfCorpus <-data.frame(text=unlist(sapply(corpus,`[`, "content")), stringsAsFactors = FALSE)
grams <- c('unigram','bigram','trigram','quadgram')
for (i in 1:4) {
  xgram <- Ngramize(dfCorpus,i)
  saveRDS(xgram, file = paste0("./",grams[i],".RData"))
}







