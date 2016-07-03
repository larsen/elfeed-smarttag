# https://eight2late.wordpress.com/2015/07/22/a-gentle-introduction-to-cluster-analysis-using-r/

library(tm)
library(cluster)

docs <- Corpus(DirSource("~/.elfeed-smarttag/corpus/"))

docs <- tm_map(docs,content_transformer(tolower))
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "‘")
docs <- tm_map(docs, toSpace, "•")
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)

docs <- tm_map(docs, stemDocument)

dtm <- DocumentTermMatrix(docs)
m <- as.matrix(dtm)
d <- dist(m)
groups <- hclust(d, method="ward.D")
plot(groups, hang=-1)
kfit <- kmeans(d, 2, nstart=100)
