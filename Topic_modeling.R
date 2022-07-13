library(KoNLP)
useSejongDic()
library(jsonlite)
library(httr)
library(tm)
library(qgraph)

searchUrl<- "https://openapi.naver.com/v1/search/blog.json"
Client_ID <- "ID"
Client_Secret <- "PASSWORD"

query <- URLencode("MZ세대소비") ; 
url <- paste0(searchUrl, "?query=", query, "&display=100")
response <- GET(url, add_headers("Content_Type" = "application/json",
                                 "X-Naver-client-Id" = Client_ID, "X-naver-Client-Secret" = Client_Secret))

json_data <- httr::content(response, type='text', encoding="UTF-8")
json_obj <- fromJSON(json_data)
df <- data.frame(json_obj)
head(df)
MZ <- df$items.description
MZ
MZ <- gsub("</?b>", "", MZ) 
MZ <- gsub("&.+;", "", MZ)
MZ <- gsub("[[:digit:][:punct:][:lower:][:upper:]]", "", MZ)
MZ <- gsub("\\s{2,}", " ", MZ)
MZ

extractednoun <- extractNoun(MZ)
cdata <- unlist(extractednoun)
cdata <- Filter(function(x) {nchar(x)<6 & nchar(x)>=2}, cdata)
head(cdata)

library(lda)
k <- 4
set.seed(1004)
num.iterations <- 1000

##LDA를 위한 절차. gsub는 데이터 전처리 과정

corpus <- lexicalize(cdata, lower=TRUE)
corpus$vocab<-gsub("\\(","",corpus$vocab)
corpus$vocab<-gsub("c","",corpus$vocab)
corpus$vocab<-gsub("\\)","",corpus$vocab)
corpus$vocab<-gsub("\"","",corpus$vocab)
corpus$vocab<-gsub(",","",corpus$vocab)

##LDA

result <- lda.collapsed.gibbs.sampler(corpus$documents, K, corpus$vocab,
                                      num.iterations, 0.1, 0.1, compute.log.likelihood = TRUE)

##주제별 500개 단어 확인

top.words <- top.topic.words(result$topics, 500, by.score = TRUE)
print(top.words)
