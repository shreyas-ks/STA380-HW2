library(tm)
library(randomForest)
library(e1071)
library(rpart)
library(ggplot2)
library(caret)


readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), id=fname, language='en') }

author_dirs = Sys.glob('ReutersC50/C50train/*')
file_list = NULL
train_labels = NULL
for(author in author_dirs) {
  author_name = substring(author, first=23)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  train_labels = append(train_labels, rep(author_name, length(files_to_add)))
}

all_docs = lapply(file_list, readerPlain) 
names(all_docs) = file_list
names(all_docs) = sub('.txt', '', names(all_docs))
train_corpus = Corpus(VectorSource(all_docs))
names(train_corpus) = file_list
train_corpus = tm_map(train_corpus, content_transformer(tolower)) 
train_corpus = tm_map(train_corpus, content_transformer(removeNumbers)) 
train_corpus = tm_map(train_corpus, content_transformer(removePunctuation)) 
train_corpus = tm_map(train_corpus, content_transformer(stripWhitespace)) 
train_corpus = tm_map(train_corpus, content_transformer(removeWords), stopwords("SMART"))
DTM_train = DocumentTermMatrix(train_corpus)
DTM_train = removeSparseTerms(DTM_train, 0.975)
DTM_train
author_dirs = Sys.glob('ReutersC50/C50test/*')
file_list = NULL
test_labels = NULL
for(author in author_dirs) {
  author_name = substring(author, first=22)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  test_labels = append(test_labels, rep(author_name, length(files_to_add)))
}
all_docs = lapply(file_list, readerPlain) 
names(all_docs) = file_list
names(all_docs) = sub('.txt', '', names(all_docs))
test_corpus = Corpus(VectorSource(all_docs))
names(test_corpus) = file_list
test_corpus = tm_map(test_corpus, content_transformer(tolower)) 
test_corpus = tm_map(test_corpus, content_transformer(removeNumbers)) 
test_corpus = tm_map(test_corpus, content_transformer(removePunctuation)) 
test_corpus = tm_map(test_corpus, content_transformer(stripWhitespace)) 
test_corpus = tm_map(test_corpus, content_transformer(removeWords), stopwords("SMART"))
reuters_dict = NULL
reuters_dict = dimnames(DTM_train)[[2]]
DTM_test = DocumentTermMatrix(test_corpus, list(dictionary=reuters_dict))
DTM_test = removeSparseTerms(DTM_test, 0.975)
DTM_train_df = as.data.frame(inspect(DTM_train))
DTM_test_df = as.data.frame(inspect(DTM_test))
model_NB = naiveBayes(x=DTM_train_df, y=as.factor(train_labels), laplace=1)
pred_NB = predict(model_NB, DTM_test_df)

