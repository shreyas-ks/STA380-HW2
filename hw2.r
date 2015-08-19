require(ggplot2)
require(gridExtra)
require(RColorBrewer)

abia = read.csv("ABIA.csv", header=TRUE)

# Retain only rows where SecurityDelay is NA
#SecurityNA <- subset(abia, is.na(abia$SecurityDelay))

security <- subset(abia, !is.na(abia[,28]))
security <- subset(security, SecurityDelay>0)

NAS <- subset(abia, !is.na(abia[,27]))
NAS <- subset(NAS, NASDelay>0)

LateAircraft <- subset(abia, !is.na(abia[,29]))
LateAircraft <- subset(LateAircraft, LateAircraftDelay>0)

carrier <- subset(abia, !is.na(abia[,25]))
carrier <- subset(carrier, CarrierDelay>0)

weather <- subset(abia, !is.na(abia[,26]))
weather <- subset(weather, WeatherDelay>0)

ggplot(weather, aes(DepDelay, WeatherDelay, color=UniqueCarrier)) + geom_point()


incoming <- subset(abia, Dest == "AUS")
outgoing <- subset(abia, Origin == "AUS")

in_weather <- subset(incoming, !is.na(incoming[,26]))
in_weather <- subset(in_weather, WeatherDelay>0)

ggplot(in_weather, aes(DepDelay, WeatherDelay, size = Distance, color=factor(UniqueCarrier))) + 
  geom_point(alpha=0.5) + 
  geom_abline(a=0, b=1) + ggtitle("Incoming Flights") + guides(col=guide_legend(ncol=2))

ggplot(in_weather, aes(DepDelay, WeatherDelay, size = Distance)) + 
  geom_point(aes(colour=UniqueCarrier),shape=24) + 
  geom_abline(a=0, b=1) + ggtitle("Incoming Flights") + guides(col=guide_legend(ncol=2))

  
out_weather <- subset(outgoing, !is.na(incoming[,26]))
out_weather <- subset(out_weather, WeatherDelay>0)

ggplot(out_weather, aes(DepDelay, WeatherDelay, size = Distance, color=factor(UniqueCarrier))) + 
  geom_point() + 
  geom_abline(a=0, b=1) + ggtitle("Outgoing Flights")


plot1 <- ggplot(in_weather, aes(DepDelay, WeatherDelay, color=factor(UniqueCarrier))) + geom_point() + 
  geom_abline(a=0, b=1) + ggtitle("Incoming Flights") + guides(col=guide_legend(ncol=2))
plot2 <- ggplot(out_weather, aes(DepDelay, WeatherDelay, color=factor(UniqueCarrier))) + geom_point() + 
  geom_abline(a=0, b=1) + ggtitle("Outgoing Flights") + guides(col=guide_legend(ncol=2))

grid.arrange(plot1, plot2, nrow=2, ncol=1)


in_weather$ex_del = in_weather$DepDelay - in_weather$WeatherDelay
out_weather$ex_del = out_weather$DepDelay - out_weather$WeatherDelay
exdel = rbind(in_weather, out_weather)
excess_delay <- subset(exdel, ex_del>0)


ggplot(in_weather, aes(WeatherDelay, DepDelay, size = Distance)) + 
  geom_point(aes(colour=UniqueCarrier),shape=24) + geom_abline(a=0, b=1) + 
  ggtitle("Delays in Incoming Flights") + guides(col=guide_legend(ncol=2))



ggplot(excess_delay, aes(Origin,ex_del)) + geom_bin2d() + ylab("Excess Delay") + xlab("Origin") + 
  ggtitle("Excess Delay by Origin") + theme(axis.text.x  = element_text(angle=90))




####### qn2 #######
library(tm)

##################################################################
##################################################################
##################################################################
###################### doesn't fucking work ######################
##################################################################
##################################################################
##################################################################

readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }

train_author_dirs = Sys.glob('ReutersC50/C50train/*')
train_file_list = NULL
train_labels = NULL
for(author in train_author_dirs) {
  author_name = substring(author, first=29)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(train_file_list, files_to_add)
  train_labels = append(train_labels, rep(author_name, length(files_to_add)))
}

# Need a more clever regex to get better names here
train_all_docs = lapply(file_list, readerPlain) 
names(train_all_docs) = file_list
names(train_all_docs) = sub('.txt', '', names(train_all_docs))

train_my_corpus = Corpus(VectorSource(train_all_docs))
names(train_my_corpus) = train_file_list

# Preprocessing
train_my_corpus = tm_map(train_my_corpus, content_transformer(tolower)) # make everything lowercase
train_my_corpus = tm_map(train_my_corpus, content_transformer(removeNumbers)) # remove numbers
train_my_corpus = tm_map(train_my_corpus, content_transformer(removePunctuation)) # remove punctuation
train_my_corpus = tm_map(train_my_corpus, content_transformer(stripWhitespace)) ## remove excess white-space
train_my_corpus = tm_map(train_my_corpus, content_transformer(removeWords), stopwords("SMART"))

train_DTM = DocumentTermMatrix(train_my_corpus)
train_DTM = removeSparseTerms(train_DTM, 0.975)
train_DTM

test_author_dirs = Sys.glob('ReutersC50/C50test/*')
test_file_list = NULL
test_labels = NULL
for(author in test_author_dirs) {
  author_name = substring(author, first=29)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(test_file_list, files_to_add)
  test_labels = append(test_labels, rep(author_name, length(files_to_add)))
}

# Need a more clever regex to get better names here
test_all_docs = lapply(file_list, readerPlain) 
names(test_all_docs) = file_list
names(test_all_docs) = sub('.txt', '', names(test_all_docs))

test_my_corpus = Corpus(VectorSource(test_all_docs))
names(test_my_corpus) = test_file_list

# Preprocessing
test_my_corpus = tm_map(test_my_corpus, content_transformer(tolower)) # make everything lowercase
test_my_corpus = tm_map(test_my_corpus, content_transformer(removeNumbers)) # remove numbers
test_my_corpus = tm_map(test_my_corpus, content_transformer(removePunctuation)) # remove punctuation
test_my_corpus = tm_map(test_my_corpus, content_transformer(stripWhitespace)) ## remove excess white-space
test_my_corpus = tm_map(test_my_corpus, content_transformer(removeWords), stopwords("SMART"))

reuters_dict = NULL
list(dictionary=reuters_dict)
DTM_test = DocumentTermMatrix(test_my_corpus, list(dictionary=reuters_dict))

test_DTM = DocumentTermMatrix(test_my_corpus)
test_DTM = removeSparseTerms(test_DTM, 0.975)
test_DTM

DTM_test = removeSparseTerms(DTM_test, 0.975)

DTM_train_df = as.data.frame(inspect(train_DTM))
DTM_test_df = as.data.frame(inspect(DTM_test))

model_NB = naiveBayes(x=DTM_train_df, y=as.factor(train_labels), laplace=1)


conf_RF$overall


#detach("package:tm", unload=TRUE)



###### qn3 ##########

# Association rule mining
# Adapted from code by Matt Taddy
library(arules)  # has a big ecosystem of packages built around it

# Read in playlists from users
groc <-read.transactions("groceries.txt", format="basket", sep=",")

# First create a list of baskets: vectors of items by consumer
# Analagous to bags of words

## Remove duplicates ("de-dupe")
groc <- groc[!duplicated(groc)]
duplicated(groc)
#groc_frame <- as.data.frame(groc)

# Now run the 'apriori' algorithm
# Look at rules with support > .01 & confidence >.5 & length (# artists) <= 4
grocery <- apriori(groc, parameter=list(support=.005, confidence=.5, maxlen=6))

# Look at the output
inspect(grocery)

## Choose a subset
inspect(subset(grocery, subset=lift > 3))
inspect(subset(grocery, subset=confidence > 0.7))
inspect(subset(grocery, subset=support > .01 & confidence > 0.6))




