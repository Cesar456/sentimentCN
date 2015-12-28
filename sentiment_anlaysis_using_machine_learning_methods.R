###########################################
"Sentiment analysis with machine learning"
##########################################
library(RTextTools)
library(e1071)

pos_tweets =  rbind(
  c('I love this car', 'positive'),
  c('This view is amazing', 'positive'),
  c('I feel great this morning', 'positive'),
  c('I am so excited about the concert', 'positive'),
  c('He is my best friend', 'positive')
)


neg_tweets = rbind(
  c('I do not like this car', 'negative'),
  c('This view is horrible', 'negative'),
  c('I feel tired this morning', 'negative'),
  c('I am not looking forward to the concert', 'negative'),
  c('He is my enemy', 'negative')
)


test_tweets = rbind(
  c('feel happy this morning', 'positive'),
  c('larry friend', 'positive'),
  c('not like that man', 'negative'),
  c('house not great', 'negative'),
  c('your song annoying', 'negative')
)

tweets = rbind(pos_tweets, neg_tweets, test_tweets)

# native bayes
matrix1= create_matrix(tweets[,1], language="english", 
                      removeStopwords=TRUE, removeNumbers=TRUE, 
                      minWordLength=3,
                      stemWords=FALSE, 
                      weighting=tm::weightTfIdf, 
                      ngramLength=1)
mat1 = as.matrix(matrix1)

matrix2= create_matrix(tweets[,1], language="english", 
                       removeStopwords=TRUE, removeNumbers=TRUE, 
                       minWordLength=3,
                       stemWords=FALSE, 
                       weighting=tm::weightTfIdf, 
                       ngramLength=2)
mat2 = as.matrix(matrix2)

matrix3= create_matrix(tweets[,1], language="english", 
                       removeStopwords=TRUE, removeNumbers=TRUE, 
                       minWordLength=3,
                       stemWords=FALSE, 
                       weighting=tm::weightTfIdf, 
                       ngramLength=3)
mat3 = as.matrix(matrix3)

mat = cbind(mat1, mat2, mat3)

mean(colSums(mat))

library(tau)
tokenize_ngrams <- function(x, n=ngramLength) return(names(textcnt(x,method="string",n=n)))

tokenize_ngrams_list <- function(x, n = ngramList){ # ngramList in the form of 1:3
  ngrams_name = NULL
  for (i in n){
    ngrams_name[[i]] =  names(textcnt(x,method="string",n=i))
  }
  return(unlist(ngrams_name))
}
  

txt = "i love this car because it is fast"
tokenize_ngrams(txt, 3)
tokenize_ngrams_list(txt, 1:3)

control <- list(bounds=list(local=c(minDocFreq,maxDocFreq)),language=language,tolower=toLower,removeNumbers=removeNumbers,removePunctuation=removePunctuation,stopwords=removeStopwords,stripWhitespace=stripWhitespace,wordLengths=c(minWordLength,maxWordLength),weighting=weighting)

if (ngramLength > 1) { 
  control <- append(control,list(tokenize=tokenize_ngrams),after=7)
} else {
  control <- append(control,list(tokenize=scan_tokenizer),after=4)
}





classifier = naiveBayes(mat[1:10,], as.factor(tweets[1:10,2]) )
predicted = predict(classifier, mat[11:15,]); predicted

table(predicted, true = tweets[11:15, 2])
accuracy = recall_accuracy(tweets[11:15, 2], predicted); accuracy

# the other methods
container = create_container(matrix, as.numeric(as.factor(tweets[,2])),
                             trainSize=1:10, testSize=11:15,virgin=FALSE) #可以设置removeSparseTerms

models = train_models(container, algorithms=c("MAXENT" , "SVM", "RF", "BAGGING", "TREE"))

results = classify_models(container, models)

# accuracy
table(as.numeric(as.factor(tweets[11:15, 2])), results[,"FORESTS_LABEL"])
table(as.numeric(as.factor(tweets[11:15, 2])), results[,"MAXENTROPY_LABEL"])

recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"FORESTS_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"MAXENTROPY_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"TREE_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"BAGGING_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"SVM_LABEL"])
# model summary
analytics = create_analytics(container, results)
summary(analytics)

head(analytics@document_summary)
analytics@ensemble_summary

N=4
set.seed(2014)
cross_validate(container,N,"MAXENT")
cross_validate(container,N,"TREE")
cross_validate(container,N,"SVM")
cross_validate(container,N,"RF")