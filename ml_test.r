library("RTextTools")
library("e1071")

setwd("/home/akat/Documents/sentiment-dataset/")

happy = readLines("./happy.txt")
sad = readLines("./sad.txt")
neutral = readLines("./normal.txt")
happy_test = readLines("./positive_test.txt")
sad_test = readLines("./negative_test.txt")
neutral_test = readLines("./neutral_test.txt")

data = c(happy, sad ,neutral)
data_test= c(happy_test, sad_test ,neutral_test)

data_all = c(data, data_test)

sentiment = c(rep("happy", length(happy) ),
              rep("sad", length(sad)),
            rep("neutral", length(neutral)))

sentiment_test = c(rep("happy", length(happy_test) ),
                   rep("sad", length(sad_test)),
                 rep("neutral", length(neutral_test)))

sentiment_all = as.factor(c(sentiment, sentiment_test))

# naive bayes
mat= create_matrix(data_all, language="english",
                   removeStopwords=FALSE, removeNumbers=TRUE,
                   stemWords=FALSE, tm::weightTfIdf)

mat = as.matrix(mat)

classifier = naiveBayes(mat[1:136,], as.factor(sentiment_all[1:136]))
predicted = predict(classifier, mat[137:274,]);


table(sentiment_test, predicted)
recall_accuracy(sentiment_test, predicted)

# the other methods
mat= create_matrix(data_all, language="english",
                   removeStopwords=FALSE, removeNumbers=TRUE,
                   stemWords=FALSE, tm::weightTfIdf)

container = create_container(mat, as.numeric(sentiment_all),
                             trainSize=1:160, testSize=161:180,virgin=FALSE)

models = train_models(container, algorithms=c("MAXENT",
                                              "SVM",
                                              "GLMNET", "BOOSTING",
                                              "SLDA","BAGGING",
                                              "RF",  "NNET",
                                              "TREE"
))

# test the model
results = classify_models(container, models)
table(as.numeric(as.numeric(sentiment_all[161:180])), results[,"FORESTS_LABEL"])
recall_accuracy(as.numeric(as.numeric(sentiment_all[161:180])), results[,"FORESTS_LABEL"])

# formal tests
analytics = create_analytics(container, results)
summary(analytics)

head(analytics@algorithm_summary)
head(analytics@label_summary)
head(analytics@document_summary)
analytics@ensemble_summary # Ensemble Agreement

# Cross Validation
N=3
cross_SVM = cross_validate(container,N,"SVM")
cross_GLMNET = cross_validate(container,N,"GLMNET")
cross_MAXENT = cross_validate(container,N,"MAXENT")
