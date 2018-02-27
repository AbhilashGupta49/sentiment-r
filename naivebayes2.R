library(RTextTools)
library(e1071)

setwd("/home/akat/Documents/sentiment-dataset/final-dataset")

posFile = "./posout.txt"
negFile = "./negout.txt"
neuFile = "./neutral.txt"

conn = file(posFile,open="r")
lines <- readLines(conn)
res= ""
positive=rbind()
count = 2820
for (i in 1:length(lines)){
  if(lines[i] == "<--->"){
    temp = rbind(c(res,"positive"))
    positive= rbind(positive,temp)
    res= ""
    count=count-1
  }
  else{
    temp  =  gsub(pattern="[[:punct:]]", lines[i], replacement=" ")
    res  = paste(res,temp)
  }

  if(count <=  0){
  break
  }
}

print("positive bound")


close(conn)

conn = file(negFile,open="r")
lines <- readLines(conn)
res= "";
negative=rbind();
count = 2820
for (i in 1:length(lines)){
  if(lines[i] == "<--->"){
    temp = rbind(c(res,"negative"))
    negative= rbind(negative,temp)
    res= ""
    count=count-1
  }
  else{
    temp  =  gsub(pattern="[[:punct:]]", lines[i], replacement=" ")
    res  = paste(res,temp)
  }

  if(count <=  0){
  break
  }
}

print("negative bound")

close(conn)

conn = file(neuFile,open="r")
lines <- readLines(conn)
res= ""
neutral=rbind()
count = 141
for (i in 1:length(lines)){
  if(lines[i] == "<--->"){
    temp = rbind(c(res,"neutral"))
    neutral= rbind(neutral,temp)
    res= ""
    count=count-1
  }
  else{
    temp  = gsub(pattern="[[:punct:]]", lines[i], replacement=" ")
    res  = paste(res,temp)
  }

  if(count <=  0){
  break
  }
}

print("neutral bound")
close(conn)

test = rbind(
  c('good', 'positive'),
  c('nice', 'positive'),
  c('bad', 'negative'),
  c('poor', 'negative'),
  c('great', 'positive')
)


print("binding dataset")

dataset = rbind(positive,negative,test)
print("creating matrix")
matrix= create_matrix(dataset[,1], language="english", ngramLength=1,
                      removeStopwords=TRUE, removeNumbers=TRUE, removePunctuation=TRUE,
                      stemWords=FALSE)
mat = as.matrix(matrix)

print("training model")

classifier = naiveBayes(mat[1:5640,], as.factor(dataset[1:5640,2]) )

print("predicting")
predicted = predict(classifier,mat[5641:5645,]);predicted

print(predicted)


table(dataset[5641:5645,2], predicted)
recall_accuracy(dataset[5641:5645, 2], predicted)																																																																																													
