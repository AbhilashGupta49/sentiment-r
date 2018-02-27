library(RTextTools)
library(e1071)

setwd("/home/akat/Documents/sentiment-dataset/final-dataset/")

posFile = "./positive.txt"
negFile = "./negative.txt"
neuFile = "./neutral.txt"

conn = file(posFile,open="r")
lines <- readLines(conn)
res= "";
positive=rbind();
for (i in 1:length(lines)){
   if(lines[i] == "<--->"){
     positive= rbind(positive, c(res,"pos"))
   }
   else{
     temp  = gsub('[[:punct:] ]+',' ',lines[i])
     res  = paste(res,temp)
   }
}
close(conn)

conn = file(negFile,open="r")
lines <- readLines(conn)
res= "";
negative=rbind();
for (i in 1:length(lines)){
   if(lines[i] == "<--->"){
     negative= rbind(negative, c(res, "neg"))
   }
   else{
     temp  = gsub('[[:punct:] ]+',' ',lines[i])
     res  = paste(res,temp)
   }
}
close(conn)

conn = file(neuFile,open="r")
lines <- readLines(conn)
res= "";
neutral=rbind();
for (i in 1:length(lines)){
   if(lines[i] == "<--->"){
     neutral= rbind(neutral, c(res, "neu"))
   }
   else{
     temp  = gsub('[[:punct:] ]+',' ',lines[i])
     res  = paste(res,temp)
   }
}
close(conn)

test = rbind(
  c("i feel verryhappy with this product", "pos"),
  c("This makes me feel young", "pos"),
  c("i do not like his writing style", "neg"),
  c("This product is now worth the price", "neg"),
  c("It is what you  can expect from this price range", "neu")
)


dataset = rbind(positive, negative,neutral,test)

matrix= create_matrix(dataset[,1], language="english",
                      removeStopwords=FALSE, removeNumbers=TRUE,
                      stemWords=FALSE)
mat = as.matrix(matrix)


classifier = naiveBayes(mat[1:428,], as.factor(dataset[1:428,2]) )

predicted = predict(classifier,newdata= mat[424:428,]);

table(mat[424:428,2], predicted)
