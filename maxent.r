library(RTextTools)
library(e1071)

setwd("/home/akat/Documents/sentiment-dataset/final-dataset")

posFile = "./posout.txt"
negFile = "./negout.txt"
neuFile = "./neuout.txt"

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
count = 2820
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
  c('Great CD: My lovely Pat has one of the GREAT voices of her generation. I have listened to this CD for YEARS and I still LOVE IT. When Im in a good mood it makes me feel better. A bad mood just evaporates like sugar in the rain. This CD just oozes LIFE. Vocals are jusat STUUNNING and lyrics just kill. One of lifes hidden gems. This is a desert isle CD in my book. Why she never made it big is just beyond me. Everytime I play this, no matter black, white, young, old, male, female EVERYBODY says one thing Who was that singing ?', 'positive'),
  c('Unique Weird Orientalia from the 1930s: Exotic tales of the Orient from the 1930s. Dr Shen Fu, a Weird Tales magazine reprint, is about the elixir of life that grants immortality at a price. If youre tired of modern authors who all sound alike, this is the antidote for you. Owens palette is loaded with splashes of Chinese and Japanese colours. Marvelous.', 'positive'),
  c('Incorrect Disc: I love the style of this, but after a couple years, the DVD is giving me problems. It doesnt even work anymore and I use my broken PS2 Now. I wouldnt recommend this, Im just going to upgrade to a recorder now. I wish it would work but I guess im giving up on JVC. I really did like this one... before it stopped working. The dvd player gave me problems probably after a year of having it.', 'negative'),
  c('Long and boring: Ive read this book with much expectation, it was very boring all through out the book', 'negative'),
  c('Doublecharged for shipping because merchant was backordered: Merchant was out of stock on the second pair of pants, so when it was shipped I was charged $9 again when the second pair was in stock weeks later.Will not buy from Amazon vendors again.', 'negative')
)


print("binding dataset")

dataset = rbind(positive, negative,neutral ,test)
print("creating matrix")
matrix= create_matrix(dataset[,1], language="english", ngramLength=3,
                      removeStopwords=TRUE, removeNumbers=TRUE, removePunctuation=TRUE,
                      stemWords=FALSE)
mat = as.matrix(matrix)

print("training model")
container = create_container(mat, as.numeric(dataset),
                             trainSize=1:8460, testSize=8461:8465,virgin=FALSE)

models = train_models(container, algorithms=c("MAXENT"))


print("predicting")


predicted = classify_models(container, models);

print(predicted)


table(dataset[5641:5645,2], predicted)
recall_accuracy(dataset[5641:5645, 2], predicted)
