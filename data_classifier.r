
library("sentimentr")
library("dplyr")

f = file("/home/akat/Documents/sentiment-dataset/formatted.txt", "r")
pf = file("/home/akat/Documents/sentiment-dataset/positive.txt", "w")
nf = file("/home/akat/Documents/sentiment-dataset/negative.txt", "w")
nef = file("/home/akat/Documents/sentiment-dataset/neutral.txt", "w")

end = FALSE
result =0
text =""

while(!end){

next_line = trimws(readLines(f,n=1))

if(length(next_line)==0){
  end = TRUE
  close(f)
  close(pf)
  close(nf)
  close(nef)
}else if(next_line=="<--->"){
#write to individual files
text = paste(text,toString(result),"\n<--->\n",sep = " ")
result = as.integer(result*1000)

if(result > 0){
  write(text,pf,append=TRUE)
}else if(result < 0){
  write(text,nf,append=TRUE)
}else {
  write(text,nef,append=TRUE)
}

result=0
text=""
}else{

  sentiment(next_line) %>%
    subset(select = "sentiment") %>%
    colSums() -> res

  result = result + res
  text=paste(text,next_line,sep="\n")
}


}
