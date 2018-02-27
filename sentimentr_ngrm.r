library("sentimentr")
library("dplyr")

sample_set = c("worst book in the world!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!: This was the worst book I have ever read in my entire life! I was forced to read it for school. It was complicated and very boring. I would not recommend this book to anyone!! Please don't waste your time and money to read this book!! Read something else!!")
sample_set = get_sentences(sample_set)
sentiment(sample_set) %>%
  subset(select = "sentiment") %>%
  colSums() -> result

# *100 to identify neutral sentiments.
result = as.integer(result*100);
print(result)

if(result==0){
  print("neutral")
}else if(result > 0){
  print("positive")
}else
  print("negative")



print(sample_set)

