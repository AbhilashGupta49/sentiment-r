library("tidytext")
library("dplyr")

sample_set = c("this product is nice","it did not work for me","delivery was late but the product is okay","is worth the price","can get better quality at this price")
n = length(sample_set)
size = c(1:n)
data_tib = tibble(size, sample_set)
data_tib %>%
        unnest_tokens(word,sample_set) %>%
        anti_join(stop_words,by="word") %>%
        inner_join(get_sentiments("nrc"), by = "word" ) %>%
        subset(select = "sentiment")
