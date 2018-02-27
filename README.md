# sentiment-r
BlackRock Project: sentiment analysis of amazon user reviews in R.
(in active development)

Note - Requires sentimentr package and other dependencies of sentimentr to be externally downloaded via CRAN repository in a .zip or .tar.gz format. Do not add a install.package(sentimentr) line in the R files otherwise it will show connection error - contrib.url not able to connect.

Steps to run -

1. Run app.r from Rstudio
2. Enter URL in the input box

A new file will be created (test_data.txt) which can be used to test our algorithm.
This file contains first 10 reviews(or first valid reviews) and their classifications into Positive,Negative or Neutral according to ratings given to the product by the customer.

Still to do - 
1. Error Handling
2. UI Improvements
3. Apply Naive Bayes to train our dataset and then compare it with the reviews recieved from scraping url.
4. Display a final output screens with 3 tabs - 
  a. Statistics - includes PositiveSum, NegativeSum, Accuracy and other metrics.
  b. Graph - Bar Graphs showing statistics in a graphical way.
  c. Conclusion - Display final thoughts about the product according to the statistics.

Final goal - To classify the product according to customer review sentiments.
