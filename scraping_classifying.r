# Disable Warnings (For Ratings Not Present)
options(warn=-1)

# Install Packages
if(!"rvest" %in% installed.packages()[,"Package"])install.packages("rvest")
#if(!"sentimentr" %in% installed.packages()[,"Package"])install.packages("sentimentr")
#if(!"dplyr" %in% installed.packages()[,"Package"])install.packages("dplyr")
#if(!"textclean" %in% installed.packages()[,"Package"])install.packages("textclean")
#if(!"RTextTools" %in% installed.packages()[,"Package"])install.packages("RTextTools")

# Load Packages
library(rvest)
library(sentimentr)
library(dplyr)
library(RTextTools)

# Variables
reviews_flag<- 0
args<- commandArgs(TRUE)
actual<- c()
predicted<- c()
x<- 0
y<- 0
file_name<- c(args[2])
file_name_table<- c(args[3]) 
counter<- 0
acc_counter<- 0
pages<- 5
acc_map<- list()

# Source
src_array<- c(args[1])
#src_array<- c("https://www.amazon.com/Raspberry-Pi-Desktop-Starter-White/product-reviews/B01CI58722/ref=cm_cr_arp_d_paging_btm_1?ie=UTF8&reviewerType=all_reviews&pageNumber=1")

# Code - - - - 

acc_map[["positive"]]<- 0
acc_map[["neutral"]]<- 0
acc_map[["negative"]]<- 0

if(file.exists(file_name)){
  #file.remove(file_name)
  write("",file_name,append=FALSE)
}

firstLink<- src_array[1]
for(i in 2:pages){
  
  temp<- ""
  temp<- sub("cm_cr_getr_d_paging_btm_1", paste("cm_cr_getr_d_paging_btm_", i, sep=""), firstLink)
  temp<- sub("&pageNumber=1", paste("&pageNumber=", i, sep=""), temp)
  
  print(temp)
  src_array<- c(src_array, temp)
}

# For Number Of Sources in Array
for(k in 1:length(src_array)){
  
    actual_reviews_counter<- 0
    accuracy_counter<- 0
    source<- src_array[k]
    download.file(source, destfile = "scrapedpage.html", quiet=TRUE)
    get_pages<- read_html("scrapedpage.html")
    
    
    # Get Reviews
    reviews<- get_pages %>% html_nodes(".review")
    get_number<- length(reviews)
    
    # Check If No Reviews
    if(length(reviews)==0){
      print("No Reviews or Access Error")
      reviews_flag<- 1
      break()
    }
    
    # For Number Of Reviews 
    for(i in 1:get_number){
      get_total_data<- reviews[i] %>% html_nodes(".a-row")
      
      # Check If Ratings Are Present On A Node
      node_for_rating<- 1
      check_valid<- get_total_data[node_for_rating] %>% html_node(".a-link-normal")
      if(is.na(check_valid)){
        node_for_rating<- 2
      }
      
      # Get Stars
      stars_title<- get_total_data[node_for_rating] %>% html_node(".a-link-normal") %>% html_attr("title")
      stars_string<-substring(stars_title,1,3)
      stars<-as.double(stars_string)
      if (is.na(stars))
        next()
      
      
      
      # Get Review Text
      review_data<- get_total_data[4]  %>% html_node("span") %>% html_text(trim=TRUE)
      check_read_now<- substring(review_data,nchar(review_data)-9+1,nchar(review_data))
      if (check_read_now == "Read more"){
        review_data<- substring(review_data,1,nchar(review_data)-9)
      }
      
      # Run ngram on Reviews
      sample_set<- get_sentences(review_data)
      sentiment(sample_set) %>%
        subset(select = "sentiment") %>%
        colSums() -> result
      
      actual_reviews_counter<- actual_reviews_counter + 1
      
      # *100 to identify neutral sentiments.
      result<- as.integer(result*100)
      ngram_result<- ""
      
      if(result==0){
        ngram_result<- "neutral"
      }else if(result > 0){
        ngram_result<- "positive"
      }else
        ngram_result<- "negative"
      
      
      # Add To Corresponding Files
      remark<- ""
      if(stars<2.5){
        remark<- "negative"
      } else if(stars>3.5) {
        remark<- "positive"
      } else {
        remark<- "neutral"
      }
      
      actual<- c(actual,remark)
      predicted<- c(predicted,ngram_result)
      
      if(remark == ngram_result){
        accuracy_counter<- accuracy_counter + 1
        acc_map[[remark]]<- acc_map[[remark]] + 1
      }
      write(review_data,file_name,append=TRUE,sep='')
      write(paste("<---> Actual - ",remark," , Expected - ",ngram_result,"<--->\n\n"),file_name,append=TRUE,sep='')
    }
    
    #Find Out Accuracy
    accuracy<- (accuracy_counter*100)/actual_reviews_counter
    print(accuracy)
    
    x=table(predicted, actual)
    y=recall_accuracy(predicted,actual)
  
}

max<- 0
maxRemark<- ""
print(acc_map)
for(acc in names(acc_map)){
  if(acc_map[[acc]] > max){
    max<- acc_map[[acc]]
    maxRemark<- acc
  }
}

print(maxRemark)

if(y*100 < 50){
  if(maxRemark == "positive"){
    maxRemark<- "Negative"
  }
  else if(maxRemark == "negative"){
    maxRemark<- "Positive"
  }
  else{
    maxRemark<- "No Conclusion as Accuracy is LOW"
  }
}

maxRemark<- toupper(maxRemark)

out = file(file_name_table, 'w')
capture.output( print(x, print.gap=3), file=out)
write(paste('\n\t\t Accuracy%: ', y*100,sep=''), file=out,append=TRUE)
write(paste('\n\t\t Conclusion: Product has generally ', maxRemark," reviews.",sep=''), file=out,append=TRUE)

close(out)  

print("Completed Succesfully")
# End Code - - - - - -

# Enable Warnings Again
options(warn=0)
