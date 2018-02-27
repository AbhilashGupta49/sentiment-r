# Install Libraries
if(!"shiny" %in% installed.packages()[,"Package"])install.packages("shiny")
if(!"xml2" %in% installed.packages()[,"Package"])install.packages("xml2")

# Include Libraries
library(shiny)
library(xml2)

file_name<- "test_data.txt"
file_name_table<- "test_data_table.txt"

# UI Code
ui<- pageWithSidebar(
  
  # Header
  headerPanel("Sentiment Analysis"),
  
  # Sidebar
  sidebarPanel(
    # Input
    textInput("get_url","Paste Product URL - ","https://www.example.com"),
    
    # Submit Button
    actionButton("run_func","Go!"),
    hr()
    
  ),
  
  
  # Main Panel
  mainPanel(
    tabsetPanel(
      tabPanel("Reviews", h3(id="loading","Click on Go! and Wait",align="center"),
               br(),
               verbatimTextOutput("view")),
      tabPanel("Table", h3(id="loading","Click on Go! and Wait",align="center"),
               br(),
               verbatimTextOutput("table"))
    )
    
  )
  
)
# Server Code
server<- function(input,output){
  
  
  
  # On Button Click
  get_output<- eventReactive(input$run_func, {
    
    system(paste("Rscript scraping_classifying.r",input$get_url,file_name,file_name_table))
    
    # Render Table
    output$table<- renderText({
      
      removeUI(
        selector = "#loading"
      )
      string_final<- ""
      
      # Display Text File
      if(file.exists(file_name_table)){
        data<- readLines(file_name_table)
        
        for(i in 1:length(data)){
          string_final<- paste(string_final,data[i],sep="\n")
        }
      }
      else{
        string_final<- "No File for Table"
      }
      
      string_final
      
    })
    
  })
  
  
  
  
  # Render View
  output$view<- renderText({
    
    h3(id="loading","Click on Go! and Wait",align="center")
    br()
    
    get_output()
    
    removeUI(
      selector = "#loading"
    )
    
    string_final<- ""
    
    # Display Text File
    if(file.exists(file_name)){
      data<- readLines(file_name)
      for(i in 1:length(data)){
        string_final<- paste(string_final,data[i],sep="\n")
      }
    }
    else{
      string_final<- "No File for Reviews"
    }
    
    
    string_final
    
  })
  
  
}

# Run Application 
shinyApp(ui = ui, server = server)
