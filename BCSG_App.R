##Alex Kikos##
##BCSG Assignment 2022##

rm(list=ls())

#loads in necessary libraries
library(shiny)
library(dplyr)
library(readxl)
library(tidyverse)
library(DT)
library(lubridate)
library(plotly)
library(caret)
library(gains)
library(rpart)
library(rpart.plot)
library(pROC)
library(forecast)



data <- read.csv("data.csv")
athletes <- unique(data$athletes)
tests <- unique(data$assessment)
metrics <- unique(data$metric)
positions <- unique(data$position)

#applies percentile rank for each unique metric group
data <- data %>% group_by(metric) %>%
  mutate(percentile  = round(100*(rank(value)/length(value)))
         )
#provides rating for each quartile rank
data <- data %>%
  mutate(rating = case_when(percentile >= 75 ~ 'Excellent',
                            percentile < 75 & percentile >= 50 ~ 'Good',
                            percentile < 50 & percentile >= 25 ~ 'Average',
                            percentile < 25 ~ 'Poor')
  )
data <- as.data.frame(data)
#converts char dates to workable date values
data$date <- mdy(data$date)

#From Q2 on assignment, finds duplicate observations (trials) and uses the average value from test trial
data <- data %>% group_by(athletes,date,assessment,metric) %>%
  mutate(value = mean(value))
data <- unique(data)
data <- as.data.frame(data)


### QUESTION 3 REGRESSION TREE MODEL EXAMPLE ###
### QUESTION 3 REGRESSION TREE MODEL EXAMPLE ###
### QUESTION 3 REGRESSION TREE MODEL EXAMPLE ###

#modified csv file using Metric values as new column values (transposing) for simpler regression tree use
regData <- read.csv("data0.csv")
regData <- regData[,c(2:3,5:23)]
regData <- regData %>% relocate(`X0.10y`)
regData <- (regData[complete.cases(regData), ])


#factor all character values first then turn into numerics for regression tree (unclass)
regData[sapply(regData, is.character)] <- lapply(regData[sapply(regData, is.character)],
                                                 as.factor)
regData[sapply(regData, is.factor)] <- lapply(regData[sapply(regData, is.factor)],
                                              unclass)


set.seed(1)
myIndex <- createDataPartition(regData$`X0.10y`, p=0.7, list=FALSE)
trainSet <- regData[myIndex,]
validationSet <- regData[-myIndex,]

##FULL TREE
set.seed(1)
full_tree <- rpart(`X0.10y` ~ ., data = trainSet, method = "anova", cp = 0, minsplit = 2, minbucket = 1)
prp(full_tree, type = 1, extra = 1, under = TRUE)
printcp(full_tree)

#uses num 16 for min xerror > 2.2584e-05 for cp value
pruned_tree <- prune(full_tree, cp = 2.2584e-05) 
prp(pruned_tree, type = 1, extra = 1, under = TRUE)

##now predict the average balances of the observations in the validation set
predicted_value <- predict(pruned_tree, validationSet)
##PERFORMANCE EVALUATION
accuracy(predicted_value, validationSet$`X0.10y`)
#                 ME       RMSE         MAE       MPE     MAPE
# Test set 0.0003636733 0.01536601 0.003690188 0.1195302 2.342983




## START OF THE UI ##
ui <- navbarPage(
  
  "BCSG APP",## TAB 1 ##
  tabPanel("TAB 1",
           sidebarLayout(
             # Sidebar with a slider and selection inputs
             sidebarPanel(
               width = 2,
               selectInput("ath_sel1",
                           "Select an Athlete:",
                           athletes)
             ), #SIDEBAR PANEL END
             
             # Show Plots
             mainPanel(
               h4("Athlete's Most Recent Test Scores"),
               #verbatimTextOutput("table1")
               DT::dataTableOutput('tab1table', height ="50vh", width = "80vw"),
             ) #MAIN PANEL
           ) 
           
  ),
  ## TAB 2 ##
  tabPanel("TAB 2",
           sidebarLayout(
             # Sidebar with a slider and selection inputs
             sidebarPanel(
               width = 2,
               selectInput("pos_sel2",
                           "Select a Position:",
                           choices = c("None", positions)),
               radioButtons("test_sel2", "Athlete Assessments:",
                            choices = ""),
               radioButtons("metric_sel2", "Metric Options:",
                            choices = "")
             ), #SIDEBAR PANEL END
             mainPanel(
               h4("Historical Test/Metric Values By Postion"),
               DT::dataTableOutput('tab2table', height ="50vh", width = "80vw"),
             ) #main end
           )
  ),
  ## TAB 3 ##
  tabPanel("TAB 3",
           sidebarLayout(
             # Sidebar with a slider and selection inputs
             sidebarPanel(
               width = 2,
               selectInput("ath_sel3",
                           "Select an Athlete:",
                           choices = c("None", athletes)),
               radioButtons("test_sel3", "Athlete Assessments:",
                            choices = ""),
               radioButtons("metric_sel3", "Metric Options:",
                            choices = "")
             ), #SIDEBAR PANEL END
             mainPanel(
               h4("Athlete Historical Test/Metric Values"),
               plotlyOutput(outputId = "plot", height="90vh", width = "80vw")
             ) #main end
           )
  )
  
)


#SERVER File
server <- function(input, output, session) {

  ############ TAB 1 ############
  ############ TAB 1 ############
  ############ TAB 1 ############
  
  #creates output table for selected athlete
  output$tab1table = DT::renderDataTable({
    
    ath1 <- input$ath_sel1
    data1 <- subset(data, athletes == ath1)
    tableTitle1 <- paste("Viewing ", ath1)
    tableTitle1 <- toupper(tableTitle1)
    DT::datatable(data1[,2:10],
                  caption = tableTitle1,
                  options = list(scrollY = '500px'))
    
  })
  
  
  ############ TAB 2 ############
  ############ TAB 2 ############
  ############ TAB 2 ############
  
  
  #updates the assessment radio button on click
  observeEvent(input$pos_sel2, {
    if(input$pos_sel2 == "None"){
      tagList(
        updateRadioButtons(session, "test_sel2",
                           choices = "NA"),
        updateRadioButtons(session, "metric_sel2",
                           choices = "NA")
      )
    }
    else{
      currPos2 <- input$pos_sel2
      data2 <- subset(data, position == currPos2)
      updateRadioButtons(session, "test_sel2",
                         label = "Athlete Assessments:",
                         choices = unique(data2$assessment)
      )
    }
  })
  
  
  #updates the metric radio button on click
  observeEvent(input$test_sel2, {
    if(input$test_sel2 == "" || input$test_sel2 == "NA"){
    }
    else{
      currPos2 <- input$pos_sel2
      currTest2 <- input$test_sel2
      data2 <- subset(data, positions == currPos2)
      data2 <- subset(data2, assessment == currTest2)
      updateRadioButtons(session, "metric_sel2",
                         label = "Metric Options:",
                         choices = unique(data2$metric)
      )
    }
  })
  
  #creates output table for selected position/test/metric
  output$tab2table = DT::renderDataTable({
    
    pos2 <- input$pos_sel2
    test2 <- input$test_sel2
    metric2 <- input$metric_sel2
    data2 <- subset(data, positions == pos2)
    data2 <- subset(data2, assessment == test2)
    data2 <- subset(data2, metric == metric2)
    tableTitle2 <- paste("Viewing ", pos2," ",test2," - ",metric2)
    tableTitle2 <- toupper(tableTitle2)
    DT::datatable(data2[,c(1:4,7,9:10)],
                  caption = tableTitle2,
                  options = list(scrollY = '500px'))
    
  })
  
  
  ############ TAB 3 ############
  ############ TAB 3 ############
  ############ TAB 3 ############
  
  #updates the assessment radio button on click
  observeEvent(input$ath_sel3, {
    if(input$ath_sel3 == "None"){
      tagList(
        updateRadioButtons(session, "test_sel3",
                           choices = "NA"),
        updateRadioButtons(session, "metric_sel3",
                           choices = "NA")
      )
    }
    else{
      currAth3 <- input$ath_sel3
      data3 <- subset(data, athletes == currAth3)
      updateRadioButtons(session, "test_sel3",
                         label = "Athlete Assessments:",
                         choices = unique(data3$assessment)
      )
    }
  })
  
  
  #updates the metric radio button on click
  observeEvent(input$test_sel3, {
    if(input$test_sel3 == "" || input$test_sel3 == "NA"){
    }
    else{
      currAth3 <- input$ath_sel3
      currTest3 <- input$test_sel3
      data3 <- subset(data, athletes == currAth3)
      data3 <- subset(data3, assessment == currTest3)
      updateRadioButtons(session, "metric_sel3",
                         label = "Metric Options:",
                         choices = unique(data3$metric)
      )
    }
  })
  
  #genrates plot on tab 3
  output$plot<-renderPlotly({
    
    ath4 <- input$ath_sel3
    test4 <- input$test_sel3
    metric4 <- input$metric_sel3
    data4 <- subset(data, athletes == ath4)
    data4 <- subset(data4, assessment == test4)
    data4 <- subset(data4, metric == metric4)
    graphTitle <- paste(ath4,":",metric4," HISTORY")
    graphTitle <- toupper(graphTitle)
    
    if(input$metric_sel3 == "" || input$metric_sel3 == "NA"){
    }
    else{
      currMean <- mean(data4$value)
      athPlot <-  plot_ly(data4, x=~date, y = currMean, name = 'Average Metric Score', type = 'scatter', mode = 'lines',
                          line = list(color = 'rgb(221,33,44)', width = 3))
      athPlot <- athPlot %>% add_trace(y = ~value, name = 'Metric Score', mode = 'lines+markers',
                                       line = list(color = 'rgb(11,30,93)', width = 3), 
                                       marker = list(color = 'rgb(221,33,44)', size = 10))
      athPlot <- athPlot %>%
        layout(
          title = graphTitle)
      
      athPlot
      
    }
  })
  
}

#executes both UI & Server files to run as one
shinyApp(ui, server)