library(shiny)
library(tidyverse)



#Cleaned up 
CleanEmployeeData$OverTimeScale = ifelse(CleanEmployeeData$OverTime =="Yes", 1,0) 
CleanEmployeeData$MonthlyIncomeScale = scale(CleanEmployeeData$MonthlyIncome)
CleanEmployeeData$AgeScale = scale(CleanEmployeeData$Age)
CleanEmployeeData$EnvironmentSatisfactionScale = scale(CleanEmployeeData$EnvironmentSatisfaction)
CleanEmployeeData$OverTimeScale = scale(CleanEmployeeData$OverTimeScale)
CleanEmployeeData$YearsAtCompanyScale = scale(CleanEmployeeData$YearsAtCompany)



CaseStudy2_data = read_csv("CaseStudy2-data (1).csv")
dataset = CaseStudy2_data

colors = c('olivedrab3','gold','darkred')

ui = fluidPage(
  
  # app title
  titlePanel("Case Study 2: Attrition Plots Against Predictor Variables"),
  
  # sidebar layout with input/output definitions
  sidebarLayout(
    
    # side bar panel for graphic parameters
    sidebarPanel(
      
      # selectInput for choosing variables
      selectInput(
        inputId = "data",
        label = "Predictor Variables",
        choices = list(
          'MonthlyIncome',
          'JobLevel',
          'OverTime',
          'Age',
          'HourlyRate',
          'JobInvolvement'
        )
      ),
      selectInput(
        inputId = "groups",
        label = "Display by Groups",
        choices = list(
          'Attrition',
          'Gender',
          'BusinessTravel',
          'MaritalStatus'
        )
      ),
 
      
      ## Attrition Count Plot
      plotOutput(
        outputId = "aplot"
      ),
    ),
    
    # main panel for displaying plot
    mainPanel(
      
      
      # histogram outputm pplot is percentage plot
      plotOutput(
        outputId = "histplot"
      ),
      plotOutput(
        outputId = "pplot"
      )
      
    )
    
  ),
  
)

# server function for creating app

server  = function(input,output){
  
  # renderPlot function is used to map the histogram inputs to main panel outputs
  # this plot is "reactive," i.e. changes when inputs (e.g. bins) are altered
 
  #Histogram Plot
   output$histplot = renderPlot({
    dataset |> ggplot(aes_string(x = input$data,fill=input$groups))+
      geom_histogram(stats="identity")+
      xlab(input$data)+
      scale_fill_manual(values=as.vector(colors))+
      ggtitle(paste("Histogram of",
                    input$data,
                    "faceted by",
                    input$groups,
                    sep=" "))
  })
  
  #Percentage Plot
  output$pplot = renderPlot({
    # creating histogram for output
    dataset |> ggplot(aes_string(x = input$data,fill=input$groups))+
      geom_bar(position="fill")+
      xlab(input$data)+
      scale_y_continuous(labels = scales::percent)+
      scale_fill_manual(values=as.vector(colors))+
      ggtitle(paste("Histogram",
                    input$data,
                    "in Percentages by",
                    input$groups,
                    sep=" "))
  })
  
  #Attrition Count Plot
  output$aplot=renderPlot({
    dataset %>% ggplot(aes(x=Attrition,fill=Attrition))+ geom_bar()+
      ggtitle("Attrition Count") +
      scale_fill_manual(values=as.vector(colors))+
      xlab("Attrition")+ylab("Count")
  })
  
  #Naive Bayes
  output$nb=renderPrint({
    model = CleanEmployeeData %>% select(c("MonthlyIncome","JobRole","Age","OverTime","EnvironmentSatisfaction","DailyRate","JobLevel","TotalWorkingYears","JobInvolvement","Attrition"))
    
    trainIndices = sample(1:dim(model)[1],round(.70 * dim(model)[1]))
    train = model[trainIndices,]
    test = model[-trainIndices,]
    
    set.seed(7)
    classifier1 = naiveBayes(model[,c(1,4,7)],model$Attrition)
    pred = predict(classifier1,newdata=test)
    CM = confusionMatrix(table(test$Attrition,pred))
    
    
    CM
  })
  
}

shinyApp(ui = ui, server = server)