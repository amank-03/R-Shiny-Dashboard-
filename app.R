
# Loading library and data

library(shiny)
library(shinydashboard)

cr_data = read.csv("german_credit_data.csv", header = T)

cat_var = c("Sex", "Job", "Housing", "Saving A/C", "Checking A/C","Purpose")

num_val = c("Age" , "Credit Amount", "Duration")
# Define UI dashboard 
ui <- dashboardPage(
  dashboardHeader(title="Assignment 2"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Problem 1",tabName = "p1"),
      menuItem("Problem 2",tabName = "p2")
    )
  ),
  dashboardBody(
    tabItems(
      
      #UI for tab 1
      
      tabItem("p1",
              tabsetPanel(
                # UI of Panel 1
                tabPanel("Gamma", h2("CLT simulator"),
                         fluidPage(
                         numericInput("sim_size", 
                                      label = "Enter Simulation Size",
                                      value = 1000),
                         numericInput("sam_size",
                                      label = "Enter Sample Size", 
                                      value = 30),
                         
                         numericInput("mean", label = "Enter mean value",
                                      value = 20),
                         numericInput("sd", label = "Enter SD",
                                      value = 5),
                         actionButton("go" , label = "View Results"),
                         plotOutput("plot"),
                         plotOutput("QQplot"),
                         verbatimTextOutput("test")
                         )
                         ),
                # UI of Panel 2
                tabPanel("Uniform", h2("CLT simulator"),
                         fluidPage(
                           numericInput("sim_size2", 
                                        label = "Enter Simulation Size",
                                        value = 1000),
                           numericInput("sam_size2",
                                        label = "Enter Sample Size", 
                                        value = 30),
                           
                           numericInput("mean2", label = "Enter mean value",
                                        value = 20),
                           numericInput("sd2", label = "Enter SD",
                                        value = 5),
                           actionButton("go2" , label = "View Results"),
                           plotOutput("plot2"),
                           plotOutput("QQplot2"),
                           verbatimTextOutput("test2")
                         )
                         
                         )
                          )
              ),
      
      # UI for 2nd tab
      tabItem("p2",
              tabsetPanel(
                
                # UI for part 1
                
                tabPanel("Part 1",h2("German Credit Data"),
                         selectInput("choice" ,
                                     label = "Please select the varibale",
                                     choices = cat_var),
                         actionButton("go_2_1", label = "Show"),
                         plotOutput("pie"),
                         plotOutput("bar")),
                
                # UI for part 2
                
                tabPanel("Part 2",h2("German Credit Data"),
                         selectInput("choice_num" ,
                                     label = "Select Variable", choices = num_val),
                         selectInput("choice_cat",label = "Select Variable", choices = cat_var),
                         actionButton("go_2_2" , label = "Show"),
                         plotOutput("box")
                         )
              ))
    )
  )
)
  
  
  
  
  
server <- function(input, output, session) {

# server for tab 1
  
## variable setting using reactive  
  
  data <- reactive({replicate(input$sim_size , 
                    mean(rgamma(input$sam_size , 
                                shape = input$sd^2/input$mean, 
                                scale = input$mean^2/input$sd^2)))})
  
  data2 <- reactive({replicate(input$sim_size2 , 
                              mean(runif(input$sam_size2 , 
                                          min = -input$sd2*3 + input$mean2, 
                                          max = input$mean2 +input$sd2*3)))})
## observeEvent for paenl 1 question 1   
  observeEvent(input$go, {output$plot <- 
    renderPlot(hist(isolate(data())
      , xlab = "Sample Mean",main = "Histogram of Sample Mean")) 
  })
 observeEvent(input$go ,  {output$QQplot <- renderPlot({qqnorm(isolate(data())) 
                                                    qqline(isolate(data()), lwd = 2)}
                                                     )})
 observeEvent(input$go ,  {output$test <- renderPrint({
   ks.test(isolate(data()), pnorm, mean = mean(isolate(data())), 
           sd = sd(isolate(data())))
 })})

# observeEvent for paenl 2 question 1                                                                
 observeEvent(input$go2, {output$plot2 <- 
   renderPlot(hist(isolate(data2())
   , xlab  = "Sample Mean", main = "Histogram of Sample Mean"))
 })
 observeEvent(input$go2 ,  {output$QQplot2 <- renderPlot({qqnorm(isolate(data2())) 
   qqline(isolate(data2()), lwd = 2)}
 )})
 observeEvent(input$go2 ,  {output$test2 <- renderPrint({
   ks.test(isolate(data2()), pnorm, mean = mean(isolate(data2())), 
           sd = sd(isolate(data2())))
 })})
 
# Server part of Problem 2
 ##Part 1
 
 x <- reactive({
   if (input$choice == "Sex"){
     cr_data[,3]
   }
   else if (input$choice == "Job"){
     cr_data[,4]
   }
   else if (input$choice == "Housing"){
     cr_data[,5]
   }
   else if (input$choice == "Saving A/C"){
     cr_data[,6]
   }
   else if (input$choice == "Checking A/C"){
     cr_data[,7]
   }
   else if (input$choice == "Purpose"){
     cr_data[,10]
   }
 })
 
 observeEvent(input$go_2_1, output$pie <- renderPlot({
   mytab = table(isolate(x()))
   
   pie(mytab, main = paste("Pie Chart for ",isolate(input$choice)))}))
 
 observeEvent(input$go_2_1, output$bar <- renderPlot({
   mytab2 = prop.table(table(isolate(x()))) 
   barplot(mytab2, xlab = isolate(input$choice), ylab = "Percentage",
           main = paste("Bar Plot for ", isolate(input$choice)))
 }))
 
 ## Part 2
 
 cat <- reactive({
   if (input$choice_cat == "Sex"){
     cr_data[,3]
   }
   else if (input$choice_cat == "Job"){
     cr_data[,4]
   }
   else if (input$choice_cat == "Housing"){
     cr_data[,5]
   }
   else if (input$choice_cat == "Saving A/C"){
     cr_data[,6]
   }
   else if (input$choice_cat == "Checking A/C"){
     cr_data[,7]
   }
   else if (input$choice_cat == "Purpose"){
     cr_data[,10]
   }
 })
 
 num <- reactive({
   if (input$choice_num == "Age"){
     cr_data[,2]
   }
   else if (input$choice_num == "Credit Amount"){
     cr_data[,8]
   }
   else if (input$choice_num == "Duration"){
     cr_data[,9]
   }
 })
 observeEvent(input$go_2_2, output$box <- 
                renderPlot({
                  
                  boxplot(isolate(num())~isolate(cat()), 
                          xlab = isolate(input$choice_cat), 
                          ylab = isolate(input$choice_num),
                          main = paste("Box Plot on ",
                                       isolate(input$choice_num), "~" , 
                                       isolate(input$choice_cat)))
                }))
}
   
# Run the application 
shinyApp(ui = ui, server = server)
