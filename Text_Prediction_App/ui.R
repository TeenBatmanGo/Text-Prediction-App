
library(shiny)

shinyUI(pageWithSidebar(
        
        headerPanel(strong("Text Prediction")),
  
        sidebarPanel(
                helpText("Enter your text below and hit the Update button, this app will predict the next word of your text and output the top five next-words.", 
                         "Below you are free to choose a backoff method to use, stupid backoff or katz backoff.",
                         "At the same time you can control the number of predictions this app displays."),
                textInput("text",
                        h4(strong("Please enter your text: ")),
                        value = "Welcome"),
                br(),
                selectInput("method", 
                            label = "Choose a backoff method",
                            choices = list("Stupid Backoff", "Katz Backoff"),
                            selected = "Katz Backoff"),
                br(),
                sliderInput("number","Number of predictions", 1, 10, value = 5, animate = TRUE),
                submitButton("Update", icon("refresh"))
        ),
    

        mainPanel(
                tabsetPanel(
                        tabPanel("Prediction",
                                 h3("Text Predictions and Probabilities"),
                                 verbatimTextOutput("predict"),
                                 br(),
                                 p("The method you chose is ", span(textOutput("text2", inline = TRUE), style = "color:blue;font-weight:bold"), "."),
                                 p("Prediction: ", textOutput("text3", inline = TRUE), span(textOutput("text4", inline = TRUE), style = "color:blue;font-weight:bold"))
                                 )
                )
                )
))
