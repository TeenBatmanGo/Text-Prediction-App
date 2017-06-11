
library(shiny)
library(knitr)

source("word_predict.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output){
        
        dat <- reactive({
                method <- switch(input$method, 
                       "Stupid Backoff" = "sbo",
                       "Katz Backoff" = "kbo")
                k <- input$number
                text_predict(input$text, method, k)
        })
        
        output$predict <- renderPrint({
                data.frame(dat())
        })
        output$text2 <- renderText({
                input$method
        })
        output$text3 <- renderText({
                input$text
        })
        output$text4 <- renderText({
                dat()$ngram[1]
        })
})