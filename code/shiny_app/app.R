library(shiny)
library(ggplot2)
library(reshape2)

bodyfat_data = read.csv("BodyFat_cleaned.csv")
bodyfat_data = subset(bodyfat_data, select = c(BODYFAT, ABDOMEN, WRIST, AGE))
model = lm(BODYFAT ~ ABDOMEN + WRIST + AGE, data = bodyfat_data)

ui = fluidPage(
  titlePanel("Body Fat Prediction App"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("waistline", "Enter your waistline (in cm):", value = NA),
      numericInput("wrist", "Enter your wrist circumference (in cm):", value = NA),
      numericInput("age", "Enter your age (years):", value = 20),
      actionButton("predict", "Predict Body Fat"),
      hr(),
      h4("Contact Information:"),
      p("For any inquiries, please contact app maintainer:"),
      p("Hengyu Yang: hyang644@wisc.edu")
    ),
    
    mainPanel(
      h3("Predicted Body Fat (%):"),
      textOutput("prediction_output"),
      textOutput("warning_message"),
      hr(),
      img(src = "body_fat_chart.jpg", height = 320, width = 600),
      img(src = "wrist_measure.jpg", height = 300, width = 300),
      img(src = "waistline.jpg", height = 200, width = 600),
    )
  )
)


server = function(input, output, session) {
  user_confirmed = reactiveVal(FALSE)
  
  observeEvent(input$predict, {
    output$warning_message = renderText({ "" })
    output$prediction_output = renderText({ "" })
    
    if (is.na(input$abdomen) || is.na(input$wrist || is.na(input$age))) {
      output$warning_message = renderText({
        "All 3 fields are required. Please enter valid values."
      })
      return(NULL)
    }
    
    if (!is.na(input$age) && (input$age < 18 || input$age > 100)) {
      output$warning_message = renderText({
        "Age should be between 18 and 100."
      })
      return(NULL)
    }
    
    if (!is.na(input$abdomen) && (input$abdomen < 60 || input$abdomen > 140)) {
      output$warning_message = renderText({
        "Abdomen circumference should be between 60 and 140 cm."
      })
      return(NULL)
    }
    
    if (!is.na(input$wrist) && (input$wrist < 12 || input$wrist > 25)) {
      output$warning_message = renderText({
        "Wrist circumference should be between 12 and 25 cm."
      })
      return(NULL)
    }
    

    new_data <- data.frame(ABDOMEN = input$abdomen, WRIST = input$wrist, AGE = input$age)
    predicted_bodyfat <- predict(model, new_data)

    if (!is.na(predicted_bodyfat) && (predicted_bodyfat < 3 || predicted_bodyfat > 60)) {
      if (!user_confirmed()) {
        output$warning_message = renderText({
          "The predicted body fat is abnormal. Please check your input. Do you confirm the results?"
        })
        user_confirmed(TRUE)
      } else {
        output$prediction_output = renderText({
          round(predicted_bodyfat, 2)
        })
        user_confirmed(FALSE)
      }
    } else {
      output$prediction_output = renderText({
        round(predicted_bodyfat, 2)
      })
      user_confirmed(FALSE)
    }
  })
}

shinyApp(ui = ui, server = server)
