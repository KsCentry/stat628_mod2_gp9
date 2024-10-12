library(shiny)
library(ggplot2)

bodyfat_data = read.csv("BodyFat_cleaned.csv")

model = lm(BODYFAT ~ ABDOMEN + WRIST + AGE, data = bodyfat_data)

ui = fluidPage(
  titlePanel("Body Fat Prediction App"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("abdomen", "Enter your abdomen circumference (in cm):", value = NA),
      numericInput("wrist", "Enter your wrist circumference (in cm):", value = NA),
      numericInput("age", "Enter your age (years):", value = 20),
      actionButton("predict", "Predict Body Fat"),
      hr(),
      h4("Model Statistics and Plots"),
      actionButton("showPlots", "Show Model Plots"),
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
      plotOutput("residual_plot"),
      plotOutput("scatter_plot"),
      plotOutput("distribution_plot")
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
  
  observeEvent(input$showPlots, {
    
    output$residual_plot = renderPlot({
      residuals = model$residuals
      ggplot(bodyfat_data, aes(x = predict(model), y = residuals)) +
        geom_point() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(title = "Residual Plot", x = "Predicted Values", y = "Residuals") +
        theme_minimal()
    })
    
    output$scatter_plot = renderPlot({
      predicted_values = predict(model, bodyfat_data)
      ggplot(bodyfat_data, aes(x = BODYFAT, y = predicted_values)) +
        geom_point(color = "blue") +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
        labs(title = "Actual vs Predicted Body Fat", x = "Actual Body Fat (%)", y = "Predicted Body Fat (%)") +
        theme_minimal()
    })
    
    # Distribution plot of actual vs predicted body fat
    output$distribution_plot = renderPlot({
      predicted_values = predict(model, bodyfat_data)
      ggplot() +
        geom_histogram(aes(x = bodyfat_data$BODYFAT), fill = "blue", alpha = 0.5, bins = 30) +
        geom_histogram(aes(x = predicted_values), fill = "green", alpha = 0.5, bins = 30) +
        labs(title = "Distribution of Actual vs Predicted Body Fat", x = "Body Fat (%)", y = "Count") +
        theme_minimal()
    })
  })
}

shinyApp(ui = ui, server = server)
