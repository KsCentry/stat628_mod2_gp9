library(shiny)
library(ggplot2)

bodyfat_data = read.csv("../data/BodyFat.csv")

remove_outliers = function(data) {
  data = data[data$BODYFAT >= 3 & data$BODYFAT <= 60, ]
  data = data[data$ADIPOSITY >= 15 & data$ADIPOSITY <= 40, ]
  data = data[data$ABDOMEN >= 60 & data$ABDOMEN <= 140, ]
  
  return(data)
}

bodyfat_data = remove_outliers(bodyfat_data)

complex_model = lm(BODYFAT ~ ADIPOSITY + ABDOMEN + AGE, data = bodyfat_data)
simple_model = lm(BODYFAT ~ WEIGHT + HEIGHT, data = bodyfat_data)

calculate_adiposity = function(weight, height) {
  adiposity = (weight / (height^2)) * 703 # Use the formula: weight / height^2 * 703 (to adjust for units)
  return(adiposity)
}

ui = fluidPage(
  titlePanel("Body Fat Prediction App"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("height", "Enter your height (in inches):", value = 70),
      numericInput("weight", "Enter your weight (in pounds):", value = 150),
      numericInput("abdomen", "Enter your abdomen circumference (in cm) (optional):", value = NA),
      numericInput("adiposity", "Enter your adiposity/BMI (optional):", value = NA),
      numericInput("age", "Enter your age (optional), default 20:", value = 20),
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
    # Clear any previous warning messages and prediction output
    output$warning_message = renderText({ "" })
    output$prediction_output = renderText({ "" })  # Clear previous prediction result
    
    # Check if the required variables (height and weight) are provided and valid
    if (is.na(input$height) || is.na(input$weight)) {
      output$warning_message = renderText({
        "Both height and weight are required fields. Please enter valid values."
      })
      return(NULL)
    }
    
    # Validate height and weight ranges
    if (!is.na(input$height) && (input$height < 48 || input$height > 84)) {
      output$warning_message = renderText({
        "Height should be between 48 and 84 inches."
      })
      return(NULL)
    }
    
    if (!is.na(input$weight) && (input$weight < 80 || input$weight > 500)) {
      output$warning_message = renderText({
        "Weight should be between 80 and 500 pounds."
      })
      return(NULL)
    }
    
    # Check optional variables (age, abdomen) for valid ranges
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
    
    # Ensure numeric NA values for age and abdomen
    age_value <- ifelse(is.na(input$age), NA_real_, as.numeric(input$age))
    abdomen_value <- ifelse(is.na(input$abdomen), NA_real_, as.numeric(input$abdomen))
    adiposity_value <- ifelse(is.na(input$adiposity), calculate_adiposity(input$weight, input$height), input$adiposity)
    
    # Check if calculated or provided adiposity (BMI) is within a reasonable range
    if (!is.na(adiposity_value) && (adiposity_value < 15 || adiposity_value > 40)) {
      output$warning_message = renderText({
        paste("The calculated or provided BMI (", round(adiposity_value, 2), ") does not fall within the normal range (15 - 40). Please check your inputs.")
      })
      return(NULL)
    }
    
    # Predict body fat using the appropriate model
    if (!is.na(adiposity_value) && !is.na(abdomen_value) && !is.na(age_value)) {
      # Use complex model when adiposity and abdomen are provided
      new_data <- data.frame(ADIPOSITY = adiposity_value, ABDOMEN = abdomen_value, AGE = age_value)
      predicted_bodyfat <- predict(complex_model, new_data)
    } else {
      # Use simple model when only weight and height are available
      new_data <- data.frame(WEIGHT = input$weight, HEIGHT = input$height)
      predicted_bodyfat <- predict(simple_model, new_data)
    }
    
    # Check if the predicted body fat is within normal human range
    if (!is.na(predicted_bodyfat) && (predicted_bodyfat < 3 || predicted_bodyfat > 60)) {
      if (!user_confirmed()) {
        output$warning_message = renderText({
          "The predicted body fat is abnormal. Please check your input. Do you confirm the results?"
        })
        user_confirmed(TRUE)  # Set confirmation flag to true for next click
      } else {
        output$prediction_output = renderText({
          round(predicted_bodyfat, 2)
        })
        user_confirmed(FALSE)  # Reset confirmation flag
      }
    } else {
      output$prediction_output = renderText({
        round(predicted_bodyfat, 2)
      })
      user_confirmed(FALSE)  # Reset confirmation flag
    }
  })
  
  observeEvent(input$showPlots, {
    
    # Residual plot
    output$residual_plot = renderPlot({
      residuals = complex_model$residuals
      ggplot(bodyfat_data, aes(x = predict(complex_model), y = residuals)) +
        geom_point() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(title = "Residual Plot", x = "Predicted Values", y = "Residuals") +
        theme_minimal()
    })
    
    # Scatter plot of actual vs predicted body fat
    output$scatter_plot = renderPlot({
      predicted_values = predict(complex_model, bodyfat_data)
      ggplot(bodyfat_data, aes(x = BODYFAT, y = predicted_values)) +
        geom_point(color = "blue") +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
        labs(title = "Actual vs Predicted Body Fat", x = "Actual Body Fat (%)", y = "Predicted Body Fat (%)") +
        theme_minimal()
    })
    
    # Distribution plot of actual vs predicted body fat
    output$distribution_plot = renderPlot({
      predicted_values = predict(complex_model, bodyfat_data)
      ggplot() +
        geom_histogram(aes(x = bodyfat_data$BODYFAT), fill = "blue", alpha = 0.5, bins = 30) +
        geom_histogram(aes(x = predicted_values), fill = "green", alpha = 0.5, bins = 30) +
        labs(title = "Distribution of Actual vs Predicted Body Fat", x = "Body Fat (%)", y = "Count") +
        theme_minimal()
    })
  })
}

shinyApp(ui = ui, server = server)
