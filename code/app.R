library(shiny)
library(ggplot2)

# Load the dataset
bodyfat_data = read.csv("BodyFat.csv")

remove_outliers = function(data) {
  data = data[data$BODYFAT >= 3 & data$BODYFAT <= 60, ]
  data = data[data$ADIPOSITY >= 15 & data$ADIPOSITY <= 40, ]
  data = data[data$ABDOMEN >= 60 & data$ABDOMEN <= 140, ]
  
  return(data)
}

bodyfat_data = remove_outliers(bodyfat_data)

# Build the linear regression model using ADIPOSITY, ABDOMEN, and AGE to predict BODYFAT
complex_model = lm(BODYFAT ~ ADIPOSITY + ABDOMEN + AGE, data = bodyfat_data)

# Build a simpler model using just HEIGHT and WEIGHT if ADIPOSITY is not provided
simple_model = lm(BODYFAT ~ WEIGHT + HEIGHT, data = bodyfat_data)

# Function to calculate Adiposity
calculate_adiposity = function(weight, height) {
  adiposity = (weight / (height^2)) * 703 # Use the formula: weight / height^2 * 703 (to adjust for units)
  return(adiposity)
}

# Define UI for the app
ui = fluidPage(
  titlePanel("Body Fat Prediction App"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("height", "Enter your height (in inches):", value = 70),
      numericInput("weight", "Enter your weight (in pounds):", value = 150),
      numericInput("abdomen", "Enter your abdomen circumference (in cm) (optional):", value = NA),
      numericInput("adiposity", "Enter your adiposity (optional):", value = NA),
      numericInput("age", "Enter your age:", value = 25),
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
      textOutput("warning_message"), # Warning message if body fat is abnormal
      hr(),
      plotOutput("residual_plot"),
      plotOutput("scatter_plot"),
      plotOutput("distribution_plot")
    )
  )
)

# Define server logic for the app
server = function(input, output, session) {
  # To store if user confirmed the abnormal body fat
  user_confirmed = reactiveVal(FALSE)
  
  observeEvent(input$predict, {
    # Clear any previous warning messages
    output$warning_message = renderText({ "" })
    
    # Validate inputs with proper boundaries
    if (input$height < 48 || input$height > 84) {
      output$warning_message = renderText({
        "Height should be between 48 and 84 inches."
      })
      return(NULL)
    }
    
    if (input$weight < 80 || input$weight > 500) {
      output$warning_message = renderText({
        "Weight should be between 80 and 500 pounds."
      })
      return(NULL)
    }
    
    if (!is.na(input$abdomen) && (input$abdomen < 60 || input$abdomen > 140)) {
      output$warning_message = renderText({
        "Abdomen circumference should be between 60 and 140 cm."
      })
      return(NULL)
    }
    
    if (!is.na(input$adiposity) && (input$adiposity < 15 || input$adiposity > 40)) {
      output$warning_message = renderText({
        "Adiposity should be between 15 and 40."
      })
      return(NULL)
    }
    
    
    # If adiposity is not provided, calculate it from height and weight
    adiposity_value <- ifelse(is.na(input$adiposity), calculate_adiposity(input$weight, input$height), input$adiposity)
    
    # Ensure abdomen is numeric and handle missing values
    abdomen_value <- ifelse(is.na(input$abdomen), NA_real_, as.numeric(input$abdomen))
    
    # Predict body fat using the appropriate model
    if (!is.na(adiposity_value) && !is.na(abdomen_value)) {
      # Use complex model when adiposity and abdomen are provided
      new_data <- data.frame(ADIPOSITY = adiposity_value, ABDOMEN = abdomen_value, AGE = input$age)
      predicted_bodyfat <- predict(complex_model, new_data)
    } else {
      # Use simple model when only weight and height are available
      new_data <- data.frame(WEIGHT = input$weight, HEIGHT = input$height)
      predicted_bodyfat <- predict(simple_model, new_data)
    }
    
    # Check if the predicted body fat is within normal human range
    if (predicted_bodyfat < 3 || predicted_bodyfat > 60) {
      if (!user_confirmed()) {
        output$warning_message = renderText({
          "The predicted body fat is abnormal. Please check your input. Do you confirm the results?"
        })
        user_confirmed(TRUE)  # Set confirmation flag to true for next click
      } else {
        # If the user confirms, display the result anyway
        output$prediction_output = renderText({
          round(predicted_bodyfat, 2)
        })
        user_confirmed(FALSE)  # Reset confirmation flag
      }
    } else {
      # If body fat is within normal range, show the result
      output$prediction_output = renderText({
        round(predicted_bodyfat, 2)
      })
      user_confirmed(FALSE)  # Reset confirmation flag
    }
  })
  
  # Generate plots when the button is clicked
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

# Run the app
shinyApp(ui = ui, server = server)
