# Load necessary libraries
install.packages("png")
library(shiny)
library(png)
library(httr)
library(jsonlite)
library(plotly)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Credit Scoring Prediction"),
  
  # Sidebar with inputs for each field in the payload
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "ID", label = "ID", value = 1),
      numericInput(inputId = "PAY_0", label = "PAY_0", value = 0),
      numericInput(inputId = "PAY_2", label = "PAY_2", value = 0),
      numericInput(inputId = "PAY_4", label = "PAY_4", value = 0),
      numericInput(inputId = "LIMIT_BAL", label = "LIMIT_BAL", value = 50000),
      numericInput(inputId = "PAY_3", label = "PAY_3", value = 0),
      numericInput(inputId = "BILL_AMT1", label = "BILL_AMT1", value = 2000),
      numericInput(inputId = "DEFAULT", label = "DEFAULT", value = 0),
      actionButton("predict", "Predict")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(id = "inTabset", type = "tabs",
                  tabPanel(title = "Prediction", value = "pnlPredict",
                           verbatimTextOutput("response"),
                           plotlyOutput("plot")
                  )
      )        
    )
  )
)

prediction <- function(inpFeat1, inpFeat2, inpFeat3, inpFeat4, inpFeat5) {
  # Convert user inputs to the required types
  inpFeat1 <- as.numeric(inpFeat1)
  inpFeat2 <- as.numeric(inpFeat2)
  inpFeat3 <- as.numeric(inpFeat3)
  inpFeat4 <- as.numeric(inpFeat4)
  inpFeat5 <- as.numeric(inpFeat5)
  
  # Constructing the payload with dynamic values from user inputs
  payload <- toJSON(list(
    data = list(
      list(
        ID = 1,  # Assign unique or sequential IDs as needed
        PAY_0 = inpFeat1,  # Map to appropriate user input
        PAY_2 = inpFeat2,
        PAY_4 = inpFeat3,
        LIMIT_BAL = inpFeat4,
        PAY_3 = inpFeat5,
        BILL_AMT1 = 5000,  # Set defaults or get more inputs as needed
        DEFAULT = 0  # Placeholder for example; adjust as necessary
      )
      # Additional entries can be added to this list if multiple rows are needed
    )
  ), auto_unbox = TRUE)
  
  # Print payload for debugging
  print("Constructed JSON Payload:")
  print(payload)
  
  # API endpoint and authentication
  url <- Sys.getenv("API_URL")
  username <- Sys.getenv("API_USERNAME")
  password <- Sys.getenv("API_PASSWORD")
  
  # Send the JSON payload to the API
  response <- POST(
    url,
    authenticate(username, password, type = "basic"),
    body = payload,
    encode = "json",
    add_headers(`Content-Type` = "application/json")
  )
  
  # Error handling
  if (http_type(response) != "application/json") {
    stop("API did not return json")
  }
  
  # Parse and return the response
  result <- content(response, as = "parsed")
  print("Response received:")
  print(result)
  
  return(result)
}


# Gauge plot function
gauge <- function(pos) {
  if (!is.finite(pos)) {
    stop("Error: Position must be a finite number")
  }
  
  breaks <- c(3, 7, 9, 10)  # Ensure the length of breaks covers the required ranges
  get.poly <- function(a, b, r1 = 0.5, r2 = 1.0) {
    if (!is.finite(a) || !is.finite(b)) {
      stop("Error: 'a' and 'b' must be finite numbers")
    }
    th.start <- pi * (1 - a / 10)
    th.end   <- pi * (1 - b / 10)
    th       <- seq(th.start, th.end, length = 10)
    x        <- c(r1 * cos(th), rev(r2 * cos(th)))
    y        <- c(r1 * sin(th), rev(r2 * sin(th)))
    return(data.frame(x, y))
  }
  ggplot() +
    geom_polygon(data = get.poly(breaks[1], breaks[2]), aes(x, y), fill = "red") +
    geom_polygon(data = get.poly(breaks[2], breaks[3]), aes(x, y), fill = "gold") +
    geom_polygon(data = get.poly(breaks[3], breaks[4]), aes(x, y), fill = "forestgreen") +
    geom_polygon(data = get.poly(pos - 0.2, pos + 0.2, 0.2), aes(x, y)) +
    geom_text(data = as.data.frame(breaks), size = 5, fontface = "bold", vjust = 0,
              aes(x = 1.1 * cos(pi * (1 - breaks / 10)), y = 1.1 * sin(pi * (1 - breaks / 10)), label = paste0(breaks))) +
    annotate("text", x = 0, y = 0, label = paste0(pos, " Points"), vjust = 0, size = 8, fontface = "bold") +
    coord_fixed() +
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank())
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observeEvent(input$predict, {
    # Update the selected tab when Predict button is clicked
    updateTabsetPanel(session, "inTabset",
                      selected = "pnlPredict")
    
    # Call the prediction function with the new UI inputs
    result <- prediction(
      inpFeat1 = input$PAY_0,
      inpFeat2 = input$PAY_2,
      inpFeat3 = input$PAY_4,
      inpFeat4 = input$LIMIT_BAL,
      inpFeat5 = input$PAY_3
    )
    
    # Check if the result is valid
    if (is.null(result$result[[1]][[1]]) || is.na(result$result[[1]][[1]])) {
      output$summary <- renderText("Error: Invalid prediction result")
      return()
    }
    
    # Extract prediction result and other details
    pred <- result$result[[1]][[1]]
    modelVersion <- result$release$model_version_number
    responseTime <- result$model_time_in_ms
    
    # Display prediction result
    output$summary <- renderText({paste0("Quality estimate is ", round(pred, 2))})
    output$version <- renderText({paste0("Model version used for scoring: ", modelVersion)})
    output$reponsetime <- renderText({paste0("Model response time: ", responseTime, " ms")})
    
    # Plot the result as a gauge (using your existing code)
    output$plot <- renderPlotly({
      gauge(round(pred, 2))
    })
  })
}


# Run the application
shinyApp(ui = ui, server = server)
