# Server

library(shiny)
library(shinydashboard)

source("kf6-api.R")

token = NA

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    input$goButton
    
    # Use isolate() to avoid dependency on input$obs
    data <- isolate(histdata[seq_len(50)])
    hist(data)
  })
  
  
  output$notifyMenu <- renderMenu({
    # Take a dependency on input$goButton. This will run once initially,
    # because the value changes from NULL to 0.
    input$login
    
    token <<- login()
    
    
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    
    messageData = data.frame(text = c("5 new users today", "12 items delivered", "Server load at 86%"),
                             icon = c("users", "truck", "exclamation-triangle"),
                             status = c("success", "success", "warning"))
    
    msgs <- apply(messageData, 1, function(row) {
      notificationItem(text = row[["text"]], 
                       icon = icon(row[["icon"]]), 
                       status = row[["status"]])
    })
    
    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
      input$login
      get_communities(token)
    }))
    
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = "notifications", .list = msgs)
  })
  
  output$progressBox <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple"
    )
  })
  output$approvalBox <- renderInfoBox({
    infoBox(
      "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  # Same as above, but with fill=TRUE
  output$progressBox2 <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple", fill = TRUE
    )
  })
  output$approvalBox2 <- renderInfoBox({
    infoBox(
      "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow", fill = TRUE
    )
  })
}
