# Server

library(shiny)
library(shinydashboard)
library(lubridate)

source("kf6-api.R")

server <- shinyServer(function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)
  
  #' API authentication - reactive
  #' 
  #' @return An authentication token
  get_token <- eventReactive(input$loginButton, {
    login(input$host, input$username, input$password)
  })
  
  #' Dynamically update login inputs
  observeEvent(input$loginButton, {
    if (is.null(get_token())) return(NULL)
    
    # hide(id = "loginMenu", anim = TRUE)
    hide(id = "host", anim = TRUE)
    hide(id = "username", anim = TRUE)
    hide(id = "password", anim = TRUE)
    hide(id = "loginButton", anim = TRUE)
  })
  
  #' Update user info in sidebar
  output$userInfo <- renderMenu({
    if(is.null(get_token()))
      return(NULL)
    
    me = get_me(get_token())
    
    sidebarUserPanel(paste(me$firstName, me$lastName), me$role, "https://s-media-cache-ak0.pinimg.com/originals/4f/4f/91/4f4f912d1cd59d0a2d97d4620bf67e31.png")
  })
  
  #' List all communities
  #' 
  #' @return A table listing communities
  output$tabCommunities <- DT::renderDataTable(DT::datatable({
    if(is.null(get_token()))
      return(NULL)
    
    get_communities(get_token())
  }))
  
  #' Count of communities - info box
  output$communityBox <- renderInfoBox({
    infoBox("Communities", 
            nrow(get_communities(get_token())), 
            icon = icon("credit-card"))
  })
  
  #' Count of notes authored - info box
  output$communityBox <- renderValueBox({
    count = nrow(get_communities(get_token()))
    
    valueBox(
      value = count,
      subtitle = "# Communities",
      icon = icon("object-group"),
      color = if (count >= 5) "yellow" else "aqua"
    )
  })
  
  output$socialBox <- renderValueBox({
    valueBox(
      15,
      "# Peers Connected",
      icon = icon("users")
    )
  })
  
  output$writingBox <- renderValueBox({
    count = 2
    valueBox(
      count,
      "# Notes Written",
      icon = icon("pencil"),
      color = if (count < 5) "red" else {if (count < 10) "yellow" else "aqua"}
    )
  })
  
  output$plot1 <- renderPlot({
    if(is.null(get_token()))
      return(NULL)
    
    # Use isolate() to avoid dependency on input$obs
    data <- isolate(histdata[seq_len(50)])
    hist(data)
  })
  
  
  output$notifyMenu <- renderMenu({
    # Take a dependency on input$goButton. This will run once initially,
    # because the value changes from NULL to 0.
    
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
)