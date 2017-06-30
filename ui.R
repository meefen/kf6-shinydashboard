# Client

library(shiny)
library(shinydashboard)

tasks = dropdownMenu(type = "tasks", badgeStatus = "success",
             taskItem(value = 90, color = "green", "Documentation"),
             taskItem(value = 17, color = "aqua", "Project X"),
             taskItem(value = 75, color = "yellow", "Server deployment"),
             taskItem(value = 80, color = "red", "Overall project"))

header <- dashboardHeader(title = "KF6 Shiny Dash",
                          dropdownMenuOutput("notifyMenu"),
                          tasks)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th"),
             badgeLabel = "empty", badgeColor = "green"),
    # menuItem("Source code", icon = icon("file-code-o"), 
    #          href = "https://github.com/meefen/"),
    textInput("username", "Username:", "bodong.chen@gmail.com"),
    passwordInput("password", "Password:", "000000"),
    actionButton("login", label = "Login"),
    # menuItem("Controls", icon = icon("gear")),
    h4("Controls"),
    dateRangeInput("daterange", label="Date Range:")
  )
)

body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            fluidRow(
              box(title = "Communities", 
                  status = "primary",
                  # background = "maroon",
                  solidHeader = TRUE, 
                  DT::dataTableOutput("table")),
              
              box(title = "Overview", 
                  status = "primary",
                  # background = "maroon",
                  solidHeader = TRUE, 
                  plotOutput("plot1", height = 250))
            ),
            
            # infoBoxes with fill=FALSE
            fluidRow(
              # A static infoBox
              infoBox("New Orders", 10 * 2, icon = icon("credit-card")),
              # Dynamic infoBoxes
              infoBoxOutput("progressBox"),
              infoBoxOutput("approvalBox")
            ),
            
            # infoBoxes with fill=TRUE
            fluidRow(
              infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE),
              infoBoxOutput("progressBox2"),
              infoBoxOutput("approvalBox2")
            ),
            
            fluidRow(
              # Clicking this will increment the progress amount
              box(width = 4, actionButton("count", "Increment progress"))
            )
    ),
    
    # Second tab content
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "green")
