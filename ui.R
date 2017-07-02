# Client

library(shiny)
library(shinydashboard)
library(shinyjs)

tasks = dropdownMenu(type = "tasks", badgeStatus = "success",
                     taskItem(value = 90, color = "green", "Documentation"),
                     taskItem(value = 17, color = "aqua", "Project X"),
                     taskItem(value = 75, color = "yellow", "Server deployment"),
                     taskItem(value = 80, color = "red", "Overall project"))

header <- dashboardHeader(title = "KF6 Shiny Dash",
                          dropdownMenuOutput("notifyMenu"),
                          tasks)

sidebar <- dashboardSidebar(
  useShinyjs(),
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("My Contributions", tabName = "personal", icon = icon("th"), badgeLabel = "new", badgeColor = "green")
    # menuItem("Source code", icon = icon("file-code-o"), href = "https://github.com/meefen/"),
    # menuItem("Controls", icon = icon("gear"))
  ),
  sidebarMenu(
    textInput("host", "Host:", "https://kf6.ikit.org/"),
    textInput("username", "Username:", "bodong.chen@gmail.com"),
    passwordInput("password", "Password:", "000000"),
    actionButton("loginButton", label = "Login"),
    id = "loginMenu"
  ),
  sidebarMenu(
    dateRangeInput("daterange", label="Date Range:"),
    id = "controlMenu"
  ),
  sidebarMenuOutput("userInfo")
)

body <- dashboardBody(
  tabItems(
    # 1st tab content
    tabItem(tabName = "dashboard",
            fluidRow(
              box(title = "Communities", 
                  status = "primary",
                  solidHeader = TRUE, 
                  DT::dataTableOutput("tabCommunities")),
              
              box(title = "Overview", 
                  status = "primary",
                  solidHeader = TRUE, 
                  plotOutput("plot1", height = 250))
            )
    ),
    
    # 2nd tab content
    tabItem(tabName = "personal",
            h2("My Contributions"),
            
            # infoBoxes with fill=FALSE
            fluidRow(
              valueBoxOutput("communityBox"),
              valueBoxOutput("socialBox"),
              valueBoxOutput("writingBox")
              # valueBoxOutput("readingBox")
            )
            
            # # infoBoxes with fill=TRUE
            # fluidRow(
            #   infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE),
            #   infoBoxOutput("progressBox2"),
            #   infoBoxOutput("approvalBox2")
            # ),
            # 
            # fluidRow(
            #   # Clicking this will increment the progress amount
            #   box(width = 4, actionButton("count", "Increment progress"))
            # )
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "green")
