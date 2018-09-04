#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#awesome fonts gallery: https://fontawesome.com/icons?d=gallery

#-------------------------------------------------------------------------------------
library('shiny')
library('shinydashboard')
library('tidyverse')
library('leaflet')

##DATA--------------------------------------------------------------------------------
filepath <- '../Fortnite_tweets_test.csv'
data <- read_csv(filepath)
#View(data)

##HEADER------------------------------------------------------------------------------
header <- dashboardHeader(
  dropdownMenu(
    #types: 'messages', 'tasks', 'notifications'
    type = 'messages',
    messageItem(
      from = 'Talqa',
      message = 'Find lamas here!',
      href = 'https://twitter.com/epicgames'
    )
  ),
  dropdownMenu(
    type = 'notifications',
    notificationItem(
        text = 'I see lamas!'
    )
  ),
  dropdownMenu(
    type = 'tasks',
    taskItem(
      text = 'Hunting lamas...',
      value = 5
    )
  )
  #menu for messages from file
  #dropdownMenuOutput("msg_menu")
  
)

##SIDEBAR-----------------------------------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    #tabs
    menuItem('Where to find lamas',
      tabName = 'map',
      icon = icon('map', 'font-awesome')
    ),
    menuItem('Lamas',
             tabName = 'lamas',
             icon = icon('paw', 'font-awesome')
    ),
    menuItem('My lama farm',
             tabName = 'farm',
             icon = icon('trophy', 'font-awesome')
    )
  ),
  
  hr(),
  #selection based on data
  selectInput(inputId = 'platform_select',
              label = 'Lama hunters',
              choices = data$source #CONNECT THIS TO REACTIVE DATA SOURCE!!!
  ),
  #slider
  sliderInput(inputId = 'platform_limit',
              label = 'How many?',
              min = 0,
              max = 50,
              value = 15
  ),
  actionButton('click',
               'Get Lama'
  )
)

##BODY--------------------------------------------------------------------------------
body <- dashboardBody(
  tags$head(
    tags$style(
      HTML(
        'h3 {
            font-weight : bold;
            }'
      )
    )
  ),
  tabItems(
    tabItem(tabName = 'map',
            'I see where lamas are.',
            leafletOutput('plot')
            ),
    tabItem(tabName = 'lamas',
            'HERE BE LAMAS!',
            textOutput(outputId = 'platform_select'),
            tableOutput('table')
            ),
    tabItem(tabName = 'farm',
            fluidRow(
              tabBox(
                width = 12,
                title = 'My lama collection.',
                tabPanel('Lama 1'),
                tabPanel('Lama 2')
                )),
            fluidRow(
              column(width = 6,
                valueBox(
                  width = NULL,
                  value = length(data$text),
                  subtitle = 'All Lamas',
                  color = 'purple',
                  icon = icon(name = 'globe', lib = 'font-awesome')
              )),
              column(width = 6,
                valueBox(
                  width = NULL,
                  value = length(unique(data$user_id)),
                  subtitle = 'Lama hunters',
                  color = 'purple',
                  icon = icon(name = 'gamepad', lib = 'font-awesome')
                )
              )
              ),
            fluidRow(
              valueBoxOutput('click_box', width = 6),
              column(width = 6,
                infoBox(
                  width = NULL,
                  color = 'purple',
                  title = 'HUNTING LAMAS...',
                  subtitle = 'Get them all!'
                )
              )
            )
            
            )
  )
)

##UI---------------------------------------------------------------------------------
ui <- dashboardPage(skin = 'purple',
                    header, 
                    sidebar, 
                    body)

##SERVER-----------------------------------------------------------------------------
server <- function(input, output, session) {
  reactive_twitter_data <- reactiveFileReader(intervalMillis = 60000,
                                              session = session,
                                              filePath = filepath,
                                              readFunc = read_csv
                                              )
  
  output$platform_select <- renderText({
                                input$platform_select
                            })
  
  output$table <- renderTable({ 
                      data <- reactive_twitter_data()
                      selection <- data[data$source <= input$platform_select, ]
                  })
  
  output$click_box <- renderValueBox({
                          valueBox(value = input$click,
                                   subtitle = 'Lamas caught!',
                                   color = if( input$click < input$platform_limit) { 'purple'} else { 'fuchsia' },
                                   icon = icon(name = 'paw', lib = 'font-awesome')
                          )
                      })
  
  output$plot <- renderLeaflet({
                      leaflet() %>% 
                        addTiles() #%>% 
                        # addMarkers(
                        #   lng = 0, #split geo_coords to get this,
                        #   lat = 0 #split geo_coords to get this
                        #)
  })
  
  #messages generated from file
  # output$msg_menu <- renderMenu({
  #   messages <- apply(data, 1, function(row) {
  #     messageItem(from = row[["from"]],
  #                 message = row[["message"]])
  #   })
  #   dropdownMenu(type = "message", .list = messages)
  # })
}

shinyApp(ui, server)

#TEMPLATE-----------------------------------------------------------------
# # Define UI for application that draws a histogram
# ui <- fluidPage(
#    
#    # Application title
#    titlePanel("Old Faithful Geyser Data"),
#    
#    # Sidebar with a slider input for number of bins 
#    sidebarLayout(
#       sidebarPanel(
#          sliderInput("bins",
#                      "Number of bins:",
#                      min = 1,
#                      max = 50,
#                      value = 30)
#       ),
#       
#       # Show a plot of the generated distribution
#       mainPanel(
#          plotOutput("distPlot")
#       )
#    )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
#    
#    output$distPlot <- renderPlot({
#       # generate bins based on input$bins from ui.R
#       x    <- faithful[, 2] 
#       bins <- seq(min(x), max(x), length.out = input$bins + 1)
#       
#       # draw the histogram with the specified number of bins
#       hist(x, breaks = bins, col = 'darkgray', border = 'white')
#    })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)

