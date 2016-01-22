shinyUI(pageWithSidebar(
  headerPanel('Loan Data'),
  sidebarPanel(
  fluidRow(
    column(6,  
      h4("Approved"),
      h5("Mean:"),
      sliderInput('muAX', 'X', min = 0, max = 200, value = 4),
      sliderInput('muAY', 'Y', min = 0, max = 200, value = 150),
      
      h5("Standard Deviation:"),
      sliderInput('sdAX', 'X', 3, min = 0, max = 60),
      sliderInput('sdAY', 'Y', 3, min = 0, max = 60)
    ),
    column(6, 
      h4('Denied'),
      h5('Mean:'),
      sliderInput('muDX', 'X', min = 0, max = 100, value = 4),
      sliderInput('muDY', 'Y', min = 0, max = 100, value = 150),
      
      h5("Standard Deviation:"),
      sliderInput('sdDX', 'X', 3, min = 0, max = 50),
      sliderInput('sdDY', 'Y', 3, min = 0, max = 50)
    ))
  
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput('plot1')),
      tabPanel("Table", tableOutput( 'table1'))
    )
  )
))

