# Prior distribution: 
# Start by visualizing the prior distribution of the number of fish in the lake.
# You can use a histogram or a density plot to show the distribution.
# Add sliders or input fields to allow users to adjust the parameters of the prior distribution, such as
# the mean and standard deviation.
#
#
library(shiny)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Let's go fishin"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("FishPopulation",
                  "How many fish are actually in the lake:",
                  min = 1,
                  max = 1000,
                  value = 500),
      actionButton("new_lake",label = "New Lake")
    )
    
    # Show a plot of the generated distribution
#     mainPanel(
#       plotlyOutput("distPlot")
#     )
#   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # output$distPlot <- renderPlotly({
  #   # generate data based on input
  #   FishPopulation <- input$FishPopulation
  #   prior <- sample(1:FishPopulation,1)
  #   numNA <- FishPopulation-prior
  #   
  #   prior_plot_data <- data.frame("Population" = seq(1, FishPopulation),
  #                                 "Prior" = c(rep(1, prior), rep(0, numNA)),
  #                                 "P_of_Pop" = rep(1/FishPopulation, FishPopulation))
  #   
  #   # create the histogram plot
  #   distPlot <- prior_plot_data %>%
  #     ggplot(aes(x = Population, y = P_of_Pop, fill = factor(Prior))) +
  #     geom_histogram(stat = "identity", binwidth = 1, color = NA) +
  #     scale_fill_manual(values = c("red", "gray")) +
  #     labs(fill = "Prior") +
  #     theme_classic()
  #   
  #   # convert to interactive plot using plotly
  #   ggplotly(distPlot)
  }#)
# }

# Run the application 
shinyApp(ui = ui, server = server)
