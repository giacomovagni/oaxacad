
#
library(magrittr)
library(broom)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(gtools)
library(oaxaca)
library(oaxacad)

library(shiny)
#

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Blinder-Oaxaca Decomposition Simulation"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      helpText("Simulate Blinder-Oaxada Decomposition"),

      sliderInput(inputId = "intercept1",label = "Intercept Group 0 (red)",min = 80,max = 150,value = 100),
      sliderInput(inputId = "intercept2",label = "Intercept Group 1 (blue)",min = 80,max = 150,value = 100),

      sliderInput(inputId = "beta1", label = "Beta Group 0",min = 0,max = 20, value = 10),
      sliderInput(inputId = "beta2", label = "Beta Group 1",min = 0,max = 20, value = 5),

      sliderInput(inputId = "xA_avr",label = "Characteristics of Group 0 (X)",min = 0,max = 20,value = 20),
      sliderInput(inputId = "xB_avr",label = "Characteristics of Group 1 (X)",min = 0,max = 20,value = 10),

      numericInput(inputId = "xA_sd", label = 'Variance of X (SD) (Group 0)', value = 5),
      numericInput(inputId = "xB_sd", label = 'Variance of X (SD) (Group 1)', value = 5),

      sliderInput(inputId = "N",label = "Sample Size", min = 100,max = 2000, value = 1000),

      numericInput(inputId = "errorA_avr", label = 'Random Error for Group 0 (central value)', value = 0),
      numericInput(inputId = "errorB_avr", label = 'Random Error for Group 1 (central value)', value = 0),

      numericInput(inputId = "errorA_sd", label = 'Error for Group 0 (SD)', value = 1),
      numericInput(inputId = "errorB_sd", label = 'Error for Group 0 (SD)', value = 1),

      tableOutput("table"),

    ),

    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput(outputId = "ggplot"),

      p("Horizontal Line, show the average values of Y, i.e. the outcome, for Group 0 (red) and Group 1 (blue)"),
      p("Vertical Line, show the average values of X (Observed Characteristics) for Group 0 (red) and Group 1 (blue)"),

      # Output: Histogram ----
      hr(),

      p("Group Observed Characteristics (X) and outcome (Y)"),

      tableOutput("characteristics"),

      hr(),

      h2("Interpretation"),

      textOutput("text"),

      hr(),

      tableOutput("decomp"),

      hr(),

      h2("Threefold decomposition"),
      p("X0 denotes the charcteristics of Group 0, and Beta 0 the regression coefficient for Group 0, etc."),
      p("Note that you can vary the Beta 0, Beta 1, and X0, X1, with the sliders."),

      tableOutput("decomp2"),

      p("The endowment refers to the observed characteristics (X) and corresponds to the explained part."),
      p("The explained part is : (X1 - X0) B0"),
      p("The unexplained part is : (B1 - B0) X0 and the interaction")

    )
  )
)


# dd = oax  ()
#dd = oaxaca_sim()
#dd$decomp_reg
# dd$simulated_data$dataframe

# interceptA_avr = input$intercept1
# Define server logic required to draw a histogram ----
server <- function(input, output) {

  #
  ox = reactive(oaxaca_sim(
    interceptA_avr = input$intercept1,
    interceptB_avr = input$intercept2,

    xA_avr = input$xA_avr,
    xB_avr = input$xB_avr,

    betaA = input$beta1,
    betaB = input$beta2,

    errorA_sd = input$errorA_sd,
    errorB_sd = input$errorB_sd,

    errorA_avr = input$errorA_avr,
    errorB_avr = input$errorB_avr,

    xA_sd = input$xA_sd, xB_sd = input$xB_sd))
  #

  #
  output$ggplot <- renderPlot({
    ox()$simulated_data$fig
  })

  output$characteristics <- renderTable({
    ox()$simulated_data$dataframe %>%
      group_by(group) %>%
      summarise(n(), mean(X), mean(Y))
  })

  output$text = renderPrint({
    cat("If people in Group 1 had the same measured characteristics as those in Group 0, we WOULD have observed", ox()$decomp_twofold$Decomposition[5], "(more or less) of Y")
  })

  output$decomp <- renderTable({
    ox()$decomp_twofold
  })
  #

  #
  output$decomp2 <- renderTable({
    ox()$decomp_reg
  })
}

#
shinyApp(ui = ui, server = server)
#

