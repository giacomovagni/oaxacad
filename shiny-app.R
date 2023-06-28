
library("devtools")
install_github("giacomovagni/oaxacad")
#
library(magrittr)
library("broom")
library(dplyr)
library("ggplot2")
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

      sliderInput(inputId = "intercept1",label = "Intercept Group 1 (red)",min = 80,max = 150,value = 100),
      sliderInput(inputId = "intercept2",label = "Intercept Group 2 (blue)",min = 80,max = 150,value = 100),

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

      p("Horizontal Line, show the average values of Y, i.e. the outcome, for Group 1 (red) and Group 2 (blue)"),
      p("Vertical Line, show the average values of X (Observed Characteristics) for Group 1 (red) and Group 2 (blue)"),

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
      p("X1 denotes the charcteristics of Group 1, and Beta 1 the regression coefficient for Group 1, etc."),
      p("The endowment refers to the observed characteristics (X) and corresponds to the explained part. \n Explained = endowment / gap"),

      hr(),

      p("ΔXβ [endowment] = X1-X2 * Beta (of reference) ----
        ΔβX [coef] =  β1-β2 * X (of reference) ----
        ΔβΔX = interaction"),

      p("Total gap = Σ (gap)[row 3]"),
      p("Explained = ΔXβ [endowment][row 4]"),

      tableOutput("decomp2"),

      textOutput("text2"),
      textOutput("text3")
    )
  )
)

#
# dd = oaxaca_sim()
# dd$decomp_simple$`% explained`
# dd$decomp_twofold
# dd$decomp_reg[]
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
      mutate(group = factor(ifelse(group == 0, 1, 2))) %>%
      group_by(group) %>%
      summarise(n(), mean(X), mean(Y))
  })

  output$text = renderPrint({
    cat("If people in Group 2 had the same measured characteristics as those in Group 1,
        we WOULD have observed", ox()$decomp_twofold$Decomposition[5],
        if( ox()$decomp_twofold$Decomposition[5] < 0) sign = "less" else sign = "more",
        "of Y. \n The explained part of the gap is ", round(ox()$decomp_simple$`% explained`,2), "%")
  })

  output$decomp <- renderTable({
    ox()$decomp_simple
  })
  #

  #
  output$decomp2 <- renderTable({
    ox()$decomp_reg
  })

  output$text2 = renderPrint({
    cat("Explained = ", ox()$decomp_reg$`ΔXβ [E]`[4] * 100 , "% ")
    })

  output$text3 = renderPrint({
    cat("Note that the Unexplained part (", ox()$decomp_reg$`ΔβX [coef]`[3] + ox()$decomp_reg$`ΔβΔX`[3], ") is the sum of ΔβX [coef] (",
        ox()$decomp_reg$`ΔβX [coef]`[3], ") plus the interaction ΔβΔX (", ox()$decomp_reg$`ΔβΔX`[3], ")")
  })


}

#
shinyApp(ui = ui, server = server)
#

