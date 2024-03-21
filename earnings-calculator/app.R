library(shiny)
library(dplyr)
library(purrr)

# Iz is on plan 1, Dave on plan 2

take_home <- function(gross, student_loan_plan){

  advanced_rate <- max(0, gross - 125140)
  basic_rate <- gross - advanced_rate - 12570

  income_tax <- 0.4 * basic_rate + 0.45 * advanced_rate

  ni_advanced_band <- max(0, gross - (4189 * 12))
  ni_main_band <- gross - 242 - ni_advanced_band

  national_insurance <- 0.1 * ni_main_band + 0.02 * ni_advanced_band

  student_loan_threshold <-
    case_match(student_loan_plan,
               0 ~ Inf,
               1 ~ 22015,
               2 ~ 27295)

  student_loan <- 0.09 * (gross - student_loan_threshold)

  gross - income_tax - national_insurance - student_loan

}

# Define UI for application that draws a histogram
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h3("Days worked"),
      sliderInput("dave_days", "Dave", min = 0, max = 5, step = 0.5, value = 3,
                  ticks = FALSE),
      sliderInput("dave_training_days", "Dedicated training days", min = 0, max = 2, step = 0.5, value = 1,
                  ticks = FALSE),
      sliderInput("iz_days",   "Iz",   min = 0, max = 5, step = 0.5, value = 2.5,
                  ticks = FALSE),
      textInput("childcare_cost", "Childcare cost per day (£)", value = "82"),
      selectInput("training", "Theological training",
                  choices = c("None", "Crosslands (£3,100 p.a)"),
                  selected = "Crosslands (£3,100 p.a)")
    ),
    mainPanel(
      h2("Combined take home pay"),
      textOutput("household_income"),
      h2("Childcare & training"),
      p("Annual childcare spend:", textOutput("childcare_spend", inline = TRUE)),
      p("Take home pay after childcare and training:", textOutput("after_childcare", inline = TRUE))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  training_cost <- reactive({
    case_match(
      input$training,
      "None" ~ 0,
      "Crosslands (£3,100 p.a)" ~ 3100
    )

  })


  salaries <- read.csv("salaries.csv")
  salaries <- list(dave = salaries$gbp_pa[salaries$name == "Dave"],
                   iz   = salaries$gbp_pa[salaries$name == "Isobel"])

  earnings <- reactive({
    map2_dbl(salaries, c(input$dave_days, input$iz_days),
             \(x, y) x * y/5)
  })

  iz_take_home   <- reactive(take_home(earnings()["iz"],   2))
  dave_take_home <- reactive(take_home(earnings()["dave"], 1))

  household_take_home <- reactive(iz_take_home() + dave_take_home())

  output$household_income <- renderText(paste0("£", round(household_take_home())))

  ## A coarse estimate, average of 15 hours in spring term and 30 in Autumn
  childcare_allowance_days <- 3

  childcare_allowance_spring <- 2
  childcare_allowance_autumn <- 4

  childcare_days_needed <- reactive(max(0, input$dave_days + input$iz_days + input$dave_training_days - 5))

  paid_childcare_days_spring <- reactive(max(0, childcare_days_needed() - childcare_allowance_spring))
  paid_childcare_days_autumn <- reactive(max(0, childcare_days_needed() - childcare_allowance_autumn))

  annual_childcare_cost     <- reactive({
    (as.numeric(input$childcare_cost) * paid_childcare_days_spring() * 3/12 * 52) +
      (as.numeric(input$childcare_cost) * paid_childcare_days_autumn() * 4/12 * 52) +
      (as.numeric(input$childcare_cost) * childcare_days_needed() * 5/12 * 52)
    })


  output$childcare_spend  <- renderText(paste0("£", round(annual_childcare_cost())))
  output$after_childcare <- renderText(paste0("£", round(household_take_home() - annual_childcare_cost() - training_cost())))


}

# Run the application
shinyApp(ui = ui, server = server)
