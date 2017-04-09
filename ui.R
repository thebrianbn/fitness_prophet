library(shiny)

ui = fluidPage(
  headerPanel("Fitness Prophet"), tags$head(tags$link(rel = "icon", href = "/www/favicon.ico"),
                                           tags$title("YourTitle")) ,
  sidebarLayout(
    sidebarPanel(
      titlePanel("General Information"),
      inputPanel(
        radioButtons("gender", "Gender", c("Male", "Female")),
        sliderInput("age", "Age", 12, 115, 67),
        sliderInput("height", "Height (inches)", 48, 96, 72, step = .1),
        sliderInput("weight", "Weight (lbs)", 50, 300, 175, step = .1),
        sliderInput("waist", "Waist (inches)", 10, 50, 30, step = .1), width = 3),
      titlePanel("Daily intake"),
      inputPanel(
        sliderInput("calories", "Calories", 0, 3000, 1500, step = 1),
        sliderInput("protein", "Protein (g)", 0, 100, 50, step = .1),
        sliderInput("fat", "Fat (g)", 0, 100, 50, step = .1),
        sliderInput("carbs", "Carbohydrates (g)", 0, 500, 250),
        sliderInput("sugar", "Sugar (g)", 0, 100, 50, step = .1),
        sliderInput("sodium", "Sodium (g)", 0, 10, 5, step = .1), width = 3),
      titlePanel("Running Performance"),
      inputPanel(
        sliderInput("days", "Number of Days in a Week", 0, 7, 4),
        sliderInput("miles", "Number of Miles per Week", 0, 25, 12, step = .1),
        sliderInput("time", "Weekly Total Running Time (m)", 0, 500, 250, step = .1), width = 3),
    width = 6),
    mainPanel(
    tabsetPanel(
      tabPanel("Future Weight Plot", plotOutput("weight_loss")),
      tabPanel("Weight Prediction Table", dataTableOutput("weight_table")),
      tabPanel("Recommendations", 
               textOutput("user_bmr"),
               textOutput("user_bai"),
               textOutput("recommend"))),
    width = 6
    )
  )
)