library(shinyalert)

ui <- fluidPage(
  useShinyalert(),
  br(),
  uiOutput("language_button", inline = T),
  actionButton(inputId = "help", label = "", icon = icon("info")),
  uiOutput("game_button", inline = T),
  br(),
  uiOutput("n_ais"),  
  hr(),
  uiOutput("cards"),
  hr(),
  uiOutput("move_1"),
  uiOutput("move_2"),
  uiOutput("history")
)