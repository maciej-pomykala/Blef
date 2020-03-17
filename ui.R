library(shinyalert)

ui <- fluidPage(
  useShinyalert(),
  br(),
  actionButton(inputId = "help", label = "", icon = icon("info")),
  uiOutput("game_button", inline = TRUE),
  br(),
  uiOutput("n_ais"),  
  uiOutput("cards"),
  uiOutput("move_1"),
  uiOutput("move_2"),
  uiOutput("history")
)