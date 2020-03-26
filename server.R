library(shinyalert)
library(magrittr)
library(dplyr)
library(arrangements)
library(readr)
source("R/functions.R")
source("R/ai.R")
source("app/text.R")

big_text <- h4

server <- function(input, output) {
  state <- reactiveValues(
    game_status = "inactive",
    n_players = 0,
    n_cards = vector("numeric", 0),
    player_names = vector("character", 0),
    cards = data.frame(player = numeric(0), value = numeric(0), colour = numeric(0)),
    history = matrix(NA, nrow = 0, ncol = 4)
  )
  
  ui_elements <- reactiveValues()
  language <- reactiveVal("English")
  
  source("app/server_ui.R", local = TRUE)
  source("app/server_mechanics.R", local = TRUE)
}
