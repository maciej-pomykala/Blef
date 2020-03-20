library(shinyalert)
library(magrittr)
library(dplyr)
library(arrangements)
library(readr)
source("functions.R")
source("ai.R")
source("text.R")

big_text <- h4

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

server <- function(input, output) {
  source("server_ui.R", local = TRUE)
  source("server_mechanics.R", local = TRUE)
}

# TO DO:
# Fix the reverting hand type
# Complete Polish instructions
# Improve AI
# * cards are now a data frame - not matrix
# * colours names replaced with symbols
# * the game is now in two languages, except for two items