library(shinyalert)
library(magrittr)
library(dplyr)
library(arrangements)
library(readr)
source("functions.R")
source("ai.R")

big_text <- h3

state <- reactiveValues(
  game_status = "inactive",
  n_players = 0,
  n_cards = vector("numeric", 0),
  player_names = vector("character", 0),
  cards = matrix(NA, nrow = 0, ncol = 3),
  history = matrix(NA, nrow = 0, ncol = 4)
)

ui_elements <- reactiveValues()

server <- function(input, output) {
  
  proceed_normally <- function(action) {
    state$history <- rbind(state$history, action)
    state$current_player <- ifelse(state$current_player == state$n_players, 1, state$current_player + 1)
  }
  
  deal_cards_and_reset_history <- function() {
    state$cards <- draw_cards(state$n_cards)
    state$history <- matrix(NA, nrow = 0, ncol = 4)
  }
  
  give_ai_a_card <- function(player, exists = F) {
    player_name <- state$player_names[player]
    existence_text <- ifelse(exists, "Hand exists -", "Hand does not exist -")
    shinyalert(title = paste(existence_text, player_name, "gets a card"), text = display_all_cards(state$player_names, state$cards)) 
    state$n_cards[player] <- state$n_cards[player] + 1
  }
  
  erase_player <- function(player) {
    player_name <- state$player_names[player]
    shinyalert(title = paste("Hand exists -", player_name, "drops"), text = display_all_cards(state$player_names, state$cards)) 
    state$n_players <- state$n_players - 1
    state$player_names <- state$player_names[-player]
    state$n_cards <- state$n_cards[-player]
  }
  
  kick_out_human <- function() {
    shinyalert(title = "Hand exists - you lost the game...", text = display_all_cards(state$player_names, state$cards))
    state$game_status <- "inactive"
  }
  
  make_human_win <- function() {
    shinyalert(title = "Hand exists - you won the game!", text = display_all_cards(state$player_names, state$cards))
    state$game_status <- "inactive"
  }
  
  
  # Top buttons reactions
  observeEvent(input$help, {
    shinyalert(
      title = "Instructions",
      text = "The game of Blef",
      type = "info"
    )
  })
  
  output$game_button <- renderUI({
    if (state$game_status == "active") {
      actionButton(inputId = "end_button", label = "End game")
    } else if (state$game_status == "inactive") {
      actionButton(inputId = "start_button", label = "Start game")
    }
  })
  
  observeEvent(input$start_button, {
    state$game_status <- "active"
    state$n_players <- 1 + as.numeric(input$game_n_ais)
    state$n_cards <- rep(1, state$n_players)
    state$player_names <- c("You", ai_names[1:(state$n_players - 1)])
    state$current_player <- 1
    state$cards <- draw_cards(state$n_cards)
    state$max_cards <- floor(24 / state$n_players)
    state$history <- matrix(NA, nrow = 0, ncol = 4)
    
    ui_elements$last_hand_type <- "High card"
  })
  
  observeEvent(input$end_button, {
    state$game_status <- "inactive"
  })
  
  # Inactive game UI
  output$n_ais <- renderUI({
    if(state$game_status == "inactive")
      tagList(
        br(),
        selectInput(inputId = "game_n_ais", label = "Number of AIs", choices = 1:5, selected = 1)
      )
  })
  
  # Active game UI
  observeEvent(state$cards, {
    output$display_own_cards <- renderUI({
      display_own_cards(matrix(state$cards[state$cards[, 1] == 1, ], ncol = 3))
    })
  })
  
  output$display_n_cards <- renderUI({
    display_n_cards(state$player_names, state$n_cards)
  })
  
  output$cards <- renderUI({
    if(state$game_status == "active") {
      tagList(
        big_text("Cards in play"),
        htmlOutput("display_own_cards"),
        htmlOutput("display_n_cards")
      )
    }
  })
  
  observeEvent(state$history, {
    if (nrow(state$history) > 0) {
      ui_elements$check_button <- actionButton(inputId = "check", label = "Check")
    } else {
      ui_elements$check_button <- NULL
    }
  })
  
  observeEvent(state$game_status, {
    ui_elements$hand_type <- radioButtons(
      inputId = "hand_type", 
      label = "Type", 
      choices = all_hand_types,
      selected = ui_elements$last_hand_type,
      inline =  TRUE, 
      width = 800
    )
  })
  
  output$move_1 <- renderUI({
    if(state$game_status == "active") {
      tagList(
        big_text("Your move"),
        actionButton(inputId = "hand_confirm", label = "Confirm selected hand"),
        ui_elements$check_button,
        br(),
        br(),
        ui_elements$hand_type
      )
    }
  })
  
  output$move_2 <- renderUI({
    if(state$game_status == "active") {
      tagList(
        ui_elements$hand_detail_1,
        ui_elements$hand_detail_2
      )
    }
  })
  
  observeEvent(state$history, {

    output$history <- renderUI({
      if(state$game_status == "active") {
        tagList(
          big_text("History"),
          renderUI({tell_history(state$player_names, state$history)})
        )
      }
    })
  })
  
  observeEvent(input$hand_type, {
    if (input$hand_type %in% c("High card", "Pair", "Three of a kind", "Four of a kind")) {
      ui_elements$hand_detail_1 <- radioButtons(
        inputId = "hand_detail_1", 
        label = "Card value", 
        choiceNames = value_names, 
        choiceValues = 1:6,
        inline =  TRUE
      )
      ui_elements$hand_detail_2 <- NULL
    } else if (input$hand_type %in% c("Two pairs", "Full house")) {
      ui_elements$hand_detail_1 <- radioButtons(
        inputId = "hand_detail_1", 
        label = ifelse(input$hand_type == "Two pairs", "Higher card value", "Value of 3 cards"), 
        choiceNames = value_names, 
        choiceValues = 1:6,
        inline =  TRUE
      )
      ui_elements$hand_detail_2 <- radioButtons(
        inputId = "hand_detail_2", 
        label = ifelse(input$hand_type == "Two pairs", "Lower card value", "Value of 2 cards"), 
        choiceNames = value_names, 
        choiceValues = 1:6,
        inline =  TRUE
      )
    } else if (input$hand_type %in% c("Small straight", "Big straight", "Great straight")) {
      ui_elements$hand_detail_1 <- NULL
      ui_elements$hand_detail_2 <- NULL
    } else if (input$hand_type %in% c("Colour", "Small flush", "Big flush", "Great flush")) {
      ui_elements$hand_detail_1 <- radioButtons(
        inputId = "hand_detail_1", 
        label = "Colour", 
        choiceNames = colour_names, 
        choiceValues = 1:4,
        inline =  TRUE
      )
      ui_elements$hand_detail_2 <- NULL
    }
  })
  
  # Action
  observeEvent(input$check, {
    statement <- state$history[nrow(state$history), ]
    hand_exists <- determine_hand_existence(state$cards, statement)
    cp <- state$current_player
    lp <- ifelse(cp == 1, state$n_players, cp - 1) 
    
    # If hand exists, make current player lose
    if (hand_exists) {
      if (state$n_cards[cp] > state$max_cards) kick_out_human() 
      if (state$n_cards[cp] <= state$max_cards) {
        shinyalert(title = "Hand exists - you get a card", text = display_all_cards(state$player_names, state$cards)) 
        state$n_cards[cp] <- state$n_cards[cp] + 1
      }
      state$current_player <- 1
    } 
    # If hand does not exist, make previous player lose
    if (!hand_exists) {
      knock_out <- state$n_cards[lp] == state$max_cards
      if(knock_out & state$n_players == 2) make_human_win()
      if(knock_out & state$n_players >= 2) erase_player(lp)
      if(!knock_out) give_ai_a_card(lp) 
      state$current_player <- lp 
    }
    deal_cards_and_reset_history()
  })
  
  
  observeEvent(input$hand_confirm, {
    ui_elements$last_hand_type <- input$hand_type
    
    if (nrow(state$history) > 0) {
      previous <- state$history[nrow(state$history), ]
      current <- c(1, input$hand_type, input$hand_detail_1, input$hand_detail_2)
      legal <- see_if_legal(previous, current)
    } else {
      legal <- T
    }
    
    if(!legal) {
      shinyalert(title = "Not possible", text = "This bet is not higher than the last one.")
    } else {
      if (!is.null(ui_elements$hand_detail_2)) {
        action <- c(1, input$hand_type, input$hand_detail_1, input$hand_detail_2)
      } else if (!is.null(ui_elements$hand_detail_1)) {
        action <- c(1, input$hand_type, input$hand_detail_1, NA)
      } else {
        action <- c(1, input$hand_type, NA, NA)
      }
      proceed_normally(action)
    }
  })
  
  observeEvent(state$current_player, {
    if(state$current_player > 1) {
      while (state$current_player > 1) {
        cp <- state$current_player
        lp <- ifelse(cp == 1, state$n_players, cp - 1) 
        cp_name <- state$player_names[cp]
        p_cards <- state$cards[state$cards[, 1] == cp, ]
        action <- get_action(state$n_cards, p_cards, state$history, cp)

        if(action[2] == "check") {
          statement <- state$history[nrow(state$history), ]
          hand_exists <- determine_hand_existence(state$cards, statement)
          # If hand exists, make current player lose
          if (hand_exists) {
            knock_out <- state$n_cards[cp] == state$max_cards
            
            if (knock_out & state$n_players == 2 & cp != 1) make_human_win()
            if (knock_out & state$n_players > 2 & cp != 1) {
              erase_player(cp)
            }
            if(!knock_out) {
              give_ai_a_card(cp, exists = T) 
            } 
            state$current_player <- cp - 1 
            deal_cards_and_reset_history()
          } 
          # If hand does not exist, make previous player lose
          if(!hand_exists) {
            knock_out <- state$n_cards[lp] == state$max_cards
            if (knock_out & lp == 1) kick_out_human()
            if (knock_out & lp != 1 & state$n_players == 2) make_human_win()
            if (knock_out & lp != 1 & state$n_players >= 2) erase_player(cp)
            if (!knock_out) give_ai_a_card(lp, exists = F)
            state$current_player <- lp 
          } 
          deal_cards_and_reset_history()
        } else {
          proceed_normally(action)
        }
        Sys.sleep(1)
      }
    }
  })
}

# TO DO:
# Fix the reverting hand type
# Finish AI