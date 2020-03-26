proceed_normally <- function(action) {
  state$history <- rbind(state$history, action)
  state$current_player <- ifelse(state$current_player == state$n_players, 1, state$current_player + 1)
}

deal_cards_and_reset_history <- function() {
  state$cards <- draw_cards(state$n_cards)
  state$history <- matrix(NA, nrow = 0, ncol = 4)
}

give_human_a_card <- function(hand_exists) {
  existence_text <- ifelse(hand_exists, text$hand_exists_true[[language()]], text$hand_exists_false[[language()]])
  shinyalert(
    title = paste(existence_text, text$human_gets_card[[language()]]), 
    text = display_all_cards(state$player_names, state$cards, state$history, language()),
    html = T
  ) 
  state$n_cards[1] <- state$n_cards[1] + 1
  state$current_player <- 1
}

give_ai_a_card <- function(player, hand_exists) {
  player_name <- state$player_names[player]
  existence_text <- ifelse(hand_exists, text$hand_exists_true[[language()]], text$hand_exists_false[[language()]])
  shinyalert(
    title = paste(existence_text, player_name, text$ai_gets_card[[language()]]), 
    text = display_all_cards(state$player_names, state$cards, state$history, language()),
    html = T
  ) 
  state$n_cards[player] <- state$n_cards[player] + 1
}

erase_player <- function(player) {
  player_name <- state$player_names[player]
  shinyalert(
    title = paste(text$hand_exists_true[[language()]], player_name, text$drops[[language()]]),
    text = display_all_cards(state$player_names, state$cards, state$history, language()),
    html = T
  ) 
  state$n_players <- state$n_players - 1
  state$player_names <- state$player_names[-player]
  state$n_cards <- state$n_cards[-player]
}

end_game <- function(human_win, hand_exists) {
  outcome_text <- ifelse(human_win, text$human_win_true[[language()]], text$human_win_false[[language()]])
  existence_text <- ifelse(hand_exists, text$hand_exists_true[[language()]], text$hand_exists_false[[language()]])
  shinyalert(
    title = paste(existence_text, outcome_text), 
    text = display_all_cards(state$player_names, state$cards, state$history, language()),
    html = T
  )
  state$game_status <- "inactive"
}

# Action
observeEvent(input$start_button, {
  state$game_status <- "active"
  state$n_players <- 1 + as.numeric(input$game_n_ais)
  state$n_cards <- rep(1, state$n_players)
  state$player_names <- text$player_names[[language()]][1:state$n_players]
  state$current_player <- 1
  state$cards <- draw_cards(state$n_cards)
  state$max_cards <- floor(24 / state$n_players)
  state$history <- matrix(NA, nrow = 0, ncol = 4)
  
  ui_elements$last_hand_type <- "High card"
})

observeEvent(input$end_button, {
  state$game_status <- "inactive"
})

observeEvent(input$check, {
  statement <- state$history[nrow(state$history), ]
  hand_exists <- determine_hand_existence(state$cards, statement)
  cp <- state$current_player
  lp <- ifelse(cp == 1, state$n_players, cp - 1) 
  
  # If hand exists, make current player lose
  if (hand_exists) {
    if (state$n_cards[cp] == state$max_cards) end_game(human_win = F, hand_exists = T)
    if (state$n_cards[cp] < state$max_cards) give_human_a_card(hand_exists = T)
  } 
  # If hand does not exist, make previous player lose
  if (!hand_exists) {
    knock_out <- state$n_cards[lp] == state$max_cards
    if(knock_out & state$n_players == 2) end_game(human_win = T, hand_exists = F)
    if(knock_out & state$n_players >= 2) erase_player(lp)
    if(!knock_out) give_ai_a_card(lp, hand_exists = F) 
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
    shinyalert(
      title = text$impossible_move$title[[language()]], 
      text = text$impossible_move$text[[language()]]
    )
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
      
      params <- setNames(c(1, 0.3, 0.9, 0.4, 10), c("naivete_1", "naivete_2", "veracity", "cutoff", "focus"))
      action <- get_action(state$n_cards, p_cards, state$history, cp, params)
      
      if(action[2] == "check") {
        statement <- state$history[nrow(state$history), ]
        hand_exists <- determine_hand_existence(state$cards, statement)
        # If hand exists, make current player lose (do not change current player)
        if (hand_exists) {
          knock_out <- state$n_cards[cp] == state$max_cards
          if (knock_out & state$n_players == 2 & cp != 1) end_game(human_win = T, hand_exists = T)
          if (knock_out & state$n_players > 2 & cp != 1) erase_player(cp)
          if (!knock_out) give_ai_a_card(cp, hand_exists = T) 
        } 
        # If hand does not exist, make previous player lose (and switch to previous player)
        if(!hand_exists) {
          knock_out <- state$n_cards[lp] == state$max_cards
          if (knock_out & lp == 1) end_game(human_win = F, hand_exists = F)
          #if (knock_out & lp != 1 & state$n_players == 2) make_human_win(exists = F) I think this can never occur
          if (knock_out & lp != 1 & state$n_players > 2) erase_player(lp)
          if (!knock_out & lp != 1) give_ai_a_card(lp, hand_exists = F)
          if (!knock_out & lp == 1) give_human_a_card(hand_exists = F)
          state$current_player <- lp 
        } 
        deal_cards_and_reset_history()
      } else {
        proceed_normally(action)
      }
    }
  }
})