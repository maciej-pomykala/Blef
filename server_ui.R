output$language_button <- renderUI({
  if (state$game_status == "inactive") {
    if (language() == "English") {
      actionButton(inputId = "switch_to_polish", label = "Polski", width = 72)
    } else if (language() == "Polish") {
      actionButton(inputId = "switch_to_english", label = "English", width = 72)
    }
  }
})

observeEvent(input$switch_to_polish, {
  language("Polish")
})

observeEvent(input$switch_to_english, {
  language("English")
})

observeEvent(input$help, {
  shinyalert(
    title = text$instructions$title[[language()]],
    text = text$instructions$text[[language()]],
    type = "info"
  )
})

output$game_button <- renderUI({
  if (state$game_status == "active") {
    actionButton(inputId = "end_button", label = text$end_game[[language()]])
  } else if (state$game_status == "inactive") {
    actionButton(inputId = "start_button", label = text$start_game[[language()]])
  }
})

# Inactive game UI
output$n_ais <- renderUI({
  if(state$game_status == "inactive")
    tagList(
      br(),
      selectInput(inputId = "game_n_ais", label = text$n_ais[[language()]], choices = 1:5, selected = 1)
    )
})

# Active game UI
observeEvent(state$cards, {
  output$display_own_cards <- renderUI({
    c(
      text$your_cards[[language()]],
      apply(state$cards[state$cards[, 1] == 1, ], 1, function(card) {
        value <- setNames(c("9", "10", "J", "Q", "K", "A"), 1:6)[card[2]]
        colour <- setNames(colour_names, 1:4)[card[3]]
        return(paste0("<li><b>", value, " ", colour, "</b><br>"))
      })
    ) %>%
      paste() %>%
      HTML()
  })
})

output$display_n_cards <- renderUI({
  c(
    text$players_cards[[language()]],
    lapply(1:length(state$player_names), function(p) {
      player_name <- state$player_names[p]
      player_n_cards <- state$n_cards[p]
      return(paste0("<li>", player_name, ": ", player_n_cards, "<br/>"))
    })
  ) %>%
    paste() %>%
    HTML()
})

output$cards <- renderUI({
  if(state$game_status == "active") {
    tagList(
      big_text(text$cards_in_play[[language()]]),
      htmlOutput("display_own_cards"),
      htmlOutput("display_n_cards")
    )
  }
})

observeEvent(state$history, {
  if (nrow(state$history) > 0) {
    ui_elements$check_button <- actionButton(inputId = "check", label = text$check[[language()]])
  } else {
    ui_elements$check_button <- NULL
  }
})

observeEvent(state$game_status, {
  ui_elements$hand_type <- radioButtons(
    inputId = "hand_type", 
    label = text$hand_type[[language()]], 
    choiceValues = all_hand_types,
    choiceNames = text$hand_types[[language()]],
    selected = ui_elements$last_hand_type,
    inline =  TRUE, 
    width = 720
  )
})

output$move_1 <- renderUI({
  if(state$game_status == "active") {
    tagList(
      big_text(text$your_move[[language()]]),
      actionButton(inputId = "hand_confirm", label = text$confirm_hand[[language()]]),
      ui_elements$check_button,
      br(),
      br(),
      ui_elements$hand_type
    )
  }
})

observeEvent(input$hand_type, {
  if (input$hand_type %in% c("High card", "Pair", "Three of a kind", "Four of a kind")) {
    ui_elements$hand_detail_1 <- radioButtons(
      inputId = "hand_detail_1", 
      label = text$card_value[[language()]], 
      choiceNames = value_names, 
      choiceValues = 1:6,
      inline =  TRUE
    )
    ui_elements$hand_detail_2 <- NULL
  } else if (input$hand_type %in% c("Two pairs", "Full house")) {
    ui_elements$hand_detail_1 <- radioButtons(
      inputId = "hand_detail_1", 
      label = ifelse(input$hand_type == "Two pairs", text$higher_pair_value[[language()]], text$three_cards_value[[language()]]), 
      choiceNames = value_names, 
      choiceValues = 1:6,
      inline =  TRUE
    )
    ui_elements$hand_detail_2 <- radioButtons(
      inputId = "hand_detail_2", 
      label = ifelse(input$hand_type == "Two pairs", text$lower_pair_value[[language()]], text$two_cards_value[[language()]]), 
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
      label = text$colour[[language()]], 
      choiceNames = colour_names, 
      choiceValues = 1:4,
      inline =  TRUE
    )
    ui_elements$hand_detail_2 <- NULL
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

# History ----
observeEvent(state$history, {
  
  output$history <- renderUI({
    if(state$game_status == "active") {
      tagList(
        big_text(text$history[[language()]]),
        renderUI({tell_history(state$player_names, state$history, language())})
      )
    }
  })
})
