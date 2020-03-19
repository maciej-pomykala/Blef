all_hand_types <- c(
  "High card", "Pair", "Two pairs", "Small straight", "Big straight", "Great straight", "Three of a kind",
  "Full house", "Colour", "Four of a kind", "Small flush", "Big flush", "Great flush"
)
value_names <- c("9", "10", "J", "Q", "K", "A")
colour_names <- c("spades", "hearts", "diamonds", "clubs")
ai_names <- c("Ay Eye One", "Ay Eye Two", "Ay Eye Three", "Ay Eye Four", "Ay Eye Five")

encode_type <- function(type) {
  code <- ifelse(type == "check", 0, which(all_hand_types) == type)
  return(code)
}

decode_type <- function(code) {
  type <- ifelse(code == 0, "check", all_hand_types[code])
  return(type)
}

force_matrix <- function(table, n) {
  if(length(table) == n) {
    return(matrix(table, ncol = n))
  } else {
    return(table)
  }
}

possible_cards <- apply(expand.grid(value = 1:6, colour = 1:4), 1, function(row) 
  c(row[1], row[2])
) %>%
  t()

draw_cards <- function(n_cards) {
  all_cards <- possible_cards[sample(1:24, sum(n_cards)), ]
  player_numbers <- unlist(
    sapply(1:length(n_cards), function(p) rep(p, n_cards[p])) %>% unlist() %>% as.vector()
  )
  cbind(player_numbers, all_cards)
}

display_own_cards <- function(cards) {
  cards %<>% 
    as.data.frame() %>%
    set_colnames(c("player", "value", "colour")) %>%
    arrange(-value)
  c(
    "Your cards are the following:<br/>",
    apply(cards, 1, function(card) {
      value <- setNames(c("9", "10", "J", "Q", "K", "A"), 1:6)[card[2]]
      colour <- setNames(colour_names, 1:4)[card[3]]
      return(paste0("<li><b>", value, " ", colour, "</b><br>"))
    })
  ) %>%
    paste() %>%
    HTML()
}

display_all_cards <- function(player_names, cards, history) {
  last_item <- history[nrow(history), ]
  owners <- cards[, 1]
  
  display_p_card <- function(card) {
    value <- setNames(c("9", "10", "J", "Q", "K", "A"), 1:6)[card[2]]
    colour <- setNames(colour_names, 1:4)[card[3]]
    return(paste0(value, " ", colour))
  }
  
  display_all_p_cards <- function(p) {
    c(paste0("<br><b>", player_names[p], "</b>:"), apply(matrix(cards[owners == p, ], ncol = 3), 1, display_p_card)) %>%
      c() %>%
      paste(collapse = "<br>")
  }
  
  c(display_history_item(player_names, last_item), "The cards are the following:", lapply(1:length(player_names), display_all_p_cards)) %>%
    c() %>%
    paste(collapse = "<br>") %>%
    HTML()
}

display_n_cards <- function(player_names, n_cards) {
  HTML(
    paste(
      c(
        "<br/>Players have the following number of cards:<br/>",
        lapply(1:length(player_names), function(p) {
          player_name <- player_names[p]
          player_n_cards <- n_cards[p]
          return(paste0("<li>", player_name, ": ", player_n_cards, "<br/>"))
        })
      )
    )
  )
}

determine_hand_existence <- function(cards, statement) {
  
  hand_type <- statement[2]
  detail_1 <- as.numeric(statement[3])
  detail_2 <- as.numeric(statement[4])
  
  card_values <- cards[, 2]
  card_colours <- cards[, 3]
  
  if (hand_type == "High card") {
    outcome <- sum(card_values == detail_1) >= 1
    return(outcome)
  } else if (hand_type == "Pair") {
    outcome <- sum(card_values == detail_1) >= 2
    return(outcome)
  } else if (hand_type == "Two pairs") {
    outcome <- sum(card_values == detail_1) >= 2 & sum(card_values == detail_2) >= 2
    return(outcome)
  } else if (hand_type == "Small straight") {
    outcome <- any(card_values == 1) & any(card_values == 2) & any(card_values == 3) & any(card_values == 4) & any(card_values == 5)
    return(outcome)
  } else if (hand_type == "Big straight") {
    outcome <- any(card_values == 2) & any(card_values == 3) & any(card_values == 4) & any(card_values == 5) & any(card_values == 6) 
    return(outcome)
  } else if (hand_type == "Great straight") {
    outcome <- any(card_values == 1) & any(card_values == 2) & any(card_values == 3) & any(card_values == 4) & any(card_values == 5) & any(card_values == 6)    
    return(outcome)
  } else if (hand_type == "Three of a kind") {
    outcome <- sum(card_values == detail_1) >= 3
    return(outcome)
  } else if (hand_type == "Full house") {
    outcome <- sum(card_values == detail_1) >= 3 & sum(card_values == detail_2) >= 2
    return(outcome)
  } else if (hand_type == "Colour") {
    outcome <- sum(card_colours == detail_1) >= 5
    return(outcome)
  } else if (hand_type == "Four of a kind") {
    outcome <- sum(card_values == detail_1) >= 4
    return(outcome)
  } else if (hand_type == "Small flush") {
    relevant_values <- card_values[card_colours == detail_1]
    if (all(is.na(relevant_values))) {
      return(F)
    } else {
      outcome <- any(relevant_values == 1) & any(relevant_values == 2) & any(relevant_values == 3) & any(relevant_values == 4) & any(relevant_values == 5)
      return(outcome)
    }
  } else if (hand_type == "Big flush") {
    relevant_values <- card_values[card_colours == detail_1]
    if (all(is.na(relevant_values))) {
      return(F)
    } else {
      outcome <- any(relevant_values == 2) & any(relevant_values == 3) & any(relevant_values == 4) & any(relevant_values == 5) & any(relevant_values == 6)
      return(outcome)
    }
  } else if (hand_type == "Great flush") {
    relevant_values <- card_values[card_colours == detail_1]
    if (all(is.na(relevant_values))) {
      return(F)
    } else {
      outcome <- any(relevant_values == 1) & any(relevant_values == 2) & any(relevant_values == 3) & any(relevant_values == 4) & any(relevant_values == 5) & any(relevant_values == 6)
      return(outcome)
    }
  }
}

see_if_type_exhausted <- function(statement) {
  hand_type <- statement[2]
  detail_1 <- statement[3]
  detail_2 <- statement[4]
  if (hand_type %in% c("High card", "Pair", "Three of a kind", "Four of a kind")) {
    outcome <- ifelse(detail_1 == 6, TRUE, FALSE)
    return(outcome)
  } else if (hand_type %in% c("Two pairs", "Full house")) {
    outcome <- ifelse(detail_1 == 6 & detail_2 == 5, TRUE, FALSE)
    return(outcome)
  } else if (hand_type %in% c("Small straight", "Big straight", "Great straight")) {
    return(TRUE)
  } else if (hand_type %in% c("Colour", "Small flush", "Big flush", "Great flush")) {
    outcome <- ifelse(detail_1 == 1, TRUE, FALSE) # Define 1 to be the best colour
  }
}

see_if_legal <- function(previous, current) {
  if(which(all_hand_types == current[2]) < which(all_hand_types == previous[2])) {
    return(F)
  } else if (which(all_hand_types == current[2]) > which(all_hand_types == previous[2])) {
    hand_type <- current[2]
    if (hand_type == "Two pairs") {
      return(current[3] > current[4])
    } else if (hand_type == "Full house") {
      return(current[3] != current[4])
    } else {
      return(T)
    }
  } else {
    hand_type <- previous[2]
    if (hand_type %in% c("High card", "Pair", "Three of a kind", "Four of a kind")) {
      return(as.numeric(current[3]) > as.numeric(previous[3]))
    } else if (hand_type %in% c("Two pairs", "Full house")) {
      primary_higher <- as.numeric(current[3]) > as.numeric(previous[3])
      primary_equal <- as.numeric(current[3]) == as.numeric(previous[3])
      secondary_higher <- as.numeric(current[4]) > as.numeric(previous[4])
      return(primary_higher | (primary_equal & secondary_higher))
    } else if (hand_type %in% c("Small straight", "Big straight", "Great straight")) {
      return(FALSE)
    } else if (hand_type %in% c("Colour", "Small flush", "Big flush", "Great flush")) {
      return(as.numeric(current[3]) < as.numeric(previous[3]))
    }
  }
}

display_history_item <- function(player_names, item) {
  type <- item[2]
  detail_1 <- as.numeric(item[3])
  detail_2 <- as.numeric(item[4])
  player <- player_names[as.numeric(item[1])]
  if (type == "check") {
    action_text <- "checked"
  } else {
    base_action_text <- "bet the following: "
    if (type %in% c("Two pairs", "Full house")) {
      further_action_text <- paste(type, value_names[detail_1], value_names[detail_2], collapse = ", ")
    } else if(type %in% c("High card", "Pair", "Three of a kind", "Four of a kind")) {
      further_action_text <- paste(type, value_names[detail_1], collapse = ", ")
    } else if(type %in% c("Colour", "Small flush", "Big flush", "Great flush")) {
      further_action_text <- paste(type, colour_names[detail_1], collapse = ", ")
    } else {
      further_action_text <- type
    }
    action_text <- paste(base_action_text, further_action_text)
  }
  
  paste(player, action_text, "<br>")
}


tell_history <- function(player_names, history) {
  if (nrow(history) >= 1)
    lapply(nrow(history):1, function(i) {
      display_history_item(player_names, history[i, ])
    }) %>%
    paste() %>%
    HTML()
}