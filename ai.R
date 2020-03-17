index_hand <- function(type, detail_1, detail_2) {
  if(type == "High card") return(detail_1)
  if(type == "Pair") return(6 + detail_1)
  if(type == "Two pairs") return(12 + (detail_1 - 2) * (detail_1 - 1) / 2 + detail_2)
  if(type == "Three of a kind") return(27 + detail_1)
  if(type == "Small straight") return(34)
  if(type == "Big straight") return(35)
  if(type == "Great straight") return(36)
  if(type == "Colour") return(36 + detail_1)
  if(type == "Full house") return(40 + (detail_1 - 1) * 5 + detail_2 - sum(detail_2 > detail_1))
  if(type == "Four of a kind") return(70 + detail_1)
  if(type == "Small flush") return(76 + 5 - detail_1)
  if(type == "Big flush") return(80 + 5 - detail_1)
  if(type == "Great flush") return(84 + 5 - detail_1)
}

indexation <- read.csv("indexation.csv")

compute_hands_chances <- function(combinations) {
  out <- vector("numeric", 88)
  
  # HIGH CARD, PAIR, THREE OF A KIND, FOUR OF A KIND
  for(value in 1:6) {
    relevant_columns <- ((value - 1) * 4 + 1):((value - 1) * 4 + 4)
    distribution <- data.frame(stack = rowSums(combinations[, relevant_columns] > 0), chance = combinations[, 25]) %>%
      group_by(stack) %>%
      summarise(chance = sum(chance))
    out[value] <- sum(distribution$chance[distribution$stack >= 1])
    out[6 + value] <- sum(distribution$chance[distribution$stack >= 2])
    out[27 + value] <- sum(distribution$chance[distribution$stack >= 3])
    out[70 + value] <- sum(distribution$chance[distribution$stack >= 4])
  }
  
  # TWO PAIRS
  position <- 13
  for(big in 2:6) {
    for(small in 1:(big - 1)) {
      relevant_columns_small <- ((small - 1) * 4 + 1):((small - 1) * 4 + 4)
      relevant_columns_big <-   ((big   - 1) * 4 + 1):((big   - 1) * 4 + 4)
      small_reached <- rowSums(combinations[, relevant_columns_small] > 0) >= 2
      big_reached   <- rowSums(combinations[, relevant_columns_big  ] > 0) >= 2
      out[position] <- sum(combinations[small_reached & big_reached, 25])
      position <- position + 1
    }
  }
  
  # SMALL STRAIGHT, BIG STRAIGHT, GREAT STRAIGHT
  v <- list()
  for(value in 1:6) {
    relevant_columns <- ((value - 1) * 4 + 1):((value - 1) * 4 + 4)
    v[[value]] <- rowSums(combinations[, relevant_columns] > 0) >= 1
  }
  out[34] <- sum(combinations[v[[1]] & v[[2]] & v[[3]] & v[[4]] & v[[5]], 25])
  out[35] <- sum(combinations[v[[2]] & v[[3]] & v[[4]] & v[[5]] & v[[6]], 25])
  out[36] <- sum(combinations[v[[1]] & v[[2]] & v[[3]] & v[[4]] & v[[5]] & v[[6]], 25])
  
  # COLOUR
  colour_reached <- list()
  for(colour in 1:4) {
    relevant_columns <- (0:5) * 4 + colour 
    colour_reached[[colour]] <- rowSums(combinations[, relevant_columns] > 0) >= 5
    out[36 + colour] <- sum(combinations[colour_reached[[colour]], 25])
  }
  
  # FULL HOUSE
  position <- 41
  for(big in 1:6) {
    for(small in (1:6)[-big]) {
      relevant_columns_small <- ((small - 1) * 4 + 1):((small - 1) * 4 + 4)
      relevant_columns_big <-   ((big   - 1) * 4 + 1):((big   - 1) * 4 + 4)
      small_reached <- rowSums(combinations[, relevant_columns_small] > 0) >= 2
      big_reached   <- rowSums(combinations[, relevant_columns_big  ] > 0) >= 3
      out[position] <- sum(combinations[small_reached & big_reached, 25])
      position <- position + 1
    }
  }
  
  position <- 77
  # SMALL POKER, BIG POKER, GREAT POKER
  for(colour in 1:4) {
    out[76 + 0 * 4 + 5 - colour] <- sum(combinations[rowSums(combinations[, (0:4) * 4 + colour] > 0) >= 5, 25])
    out[76 + 1 * 4 + 5 - colour] <- sum(combinations[rowSums(combinations[, (1:5) * 4 + colour] > 0) >= 5, 25])
    out[76 + 2 * 4 + 5 - colour] <- sum(combinations[rowSums(combinations[, (0:5) * 4 + colour] > 0) == 6, 25])
  }
  
  return(out)
}

index_card <- function(card) (card[2] - 1) * 4 + card[3]

get_action <- function(n_cards, p_cards, history, p) {
  p_cards %<>% force_matrix(3)
  history %<>% force_matrix(4)
  random_cards <- rep(1/24, 24)
  
  base_cards <- rep(0, 24)
  own_cards <- apply(p_cards, 1, index_card)
  foreign_cards <- c(1:24)[-own_cards]
  base_cards[own_cards] <- 1
  base_cards[foreign_cards] <- rep(sum(n_cards[-p]) / length(foreign_cards), length(foreign_cards))
  
  n_own_cards <- n_cards[p]
  n_rival_cards <- n_cards[-p]

  m <- permutations(
    x = c(0, (1:length(n_cards))[-p]),
    freq = c(24 - sum(n_cards), n_cards[-p]),
    k = 24 - n_own_cards
  )
  nc <- nrow(m)
  combinations <- matrix(c(rep(0, nc * 24), rep(0, nc)), nrow = nc, ncol = 25)
  
  combinations[, foreign_cards] <- m
  combinations[, own_cards] <- p
  combinations[, 25] <- rep(1 / nc, nc)

  if(nrow(history) > 0) {
    last_move <- history[nrow(history), ]
    last_move_index <- index_hand(last_move[2], as.numeric(last_move[3]), as.numeric(last_move[4])) %>%
      as.numeric()
  } else {
    last_move_index <- 0
  }
  hand_chances <- compute_hands_chances(combinations)
  hand_chances[1:last_move_index] <- -1
  
  if(max(hand_chances) < 0.4) {
    return(c(p, "check", NA, NA))
  } else {
    hand_picked <- tail(which(hand_chances == max(hand_chances)), 1)
    hand_type <- as.character(indexation$hand_type[hand_picked])
    detail_1 <- indexation$detail_1[hand_picked]
    detail_2 <- indexation$detail_2[hand_picked]
    return(c(p, hand_type, detail_1, detail_2))
  }
}

