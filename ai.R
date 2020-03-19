index_hand <- function(type, detail_1, detail_2) {
  detail_1 %<>% as.numeric()
  detail_2 %<>% as.numeric()
  if(type == "High card") return(detail_1)
  if(type == "Pair") return(6 + detail_1)
  if(type == "Two pairs") return(12 + (detail_1 - 2) * (detail_1 - 1) / 2 + detail_2)
  if(type == "Small straight") return(28)
  if(type == "Big straight") return(29)
  if(type == "Great straight") return(30)
  if(type == "Three of a kind") return(30 + detail_1)
  if(type == "Full house") return(36 + (detail_1 - 1) * 5 + detail_2 - sum(detail_2 > detail_1))
  if(type == "Colour") return(66 + detail_1)
  if(type == "Four of a kind") return(70 + detail_1)
  if(type == "Small flush") return(76 + 5 - detail_1)
  if(type == "Big flush") return(80 + 5 - detail_1)
  if(type == "Great flush") return(84 + 5 - detail_1)
}

indexation <- read.csv("indexation.csv")

get_bet_by_number <- function(p, bet_number) {
  hand_type <- as.character(indexation$hand_type[bet_number])
  detail_1 <- indexation$detail_1[bet_number]
  detail_2 <- indexation$detail_2[bet_number]
  return(c(p, hand_type, detail_1, detail_2))
}

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
    out[30 + value] <- sum(distribution$chance[distribution$stack >= 3])
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
  out[28] <- sum(combinations[v[[1]] & v[[2]] & v[[3]] & v[[4]] & v[[5]], 25])
  out[29] <- sum(combinations[v[[2]] & v[[3]] & v[[4]] & v[[5]] & v[[6]], 25])
  out[30] <- sum(combinations[v[[1]] & v[[2]] & v[[3]] & v[[4]] & v[[5]] & v[[6]], 25])
  
  # FULL HOUSE
  position <- 37
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
  
  # COLOUR
  colour_reached <- list()
  for(colour in 1:4) {
    relevant_columns <- (0:5) * 4 + colour 
    colour_reached[[colour]] <- rowSums(combinations[, relevant_columns] > 0) >= 5
    out[66 + colour] <- sum(combinations[colour_reached[[colour]], 25])
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

# random_chance_list <- lapply(15:23, function(i) { # Necessary to do it in chunks so the code doesn't explode the machine
#   m <- permutations(
#     x = c(0, 1),
#     freq = c(24 - i, i),
#     k = 24
#   )
#   m <- matrix(c(m, rep(1 / nrow(m), nrow(m))), nrow = nrow(m), ncol = 25)
#   compute_hands_chances(m)
# }) %>%
#   unlist() %>%
#   matrix(ncol = 88, byrow = T) %>%
#   as.data.frame()
# write_csv(rbind(read_csv("indexation.csv"), random_chance_list), "indexation.csv")

random_chance_table <- read_csv("random_chances.csv")

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

  # Compute chances of each card distribution occurring
  if(sum(n_cards) == 24) {
    hand_chances <- rep(1, 88)
  } else {
    m <- permutations(
      x = c(0, length(n_cards) + 1),
      freq = c(24 - sum(n_cards), sum(n_cards[-p])),
      k = 24 - n_own_cards
    )
    nc <- nrow(m)
    combinations <- matrix(c(rep(0, nc * 24), rep(0, nc)), nrow = nc, ncol = 25)
    combinations[, foreign_cards] <- m
    combinations[, own_cards] <- p
    combinations[, 25] <- rep(1 / nc, nc)
    hand_chances <- compute_hands_chances(combinations)
  }
  
  # Compute chances of each card distribution occurring at random
  if (sum(n_cards) == 24) random_chances <- rep(1, 88)
  if (sum(n_cards) != 24) random_chances <- random_chance_table[sum(n_cards), ]
  
  # Compute which moves are illegal
  if(nrow(history) > 0) {
    last_move <- history[nrow(history), ]
    last_move_index <- index_hand(last_move[2], as.numeric(last_move[3]), as.numeric(last_move[4])) %>%
      as.numeric()
    illegal_moves <- 1:last_move_index
  } else {
    illegal_moves <- NULL
  }
  
  veracity <- 0.7
  balanced_chances <- (1 - veracity) * random_chances + veracity * hand_chances
  balanced_chances[illegal_moves] <- -10

  bc <- balanced_chances
  
  if(max(bc) < 0.4) {
    return(c(p, "check", NA, NA))
  } else {
    # try if we can go to a bigger bet worth >40% that prevents opponents from doing anything worth >15%
    block <- F
    first <- tail(which(bc == max(bc)), 1)
    if (first != 88) {
      second <- tail(which(bc == max(bc[(first + 1): 88])), 1)
      if (second != 88) {
        third <- tail(which(bc == max(bc[(second + 1): 88])), 1)
        block <- bc[second] >= (bc[first] - 0.1) & bc[second] > 0.5 & bc[third] <= 0.25
      }
    }
    
    if(block) {
      hand_picked <- second
    } else {
      hand_picked <- which(bc == max(bc))
      if (length(hand_picked) > 1) hand_picked %<>% sample(1)
    }
    
    return(get_bet_by_number(p, hand_picked))
  }
}

