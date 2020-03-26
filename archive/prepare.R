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
# write_csv(rbind(read_csv("random_chances.csv"), random_chance_list), "random_chances.csv")
# 
# progressions <- lapply(19:23, function(i) { # Necessary to do it in chunks so the code doesn't explode the machine
#   m <- permutations(
#     x = c(0, 1),
#     freq = c(24 - i, i),
#     k = 24
#   )
#   m <- matrix(c(m, rep(1 / nrow(m), nrow(m))), nrow = nrow(m), ncol = 25)
#   chances <- compute_detailed_chances(m)
#   progressions <- vector("numeric", 88)
#   for(j in 1:87) 
#     progressions[j] <- chances[, (25 + j):(25 + 88)] %>%
#       rowSums() %>%
#       is_greater_than(0) %>%
#       sum() %>%
#       divide_by(nrow(chances))
#   progressions[88] <- sum(chances[, 25 + 88] > 0) / nrow(chances)
#   return(progressions)
# }) %>%
#   unlist() %>%
#   matrix(ncol = 88, byrow = T) %>%
#   as.data.frame()
# write_csv(rbind(read_csv("progressions.csv"), progressions), "progressions.csv")
