# Load data
setwd('C:/Users/zacha/OneDrive/Documents/Spring2020/Consem/ref_reports')
data <- read.csv('choice_data.csv')

# Get just the ranking columns
choices <- data[5:length(data)]
# Store names so we can map back to them later
name_map <- colnames(choices)

# convert choice data to numeric matrix
choices <- data.matrix(choices)

# Translate Nico's preferences (he only ranked up to 3)
choices[,1][choices[,1]==3] <- 4
choices[,1][choices[,1]==2] <- 3

# function to calculate match quality of a pair
match_qual <- function(stu_1, stu_2) {
  mean_match <- (stu_1 + stu_2)/2
  mean_match[is.na(mean_match)] <- 0
  return(max(mean_match)) 
      # This returns the mean ranking of the pair's best match
}

# get mean top-rank for all possible pairs
pair_quality <- matrix(nrow= (name_map), ncol=length(name_map))
for (i in 1:length(name_map)) {
  for (j in i:length(name_map)) { # can start with i because matrix will be symmetric
    pair_quality[i,j] <- match_qual(choices[,i], choices[,j])
    pair_quality[j,i] <- pair_quality[i,j]
  }
} # so pair_quality[i,j] is the mean ranking of pair (i,j)'s best match

#################################################
### get all possible sets of pairings ###########
#################################################

# pairs are only eligible if
    # 1) they have at least 1 matching paper
    # 2) they aren't a self-match
possible_pairs <- list()
idx <- 1

for (i in 1:length(name_map)) {
  for (j in i:length(name_map)) {
    if ((pair_quality[i,j] > 0) & (i!= j)) {
      possible_pairs[[idx]] <- c(i,j)
      idx <- idx + 1
    }
  }
}

# for each person, get vector of indices for pairs they are in
idx_map <- matrix(nrow=length(name_map), ncol=length(possible_pairs))
for (i in 1:length(name_map)) {
  for (j in 1:length(possible_pairs)) {
    idx_map[i,j] <- i %in% possible_pairs[[j]]
  }
}

# function to get pairs with people who haven't been matched yet
remaining_pairs <- function(current_pairs, super_pairs) {
  p <- unlist(current_pairs)
  remain <- list()
  i <- 1
  for (pair in super_pairs) {
    if (max(p %in% pair) == 0) {
      remain[[i]] <- pair
      i <- i+1
    }
  }
  return(remain)
}

possible_sets <- list()
idx <- 1
# This could be sped up by logging which pairs have been parents
# and skipping them, but our n is small enough here that it doesn't
# matter
for (pair in possible_pairs) {
  set <- list()
  set[[1]] <- pair
  remainder <- remaining_pairs(set, possible_pairs)
  #print(remainder)
  if (length(remainder) > 0) {
    for (pair2 in remainder) {
      set[[2]] <- pair2
      remainder2 <- remaining_pairs(set, remainder)
      #print(remainder2)
      if (length(remainder2) > 0) {
        for (pair3 in remainder2) {
          set[[3]] <- pair3
          remainder3 <- remaining_pairs(set, remainder2)
          if (length(remainder3) > 0) {
            for (pair4 in remainder3) {
              set[[4]] <- pair4
              possible_sets[[idx]] <- set
              idx <- idx + 1
            }
          }
        }
      }
    }
  }
}

# function to score a set (mean pair-quality of set)
score_set <- function(set) {
  pair_qualities <- rep(NA, length(set))
  for (i in 1:length(set)) {
    pair_qualities[i] <- pair_quality[set[[i]][1], set[[i]][2]]
  }
  return(mean(pair_qualities))
}

# score all eligible sets of pairs
set_scores <- rep(NA, length(possible_sets))
for (i in 1:length(possible_sets)) {
  set_scores[i] <- score_set(possible_sets[[i]])
}

# get minimum match quality in a set (for tie-breaking)
min_match_in_set <- function(set) {
  pair_qualities <- rep(NA, length(set))
  for (i in 1:length(set)) {
    pair_qualities[i] <- pair_quality[set[[i]][1], set[[i]][2]]
  }
  return(min(pair_qualities))
}

# find best sets by mean pair-quality
best_set_idx <- which(set_scores==max(set_scores))
best_sets <- possible_sets[best_set_idx]

# find minimums of best sets
best_set_mins <- rep(NA, length(best_sets))
for (i in 1:length(best_sets)) {
  best_set_mins[i] <- min_match_in_set(best_sets[[i]])
}

best_of_best_idx <- which(best_set_mins==max(best_set_mins))

# Assert that the tied sets are permutations of the same set
same_sets <- TRUE
for (i in 2:length(best_sets)) {
  if (!setequal(best_sets[[1]], best_sets[[i]])) {
    same_sets <- FALSE
  }
}

if (same_sets==TRUE) {
  best_set_idx = best_set_idx[1]
}

best_set <- possible_sets[[best_set_idx]]

############################################################
#### Extract info about best possible set of pairs##########
############################################################

# Map pair numbers to names
best_pairs <- list()
for (i in 1:length(best_set)) {
  named_pair <- c(name_map[best_set[[i]][1]], name_map[best_set[[i]][2]])
  best_pairs[[i]] <- named_pair
}

# Find the best matching papers for each pair
top_papers <- list()
i <- 1
for (pair in best_set) {
  pair_qual_scores <- match_qual(choices[,pair[1]], choices[,pair[2]])
  qual_by_paper <- (choices[,pair[1]] + choices[,pair[2]])/2
  qual_by_paper[is.na(qual_by_paper)] <- 0
  best_papers_idx <- which(qual_by_paper==pair_qual_scores)
  top_papers[[i]] <- data[best_papers_idx,1]
  i <- i + 1
}

pairs_with_papers <- list()
for (i in 1:length(best_pairs)) {
  pair_with_papers <- list()
  pair_with_papers[[1]] <- best_pairs[[i]]
  pair_with_papers[[2]] <- top_papers[[i]]
  pairs_with_papers[[i]] <- pair_with_papers
}

for (i in 1:length(pairs_with_papers)) {
  names <- pairs_with_papers[[i]][[1]]
  print(paste0('Pair ', i, ': ', names[1], ' and ', names[2]))
  cat('\tTop papers for this group: \n')
  for (paper in pairs_with_papers[[i]][[2]]) {
    cat(paste0('\t \t', paper, '\n'))
  }
}
