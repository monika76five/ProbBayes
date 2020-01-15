# chapter 2 code

# Section 2.2
# sampling from a box

box <- c("red", "red", "red", "blue", "white")
sample(box, size = 3, replace = FALSE)

# Section 2.4
# permutations
require(stringr)
permutation <- function(d, Size){
  str_flatten(sample(d, size=Size),
              collapse = " ")
}
songs <- c("Song A", "Song B", "Song C", "Song D",
           "Song E", "Song F")
permutation(songs, Size = 2)

# Section 2.5
# combinations

Numbers <- c(1, 2, 3, 4, 5)
all_combo <- t(combn(Numbers, 2))
all_combo

N <- nrow(all_combo)
df <- data.frame(Iter = 1:500,
                 Balls = all_combo[sample(N, size = 500,
                                    replace = TRUE), ])
df %>% group_by(Balls.1, Balls.2) %>% count()

# Section 2.6
# arranging non-distinct objects

require(stringr)
permutation <- function(d, Size){
  str_flatten(sample(d, size=Size),
              collapse = " ")
}
objects <- c('x', 'x', 'x', 'o', 'o')
df <- data.frame(Iter = 1:1000,
              Arrangement = replicate(1000,
                  permutation(objects, 5)))
df %>% group_by(Arrangement) %>% count()

# Section 2.7
# Yahtzee

four_kind <- function(){
  rolls <- sample(6, size = 5, replace = TRUE)
  ifelse(max(table(rolls) == 4),
         "4 kind", "nothing")
}

df <- data.frame(Iter = 1:1000,
            Result = replicate(1000, four_kind()))
df %>% group_by(Result) %>% count()

