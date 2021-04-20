# defining the metric function
d <- function(X, Y) {
    return (length(setdiff(X, Y)) + length(union(X, Y)))
}

# defining the sets
A <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
B <- c(10, 11, 12)

# proving that symmetric axiom isn't satisfied
d(A, B) # 21
d(B, A) # 14