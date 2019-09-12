

d <- readLines('data/day02.txt', warn = FALSE)
code <- numeric(5)
dir <- list(U = c(-1, 0), D = c(1, 0), R = c(0, 1), L = c(0, -1))
keypad <- t(matrix(1:9, nrow = 3))
m <- c(2,2)
for (i in seq_along(code)){
  key <- strsplit(d[i], "")[[1]]
  for (k in seq_along(key)){
    m_ <- m + dir[[ key[k] ]]
    if (m_[1] >= 1 &&
        m_[1] <= 3 && 
        m_[2] >= 1 && 
        m_[2] <= 3 ){
      m <- m_
    } 
  }
  code[i] <- keypad[m[1], m[2]]
}
paste(code, collapse = "")

code <- numeric(5)
keypad <- matrix("", nrow = 5, ncol = 5)
keypad[1, 3] <- 1
keypad[2, 2:4] <- 2:4
keypad[3, ] <- 5:9
keypad[4, 2:4] <- c("A", "B", "C")
keypad[5, 3] <- "D"
m <- c(3, 1)
for (i in seq_along(code)){
  key <- strsplit(d[i], "")[[1]]
  for (k in seq_along(key)){
    m_ <- m + dir[[ key[k] ]]
    val <- try(keypad[m_[1], m_[2]], silent = TRUE)
    if (!inherits(val, 'try-error') && isTRUE(nzchar(val))){
      m <- m_
    } 
  }
  code[i] <- keypad[m[1], m[2]]
}
paste(code, collapse = "")
