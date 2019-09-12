
library(digest)

get_pass <- function(key, len = 8) {
  pass <- character(0)
  m <- 0
  n <- 1e6
  repeat {
    if (length(pass) >= len)
      break
    input <- sprintf('%s%d', key, m:n) 
    for (x in input){
      d <- digest(x, serialize = FALSE)
      if (substr(d, 1, 5) == '00000') {
        pass[length(pass) + 1] <- substr(d, 6, 6)
        print(pass)
      }
      if (length(pass) >= len)
        break
    }
    m <- n
    n <- n + 2e6
  }
  paste(pass, collapse = '')
}

get_pass('ugkcyxxp')


get_pass2 <- function(key, len = 8) {
  pass <- character(8)
  i <- 0
  n <- 2e6
  m <- 0
  repeat {
    if (m == len)
      break
    input <- paste0(key, i:n)
    for (x in input) {
      d <- digest(x, serialize = FALSE)
      
      if (grepl('^0{5}', d)) {
        p <- substr(d, 6, 6)
        if (p %in% letters)
          next
        p <- as.numeric(p) + 1
        if (p < 1 || p > 8)
          next
        if (!nzchar(pass[p])) {
          pass[p] <- substr(d, 7, 7)
          m <- m + 1
        }
        print(pass)
      }
    }
    i <- n
    n <- n + 2e6
  }
  paste(pass, collapse = "")
}

get_pass2('ugkcyxxp')

