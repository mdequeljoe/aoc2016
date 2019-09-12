
d <- readLines('data/day03.txt', warn = FALSE)
invalid <- 0
for (j in seq_along(d)){
  tr <- d[j]
  tr <- trimws(gsub("\\s+", " ", tr))
  tr <- strsplit(tr, " ")[[1]]
  tr <- as.numeric(tr)
  for (i in seq_along(tr)){
    x <- tr[i]
    rem <- tr[-i]
    if (sum(rem) <= x){
      invalid <- invalid + 1
      break
    }
  }
}
length(d) - invalid

d <- readLines('data/day03.txt', warn = FALSE)
d <- trimws(gsub("\\s+", " ", d))
d <- strsplit(d, " ")
invalid <- 0
for ( i in 1:3 ){
  k <- lapply(d, `[`, i)
  k <- unlist(k)
  k <- as.numeric(k)
  for ( j in seq(1, length(k), 3) ){
    x <- k[j:(j + 2)]
    for ( h in seq_along(x) ){
      a <- x[h]
      b <- x[-h]
      if (sum(b) <= a){
        invalid <- invalid + 1
        break
      }
    }
  }
}
length(d) - invalid