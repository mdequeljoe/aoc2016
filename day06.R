
d <- readLines('data/day06.txt', warn= FALSE)
n <- nchar(d[1])
d <- strsplit(d, '')
out <- character(n)
for (i in 1:n){
  k <- unlist(lapply(d, `[[`, i))
  k <- sort(table(k), decreasing = TRUE)
  #part 1
  #out[i] <- names(k[1])
  #part 2
  out[i] <- names(k[length(k)])
}
paste(out, collapse = "")