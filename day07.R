
d <- readLines('data/day07.txt', warn = FALSE)
valid <- logical(length(d))

has_tls <- function(s) {
  s <- strsplit(s, '')[[1]]
  for (i in seq_along(s)) {
    if (i == (length(s) - 2))
      break
    
    p <- s[i:(i + 1)]
    p2 <- s[(i + 2):(i + 3)]
    
    if (p[1] != p[2] && all(p == rev(p2)))
      return(TRUE)
  }
  FALSE
}

for (i in seq_along(d)){
  
  abba <- regmatches(d[i], gregexpr('\\[[a-z]+\\]', d[i]))[[1]]
  abba <- gsub('\\[||\\]', '', abba)
  
  al <- vapply(abba, has_tls, logical(1))
  if (any(al))
    next
  
  s <- gsub(paste(abba, collapse = "|"), "", d[i])
  s <- strsplit(s, "\\[\\]")[[1]]
  
  sl <- vapply(s, has_tls, logical(1))
  if (any(sl))
    valid[i] <- TRUE
}

length(valid[valid])

# another way
d <- readLines('data/day07.txt', warn = FALSE)
valid2 <- logical(length(d))
for (i in seq_along(d)){
  x <- d[i]
  abba_rx <- "([a-z])(?!\\1)([a-z])\\2\\1"
  bk <- gregexpr('\\[|\\]', x)[[1]]
  bk <- unlist(Map(`:`, bk[ev <- seq_along(bk) %% 2 == 0], bk[!ev]))
  abba <- gregexpr(abba_rx, x, perl = TRUE)[[1]]
  if (any(abba %in% bk))
    next
  
  if (any(abba > -1))
    valid2[i] <- TRUE
}
length(valid2[valid2])

# part 2
d <- readLines('data/day07.txt', warn = FALSE)
valid <- logical(length(d))

for (i in seq_along(d)){
  
  abba <- regmatches(d[i], gregexpr('\\[[a-z]+\\]', d[i]))[[1]]
  abba <- gsub('\\[||\\]', '', abba)
  s <- gsub(paste(abba, collapse = "|"), "", d[i])
  s <- strsplit(s, "\\[\\]")[[1]]
  
  for (j in seq_along(s)){
    
    x <- s[j]
    xs <- strsplit(x, '')[[1]]
    
    for (k in seq_along(xs)){
      
      if (k == (length(xs) - 1))
        break
      
      v <- xs[k:(k + 2)]
      if (v[1] == v[3] && 
          v[2] != v[1] &&
          any(grepl(paste0(v[2], v[1], v[2]), abba))){
        valid[i] <- TRUE
        break
      }
    }
    if (valid[i])
      break
  }
  
}
length(valid[valid])

