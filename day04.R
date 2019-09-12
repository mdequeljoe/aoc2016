
d <- readLines('data/day04.txt', warn = FALSE)
s <- 0
for (room in d){
  cs <- gsub(".+\\[([a-z]+)\\]$", "\\1", room)
  id <- as.numeric(gsub('[^0-9+]', "", room))
  nm <- gsub(cs, "", room)
  nm <- gsub("[^a-z]", "", room)
  nm <- strsplit(nm, '')[[1]]
  cnt <- table(nm)
  cnt <- sort(cnt, decreasing = TRUE)
  cs <- strsplit(cs, '')[[1]]
  if (all(names(cnt[1:5]) %in% cs))
    s <- s + id
}
s


# part 2
d <- readLines('data/day04.txt', warn = FALSE)
rid <- numeric(0)
rnames <- character(0)
for (room in d){
  cs <- gsub(".+\\[([a-z]+)\\]$", "\\1", room)
  id <- as.numeric(gsub('[^0-9+]', "", room))
  rid[length(rid) + 1] <- id
  nm <- gsub(cs, "", room)
  nm <- gsub("[^a-z]", "", nm)
  nm <- strsplit(nm, '')[[1]]
  for (i in seq_along(nm)){
    x <- which(letters == nm[i])
    for (j in 1:id)
      x <- x %% 26 + 1
    nm[i] <- letters[x]
  }
  rnames[length(rnames) + 1] <- paste(nm, collapse = "")
}

v <- rnames[grepl("north", rnames)]
rid[which(rnames == v)]
