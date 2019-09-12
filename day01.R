
d <- readLines('data/day01.txt', warn = FALSE)
d <- strsplit(d, ",")[[1]]

dist <- function(x) abs(x[1]) + abs(x[2])
step_by <- function(n, i)
  list(
    N = c(0, n),
    W = c(-n, 0),
    S = c(0, -n),
    E = c(n, 0)
  )[[i]]

#part 1
k <- c(0, 0)
cdir <- 1
for (step in d){
  dir <- if (grepl('L', step)) 'L' else 'R'
  val <- as.numeric(gsub('L|R', '', step))
  cdir <- if (dir == 'L') cdir %% 4 + 1 else (cdir + 2) %% 4 + 1
  k <- k + step_by(val, cdir)
}
dist(k)

#part 2
k <- c(0, 0)
cdir <- 1
seen <- list(k)
for (step in d){
  dir <- if (grepl('L', step)) 'L' else 'R'
  val <- as.numeric(gsub('L|R', '', step))
  cdir <- if (dir == 'L') cdir %% 4 + 1 else (cdir + 2) %% 4 + 1
  for (i in seq(val))
    seen[[length(seen) + 1]] <- k + step_by(i, cdir)
  k <- k + step_by(val, cdir)
}
seen[[ anyDuplicated(seen)[1] ]] -> v
dist(v)