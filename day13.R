
is_wall <- function(x, y, n){
  v <- x * x + 3 * x + 2 * x * y + y + y * y
  v <- v + n
  v <- R.utils::intToBin(v)
  nchar(gsub('0', '', v)) %% 2 != 0
}

as_seen <- function(coord) 
  paste('x', coord[1], 'y', coord[2], sep = '', collapse = '')

valid_steps <- function(coords, n){
  dir <- list(
    c(-1, 0), c(1, 0), c(0, -1), c(0, 1)
  )
  o <- list()
  for (d in dir){
    v <- d + coords
    if (any(v < 0) || is_wall(v[1], v[2], n))
      next
    o[[length(o) + 1]] <- v
  }
  o
}

min_steps <- function(start.x, start.y, end.x, end.y, n) {
  start <- c(start.x, start.y)
  end <- c(end.x, end.y)
  qq <- list(list(cnt = 0, coord = start))
  seen <- list()
  seen[[as_seen(start)]] <- TRUE
  repeat {
    if (!length(qq))
      break
    k <- qq[[length(qq)]]
    qq <- qq[-length(qq)]
    
    if (identical(k$coord, end))
      return(k$cnt)
    
    steps <- valid_steps(k$coord, n)
    for (s in steps) {
      as <- as_seen(s)
      if (isTRUE(seen[[as]]))
        next
      seen[[as]] <- TRUE
      qq <- c(list(list(
        cnt = k$cnt + 1, coord = s
      )), qq)
    }
  }
}

#example
input <- 10
min_steps(1, 1, 7, 4, n = input)

#part 1
input <- 1350
min_steps(1, 1, 31, 39, n = input)

#part 2
n_steps <- function(start.x, start.y, n) {
  start <- c(start.x, start.y)
  qq <- list(list(cnt = 0, coord = start))
  seen <- list()
  seen[[as_seen(start)]] <- TRUE
  
  repeat {
    if (!length(qq))
      break
    k <- qq[[length(qq)]]
    qq <- qq[-length(qq)]
    
    if (k$cnt == 50)
      next 
    
    steps <- valid_steps(k$coord, n)
    for (s in steps) {
      as <- as_seen(s)
      if (isTRUE(seen[[as]]))
        next
      seen[[as]] <- TRUE
      qq <- c(list(list(
        cnt = k$cnt + 1, coord = s
      )), qq)
    }
  }
  length(seen)
}
n_steps(1, 1, input)
