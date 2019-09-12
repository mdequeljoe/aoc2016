key <- c(17, 61)
d <- readLines('data/day10.txt', warn = FALSE)
nbots <- max(as.numeric(gsub('.*(bot) (\\d+).*', "\\2", d))) + 1
val <- d[grepl('value', d)]
bots <- vector('list', nbots + 1)
output <- list()

for (v in val){
  v <- regmatches(v, gregexpr('\\d+', v))[[1]]
  v <- as.numeric(v)
  v[2] <- v[2] + 1
  bots[[ v[2] ]] <- c(bots[[ v[2] ]], v[1])
}

check_bot <- function(bot, b,  key)
  if (all(key %in% bot[[b]])) cat('key found at', b - 1, '\n')

ins <- d[!d %in% val]
i <- 1
repeat{
  
  if (!length(ins))
    break
  
  x <- strsplit(ins[i], ' ')[[1]]
  b <- as.numeric(x[2]) + 1
  
  if (length(bots[[b]]) < 2) {
    i <- i %% length(ins) + 1
    next
  }
  
  check_bot(bots, b, key)
  bots[[b]] <- sort(bots[[b]])
  low <- as.numeric(x[7]) + 1
  high <- as.numeric(x[length(x)]) + 1
  
  if (x[6] == 'output') {
    output[[low]] <- bots[[b]][1]
  } else{
    bots[[low]] <- c(bots[[low]], bots[[b]][1])
  }
  
  if (x[11] == 'output') {
    output[[high]] <- bots[[b]][2]
  } else {
    bots[[high]] <- c(bots[[high]], bots[[b]][2])
  }
  
  bots[[b]] <- numeric(0)
  
  check_bot(bots, low, key)
  check_bot(bots, high, key)
  
  ins <- ins[-i]
  
}

#part 2
Reduce(`*`, output[1:3])