d <- readLines('data/day08.txt', warn = FALSE)
m <- matrix(".", nrow = 6, ncol = 50)

shift <- function(v, offset)
  v[(seq_along(v) - offset - 1) %% length(v) + 1]

for (i in seq_along(d)){
  
  x <- strsplit(d[i], ' ')[[1]]
  
  if (x[1] == "rect"){
    
    dim <- strsplit(x[2], "x")[[1]]
    rw <- as.numeric(dim[2])
    cl <- as.numeric(dim[1])
    m[1:rw, 1:cl] <- "#"
    next
  }  
  
  k <- strsplit(x[3], "=")[[1]]
  k <- as.numeric(k[2]) + 1 #coords start at 0 in input
  offset <- as.numeric(x[5])
  if (x[2] == 'row')
    m[k, ] <- shift(m[k, ], offset)
  else
    m[, k] <- shift(m[, k], offset)
  
}

length(m[m == "#"])

# part 2
m <- apply(m, 1, paste, collapse = "")
m <- gsub("\\.", " ", m)
cat(m, sep = '\n')

# EOARGPHYAO