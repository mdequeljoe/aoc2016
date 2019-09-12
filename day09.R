
d <- readLines('data/day09.txt', warn = FALSE)

nk_decomp <- function(s){
  nk <- 0
  i <- 1
  n <- nchar(s)
  repeat{
    
    if (i > n)
      break
    
    if (substring(s, i, i)  == "("){
      v <- sub('\\(([0-9]+)x([0-9]+)\\).*', "\\1 \\2", substring(s, i, n))
      vlen <- nchar(v) + 2
      v <- as.numeric(strsplit(v, ' ')[[1]])
      nk <- nk + v[1] * v[2]
      i <- i + vlen + v[1]
      next
    } 
    nk <- nk + 1
    i <- i + 1
  }
  nk
}

nk_decomp('ADVENT') == 6
nk_decomp(s <- 'A(1x5)BC') == 7
nk_decomp('(3x3)XYZ')
nk_decomp(s <-'A(2x2)BCD(2x2)EFG')
nk_decomp('(6x1)(1x3)A') 
nk_decomp(s <- 'X(8x2)(3x3)ABCY')
nk_decomp(d)



#part 2

nchar_decomp <- function(s){
  
  nk <- 0
  rx <- '\\([0-9]+x[0-9]+\\)'
  repeat{
    
    m <- regexpr(rx, s)
    if (m < 0)
      break
    mlen <- attr(m, 'match.length')
    
    nk <- nk + nchar(substring(s, 1, m - 1))
    s <- substring(s, m, nchar(s))
    m <- 1
    
    v <- gsub('\\(|\\)', '', substr(s, m, m + mlen - 1))
    v <- as.numeric(strsplit(v, 'x')[[1]])
    
    rng <- substring(s, m + mlen, m + mlen + v[1] - 1)
    if (grepl(rx, rng))
      nk <- nk + v[2] * nchar_decomp(rng)
    else nk <- nk + v[1] * v[2]
    
    s <- substring(s, m + mlen + v[1], nchar(s))
    
  }
  nk + nchar(s)
}

nchar_decomp(s <- '(3x3)XYZ') == 9
nchar_decomp(s <- 'X(8x2)(3x3)ABCY') == 20
nchar_decomp('(27x12)(20x12)(13x14)(7x10)(1x12)A') == 241920
nchar_decomp('(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN') == 445
nchar_decomp(d)