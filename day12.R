
d <- readLines('data/day12.txt', warn = FALSE)
d <- strsplit(d, '\\s')
'
cpy x y copies x (either an integer or the value of a register) into register y.
inc x increases the value of register x by one.
dec x decreases the value of register x by one.
jnz x y jumps to an instruction y away (positive means forward; 
negative means backward), but only if x is not zero.
'
nm <- letters[1:4]
#part 1
rg <- setNames(vector('integer', 4), nm)
#part two
rg['c'] <- 1
i <- 1
repeat{
  if (i > length(d))
    break
  x <- d[[i]]
  if (x[1] == "cpy"){
    v <- if (x[2] %in% nm) rg[ x[2] ] else as.numeric(x[2])
    rg[ x[3] ] <- v
    i <- i + 1
  }
  else if (x[1] == "inc"){
    rg[ x[2] ] <- rg[ x[2] ] + 1
    i <- i + 1
  }
  else if (x[1] == 'dec'){
    rg[ x[2] ] <- rg[ x[2] ] - 1
    i <- i + 1
  }
  else {
    v <- if (x[2] %in% nm) rg[ x[2] ] else as.numeric(x[2])
    i <- i + if (v != 0) as.numeric(x[3]) else 1
  }
}
rg
