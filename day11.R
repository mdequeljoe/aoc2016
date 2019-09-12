

trim_name <- function(name) gsub('(\\b[a-z]).*(\\b[a-z]).*', "\\1\\2", name)
set_items <- function(input){
  input <- strsplit(input, '\n')[[1]]
  rx <- '[a-z|-]+ microchip|[a-z|-]+ generator'
  o <- integer()
  d <- regmatches(input, gregexpr(rx, input))
  for (i in seq_along(d))
    o[trim_name(d[[i]])] <- i
  o[["E"]] <- 1
  o
}

is_balanced <- function(x)
  all(grepl('g$|E', x)) || 
  all(grepl('m$|E', x)) ||
  all(gsub('m$', 'g', x[grepl('m$', x)]) %in% x)

step_items <- function(d, x, i, j){
  a <- names(d[d == j])
  if (!is_balanced(x) || 
      !is_balanced(c(x, a)))
    return()
  d[c(x, "E")] <- j
  d
}

valid_steps <- function(d){
  level <- d["E"]
  x <- {. <- names(d[d == level])}[. != "E"]
  x2 <- if (length(x) > 1) combn(x, 2, simplify = FALSE)
  items <- c(x2, x)
  steps_up <- if (level != 4) 
    lapply(items, step_items, d = d, i = level, j = level + 1)
  
  steps_down <- if (level != 1 && length(d[d < level]) > 0)
    lapply(rev(items), step_items, d = d, i = level, j = level - 1)
  
  o <- Filter(Negate(is.null), c(steps_up, steps_down))
  rev(o)
}

completed <- function(d) all(d == 4)
as_seen <- function(s)
  paste(paste0(names(s), s), collapse = "")

min_steps <- function(d) {
  
  seen <- list()
  seen[as_seen(d)] <- TRUE
  st <- list(list(cnt = 0, value = d))
  
  repeat{
    current <- st[[length(st)]]
    st <- st[-length(st)]
    
    if (completed(current$value))
      return(current$cnt)
    
    steps <- valid_steps(current$value)
    for (s in steps){
      as <- as_seen(s)
      if (isTRUE(seen[[as]]))
        next
      seen[as_seen(s)] <- TRUE
      st[[length(st) + 1]] <-
        list(cnt = current$cnt + 1, value = s)
    }
    if (!length(st)){
      print('not found!')
      break
    }
  }
}


#example
'The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
The second floor contains a hydrogen generator.
The third floor contains a lithium generator.
The fourth floor contains nothing relevant.' -> input 
d <- set_items(input)
min_steps(d)

#part 1
'The first floor contains a strontium generator, a strontium-compatible microchip, a plutonium generator, and a plutonium-compatible microchip.
The second floor contains a thulium generator, a ruthenium generator, a ruthenium-compatible microchip, a curium generator, and a curium-compatible microchip.
The third floor contains a thulium-compatible microchip.
The fourth floor contains nothing relevant.' -> input
d <- set_items(input)
min_steps(d)

#part 2
min_steps(c(em = 1, eg = 1, dg = 1, dm = 1, d))


