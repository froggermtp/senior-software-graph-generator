do <- function(txt) {
  paste(txt, '!', sep="")
}

foo <- function(f) {
  f('bar')
}

apply <- function(vec, f) {
  out = c()
  
  for(ii in 1:length(vec)) {
    out[ii] <- f(vec[ii])
  }
  
  out
}

apply2 = function(vec, f, out=c()) {
  index = length(out) + 1
  out[index] = f(vec[1])
  
  if(!is.na(vec[2:length(vec)][1])) {
    apply2(vec[2:length(vec)], f, out)
  }
  else {
    out
  }
}

times2 <- function(x) {
  2 * x
}

apply(c(1, 2, 3), times2)

apply2(c(1, 2, 3), times2)

count <- 0

repeat {
  print(foo(do))
  count <- count + 1
  
  if(count == 5) {
    break
  }
}