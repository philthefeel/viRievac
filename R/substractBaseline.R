



# substract baseline values -----------------------------------------------

substractBaseline = function(x,c,positivize=FALSE,tozero=FALSE){
  y = x-x[c]
  if(positivize) y = y+abs(min(y))
  if(tozero) y = y+abs(min(y))
  return(y)
}

