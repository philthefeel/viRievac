


# Fold change -------------------------------------------------------------


foldChange  = function(x,c,block = FALSE){
  y = x/x[c]
  if(block) y = 100-(y*100)
  return(y)
}
