


# list of possible colours for tSNE clusters:


color_cluster = function(n){

  colors30 <- c("#DC050C", "#FB8072", "#1965B0", "#7BAFDE", "#882E72",
                "#B17BA6", "#FF7F00", "#FDB462", "#E7298A", "#E78AC3",
                "#33A02C", "#B2DF8A", "#55A1B1", "#8DD3C7", "#A6761D",
                "#E6AB02", "#7570B3", "#BEAED4", "#666666", "#999999",
                "#aa8282", "#d4b7b7", "#8600bf", "#ba5ce3", "#808000",
                "#aeae5c", "#1e90ff", "#00bfff", "#56ff0d", "#ffff00")
  colors37 = c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d",
               "#5a51dc","#d49f36","#552095","#507f2d","#db37aa","#84b67c",
               "#a06fda","#df462a","#5b83db","#c76c2d","#4f49a3","#82702d",
               "#dd6bbb","#334c22","#d83979","#55baad","#dc4555","#62aad3",
               "#8c3025","#417d61","#862977","#bba672","#403367","#da8a6d",
               "#a79cd4","#71482c","#c689d0","#6b2940","#d593a7","#895c8b","#bd5975")
  total = toupper(c(colors30,colors37))

  if(n>length(total)) cols = sample(total,n,replace = T) else cols = sample(total,n,replace=F)

  return(cols)

}

