


# Phenograph function (returns a vector of clusters, length equal to the number of rows entered)
viRPhenograph = function(data,params,k=30){

  # Finding clusters in the scaled data using Phenograph
  aux <- Rphenograph::Rphenograph(data %>% dplyr::select(params), k = k)
  louvain_phenograph <- igraph::membership(aux[[2]])

  # Add cluster annotation/cell to original unscaled data matrix with tSNE axis
  clusters = as.factor(louvain_phenograph)

  return(clusters)

}
