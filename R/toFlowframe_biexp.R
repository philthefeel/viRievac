


# create a flowFrame and performs biexponential transformation (arcsinh)
toFlowframe_biexp = function(data, biexp = TRUE, dataframe = TRUE){

  fcsData <- as.matrix(data)
  # create meta data of the markers
  metaData <- data.frame(name = colnames(fcsData),
                         desc = paste('this is column',colnames(fcsData)))
  # calculate range of markers
  metaData$range <- apply(apply(fcsData,2,range),2,diff)
  metaData$minRange <- apply(fcsData,2,min)
  metaData$maxRange <- apply(fcsData,2,max)
  # create a flowframe
  aux <- new("flowFrame", exprs = fcsData, parameters = Biobase::AnnotatedDataFrame(metaData))

  if(biexp){
    ### transform MFI by 'biexponentatial transformation'
    biexp <- flowCore::biexponentialTransform("BETransform",
                                              a = 0.5, b = 1, c = 0.5, d = 1, f = 0, w = 0,
                                              tol = .Machine$double.eps^0.25, maxit = as.integer(5000))
    aux <- flowCore::transform(aux, flowCore::transformList(colnames(flowCore::exprs(aux)), biexp))
  }

  if(dataframe){
    # fetch transformed expression
    aux = as.data.frame(flowCore::exprs(aux))
  }

  return(aux)
}

