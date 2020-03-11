


# import FlowJo files
importFlowJo = function(path,pattern='*.csv$',recursive=T,full.names=T,params,new.params=NULL){

  # Search and Import fcs exported single cell data:
  fileLS <- list.files(path = path,
                       pattern = pattern,
                       full.names = full.names,
                       recursive = recursive)

  if(is.null(params)) params = colnames(flowLS_temp)

  # import single cell data:
  flowLS_temp <- lapply(fileLS, function(FILE) {

    fileDF <- data.table::fread(FILE) %>% as.data.frame()
    fileDF = fileDF[,names(fileDF)[grep(paste0(params,collapse='|'),names(fileDF))]]
    fileDF$fcs_file = basename(FILE) %>% stringr::str_remove('.csv')
    cols = params[params%in%names(fileDF)]
    fileDF = fileDF %>% select(fcs_file,cols)

    if(!is.null(new.params)){
      colnames(fileDF) = c('fcs_file',new.params)}
    else{
      colnames(fileDF) = c('fcs_file',params)
    }
    return(fileDF)
  }) %>% bind_rows()

  return(flowLS_temp)

}
