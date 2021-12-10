


# Sample and scales cells from imported files
sampleCells = function(data,fcsName.var='fcs_file',params,mincells=1000,
                       minimize=FALSE,maxcells=max(50000,nrow(data)*0.1),
                       keepFileName=TRUE,seed = 1985){

  if(dplyr::n_distinct(data[,fcsName.var])==1){

    aux = data %>% dplyr::select(fcsName.var,params) %>% sample_n(mincells) %>% na.omit()

    cat('sampling',mincells,'cells\n')

  }else{

    aux = data %>% group_by_((fcsName.var)) %>% mutate(n=n(),l=ifelse(n>mincells,T,F)) %>%
      dplyr::filter(l) %>% ungroup

    fcs_toAnalyze = data %>% group_by_((fcsName.var)) %>% summarise(n=n(),l=ifelse(n>mincells,T,F)) %>%
      dplyr::filter(l) %>% ungroup %>% data.frame
    fcs_toAnalyze = fcs_toAnalyze[,fcsName.var]

    cat('Total samples:',length(fcs_toAnalyze),' (',n_distinct(data[,fcsName.var])-length(fcs_toAnalyze),'removed )\n')

    if(length(fcs_toAnalyze)==0) stop()

    if(minimize) min_events=mincells else min_events <- aux %>% group_by_((fcsName.var)) %>% summarise(n=n()) %>% .$n %>% min
    if(min_events>maxcells) min_events=maxcells

    cat('sampling',min_events,'cells\n')

    set.seed(seed)
    aux = aux %>% dplyr::select(fcsName.var, all_of(params),everything(),-n,-l) %>%
      group_by_((fcsName.var)) %>% sample_n(min_events) %>% ungroup %>%
      na.omit()

  }

  if(keepFileName){
    aux = aux  %>% dplyr::select(fcsName.var,all_of(params),everything())
  }else{
    aux = aux  %>% dplyr::select(all_of(params),everything())
  }

  return(aux)
}
