


# data export of frequencies at each cluster or single-cell data to be entered again in FlowJo
exportSNE = function(data,sample.var='sampleID',
                     clusters.var='louvain',type='flowjo',
                     save.path='./',filename='',date=TRUE){

  if(type=='flowjo') UEMR::uniexport(data,'csv',path = save.path,filename = paste0('toFlowJo_',n_distinct(data[,clusters.var]),'_clusters_',filename),date=date,sep=',')

  if(type=='freq'){

    data = data.frame(data)
    tab = table(data[,sample.var],data[,clusters.var]) %>% data.frame
    tab = reshape2::dcast(tab,Var1~Var2)
    colnames(tab)[-1] = paste0('Cluster_',colnames(tab)[-1])
    colnames(tab)[1] = sample.var
    UEMR::uniexport(tab,'excel',path = save.path,
              filename = paste0('Cell_freq_',n_distinct(data[,clusters.var]),'_clusters_',filename),
              date=date)

  }

  # if(type=='prism'){
  #
  #   tab = table(data[,sample.var],data[,clusters.var]) %>% data.frame
  #   tab = reshape2::dcast(tab, Var1~Var2)
  #   colnames(tab)[-1] = paste0('Cluster_',colnames(tab)[-1])
  #
  #   cl = names(tab)[grep('Cluster',names(tab))]
  #
  #   aux = lapply(1:length(cl),function(c){
  #     aux_f = tab %>%
  #       arrange(sample.var) %>%
  #       select(group.var,cl[c]) %>% mutate(ID=1:nrow(.)) %>%
  #       melt(id.vars=c('ID','Group')) %>% dcast(ID~Group+variable,value.var='value')
  #
  #     aux_f = qpcR:::cbind.na(NA,na.omit(aux_f[,2]),
  #                             na.omit(aux_f[,3]),
  #                             na.omit(aux_f[,4]),
  #                             na.omit(aux_f[,5]),NA) %>% data.frame
  #     colnames(aux_f) = c(cl[c],'HC','Ei','S1','S2',NA)
  #
  #     return(aux_f)
  #   }) %>% do.call('cbind.data.frame',.)
  #
  #   uniexport(aux,'excel',path = path.data,
  #             filename = 'Single_cell_BK_tSNE_clusters_per_PRISM',
  #             date=TRUE,row.names=F)
  #
  # }


}
