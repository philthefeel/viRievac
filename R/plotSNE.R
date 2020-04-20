


# tSNE plot
plotSNE = function(data,vars=c('tSNE_V1','tSNE_V2'), col.clusters =F,
                   clusters.var='louvain',palette=NULL,
                   alpha=1,ylim=c(-100,100),xlim=c(-100,100),size=1,
                   save=T,path.save='./',filename='tSNE_',date=T,
                   height=9,width=9,res=150,units='in'){

  require(ggplot2)

  if(col.clusters){

    if(is.null(palette)){
      lev = n_distinct(data[,clusters.var])
      mycols <- viRievac::color_cluster(lev)
    } else{
      mycols <- palette
    }

    # cluster centroids for labels:
    lc.cent = data %>% group_by_(clusters.var) %>% dplyr::select(vars) %>%
      summarize_all(median)
    colnames(lc.cent) = c('louvain','tSNE_V1','tSNE_V2')

    # data for ploting:
    d = data %>% dplyr::select(vars,clusters.var)
    colnames(d) = c('tSNE_V1','tSNE_V2','louvain')

    # ggplot:
    p <- ggplot(d, aes(x = tSNE_V1, y = tSNE_V2, colour = factor(louvain))) +
      geom_point(alpha = alpha,size=size) + theme_bw() +
      ylim(ylim) + xlim(xlim) +
      scale_color_manual(values=mycols)+
      ggrepel::geom_label_repel(aes(label = louvain),
                                data = lc.cent) + guides(colour = FALSE)
  } else{
    # data for plot:
    d = data %>% dplyr::select(vars)
    colnames(d) = c('tSNE_V1','tSNE_V2')

    # ggplot:
    p <- ggplot(d, aes(x=tSNE_V1, y=tSNE_V2)) +
      geom_point(alpha = alpha,size=size) + theme_bw() + ylim(ylim) + xlim(xlim)
  }

  if(save){
    # create filename:
    filename = paste0(path.save,filename)
    if(col.clusters) filename = paste0(filename,'_with_',lev,"_clusters")
    if(date) filename = paste0(filename,'_',Sys.Date())

    # save in png:
    png(file = paste0(filename,".png"),
        height=height,width=width,res=res,units=units)
    print(p)
    dev.off()
  } else{
    print(p)
  }

}
