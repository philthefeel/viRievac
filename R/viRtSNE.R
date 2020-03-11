


# tSNE model function
viRtSNE = function(data,params,pca=F,perplexity=30,theta=0.5,iterations=2500,
                   ylim=c(-100,100),xlim=c(-100,100),palette=NULL,
                   trainSize=1000,seed=1985,normalize=TRUE,
                   plot=T,save=T,path.save='./',filename='tSNE',date=T,
                   clustering=TRUE,cluster.method='Phenograph',k=30){

  # m.train = sampleCells(data,params=params,mincells = trainSize,keepFileName = F,seed=seed)

  if(normalize) m.train <- data %>% scale else m.train = data

  # run tSNE:
  set.seed(seed)
  mod = Rtsne::Rtsne(as.matrix(m.train),
              check_duplicates = F,
              verbose = T,
              #initial_dims = selectedPC,
              pca = pca,
              #pca_scale = T,
              #pca_center = T,
              perplexity = perplexity,
              theta = theta,
              max_iter = iterations,
              normalize = F,
              dims = 2)
  tsne_emb = as.data.frame(mod$Y)
  colnames(tsne_emb) = c('tSNE_V1','tSNE_V2')

  # plot:
  if(plot){

    plotSNE(tsne_emb,vars=c('tSNE_V1','tSNE_V2'),save=save,
            path.save = path.save, filename=paste0(filename,' (sample)'),
            ylim = ylim, xlim=xlim)

  }

  if(clustering){

    cat('Finding clusters using ',cluster.method,' algorithm\n')

    # Finding clusters in the scaled data using Phenograph
    louvain = viRPhenograph(m.train,params=params,k=k)
    tsne_emb$louvain = louvain


    # Visualize Clusters on tSNE plot
    plotSNE(tsne_emb,vars=c('tSNE_V1','tSNE_V2'), path.save = path.save,
            col.clusters = T, clusters.var = 'louvain',alpha=alpha,size=size,
            palette = palette,filename=filename,xlim=xlim,ylim=ylim)

  }

  return(tsne_emb)

}
