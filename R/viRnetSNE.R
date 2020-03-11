


# netSNE model:
viRnetSNE = function(data,params,path.netSNE.dir = "~/Documents/netsne-master/bin/",
                     path.output.dir = 'netSNE_outputs',
                     toRun = c("Compute.sim", "BhtSNE","NetSNE.train","NetSNE.project"),
                     max.iter = 1000, learn.rate=200,NN.layers= 2,
                     trainOutput=T,trainSize=1000,seed=1985,normalize=TRUE,
                     minimize=FALSE,plot=F,filename='netSNE',
                     size=0.05,alpha=1,ylim=c(-50,50),xlim=c(-50,50),
                     clustering=TRUE,cluster.method='Phenograph',k=30,palette=NULL,path.save = './'){


  data$nid = 1:nrow(data)
  m.train = sampleCells(data,params=params,minimize = minimize,mincells = trainSize,keepFileName = T,seed=seed) %>%
    mutate(sample=TRUE) %>% data.frame
  m.test = dplyr::anti_join(data,m.train) %>%
    mutate(sample=FALSE) %>% data.frame

  # joint datasets:
  m.total = bind_rows(m.train,m.test) %>% arrange(nid) %>% data.frame

  if(normalize){
    m.total <- m.total %>% mutate_at(params,scale)
    train = scale(m.train %>% select(all_of(params)))
    test = scale(m.test %>% select(all_of(params)))
  }

  set.seed(seed)
  Result=rNetSNE::R_netSNE(to.run = toRun,
                           path.netSNE.dir = path.netSNE.dir,
                           out.dims = 2, max.iter = max.iter, perp = 30L, theta.BhtSNE = 0.5, theta.NetSNE = 0.5,
                           mom.init = 0.5, mom.final = 0.8, mom.switch.iter = 250L,
                           early.exag.iter = 250, learn.rate.bhtSNE = learn.rate, learn.rate.netSNE = 0.02,
                           NN.layers = NN.layers, NN.units = 50, NN.function = "relu", sgd = TRUE,
                           batch.frac.bhtSNE = NULL, batch.frac.netSNE = 0.1, random.init = TRUE,
                           local.sample = 20, min.sample.Z = 0.1, l2.reg.param = 0, step.method = "adam",
                           save.iters.cache = NULL, permute.after.iters = NULL, seed = seed, verbose = TRUE,
                           path.output.dir = path.output.dir,
                           train.data = train,
                           test.data = test,
                           ref.embedding = "BhtSNE",force.rm.old.outdir = T,
                           name.bhtSNE.output.dir = "Bhtsne_ref_embed", name.netSNE.output.dir = "Netsne_out")


  # train cells tSNE
  colnames(Result$`Bht-SNE`) = c('tSNE_V1','tSNE_V2')
  netSNE_train = cbind.data.frame(m.train %>% select(nid,sample),Result$`Bht-SNE`)

  # test cells tSNE
  colnames(Result$`Net-SNE (Projection)`) = c('tSNE_V1','tSNE_V2')
  netSNE_emb = cbind.data.frame(m.test %>% select(nid,sample),Result$`Net-SNE (Projection)`)

  # all cells together
  netSNE_emb = bind_rows(netSNE_emb,netSNE_train) %>% arrange(nid) %>%
    select(-nid)
  rm(netSNE_train,m.train,m.test); uem.gc()

  if(clustering){

    cat('Finding clusters using ',cluster.method,' algorithm\n')

    # Finding clusters in the scaled data using Phenograph
    louvain = viRPhenograph(m.total,params=params,k=k)
    netSNE_emb$louvain = louvain

    if(plot){
      # Visualize Clusters on tSNE plot
      plotSNE(netSNE_emb,vars=c('tSNE_V1','tSNE_V2'), path.save = path.save,
              col.clusters = T, clusters.var = 'louvain',alpha=alpha,size=size,
              palette = palette,filename=filename,xlim=xlim,ylim=ylim)
    }

  }

  if(plot){
    plotSNE(netSNE_emb,vars=c('tSNE_V1','tSNE_V2'),save=T,alpha=alpha,size=size,
            path.save = path.save, filename=paste0(filename,' (all cells)'),
            ylim = ylim, xlim=xlim)
    if(trainOutput){

      plotSNE(netSNE_emb %>% filter(sample),vars=c('tSNE_V1','tSNE_V2'),save=T,
              path.save = path.save, filename=paste0(filename,' (sample)'),
              ylim = ylim, xlim=xlim)

      if(clustering){
        plotSNE(netSNE_emb %>% filter(sample),vars=c('tSNE_V1','tSNE_V2'), path.save = path.save,
                col.clusters = T, clusters.var = 'louvain',alpha=alpha,size=size,
                palette = palette,filename=paste0(filename,' (sample)'),xlim=xlim,ylim=ylim)
      }
    }
  }

  return(netSNE_emb)

}
