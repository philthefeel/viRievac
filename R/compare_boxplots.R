

# Paired and Unpaired Boxplots with Pvals ---------------------------------


# Function that creates boxplots with pairwise comparisons
#' @param data - a data.frame
#' @param vars - character. Selection of variables to analyze from the data.frame
#' @param id.var - character. Identifier
#' @param group.var - character. Grouping variable
#' @param paired - logical. Type of analysis paired or independent
#' @param method - character. Method of analysis to select between 'wilcox.test' and 't.test'
#' @param pval - character. Allowed values include "p.signif" (shows the significance levels), "p.format" (shows the formatted p value).
#' @param my_comparisons - if paired=FALSE. A list specifying which groups to compare
#' @param scales - should axis scales of panels be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y")
#' @param col - points and box plot colors. To color by conditions, use color = "condition".
#' @param palette - ggpubr palette to be used for coloring or filling by groups.
#' @param add - add observations jittered
#' @param shape - character. Change the shape of points by defined variable
#' @param label.x - numeric. Position of pval inside the plot. Default: 1.5, centered
#' @param facet - factor vector. Facet variables
#' @param facet.nrow - Number of rows and columns in the panel. Used only in paired analysis.
#' @param save - logical. Save the plot in pdf
#' @param path.output - path to save
#' @param filename - character. Name
#' @param width - numeric. width of pdf output in cm
#' @param height - numeric. Height of pdf output in cm
#' @param date - include the current date in the filename?


compare_boxplots = function(data,vars,id.var,group.var,paired=F,
                            method = 'wilcox.test',
                            pval = 'p.signif',
                            my_comparisons = NULL,
                            scales = 'free',
                            col=group.var,
                            shape=group.var,
                            palette='jco',
                            add = TRUE, label.x = 1.5,
                            facet = NULL,
                            nrow = NULL,
                            save=FALSE,
                            path.output='./',
                            filename = 'compare_boxplot',
                            width=15,height=15,
                            date=F){

  if(!add) add = NULL else add='jitter'
  # if(class(mydata[,group.var])!='factor') stop('group.var must be a factor variable')

  if(is.null(my_comparisons)) my_comparisons = combn(1:n_distinct(data %>% select_(group.var)),2) %>% apply(2,list) %>% lapply(unlist)

  if(paired){

    # if(!exists(id.var)) stop('in a paired analysis id.var must be defined')

    facet = c('variable',facet)

    aux = data %>% select(id.var,group.var,vars) %>% melt %>% arrange_(group.var,id.var)
    # aux = aux %>% group_by_(id.var,facet,vars) %>%
    #   mutate(n=n()) %>% filter(n==2) %>%
    #   select(-n) %>% data.frame

    p = ggpaired(aux, x = group.var, y = "value",
                 col = group.var, palette = palette,
                 line.color = "#acbebe", line.size = 0.4)+
      theme_bw() +
      stat_compare_means(label=pval,
                         method = method,
                         paired=TRUE,
                         label.x = label.x)
    p = facet(p,
              facet.by=rev(facet),
              nrow = nrow,
              short.panel.labs = T,
              scales=scales,
              panel.labs.background = list(fill='#f4f4ef'))


  } else{

    aux = data %>% select(group.var,vars,facet) %>% melt
    facet = c('variable',facet)

    p = ggboxplot(aux, x = group.var, y = "value",
                  col = col, palette = palette,
                  add=add,shape=shape)+ theme_bw()+
      stat_compare_means(label=pval,
                         method = method,
                         comparisons = my_comparisons,
                         paired=paired)
    p = facet(p,
              facet.by=rev(facet),
              nrow = nrow,
              short.panel.labs = T,
              scales=scales,
              panel.labs.background = list(fill='#f4f4ef'))

  }

  if(save){
    if(date) filename = paste(filename,Sys.Date(),sep = '_')
    fn = paste0(path.output,filename,'.pdf')
    pdf(fn,height=height,width = width)
    print(p)
    dev.off()

  } else{
    return(p)
  }

}
