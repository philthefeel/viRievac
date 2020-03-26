


# Function that creates boxplots with pairwise comparisons
#' @param data - a data.frame
#' @param vars - character. Selection of variables to analyze from the data.frame
#' @param id.var - character. Identifier
#' @param group.var - character. Grouping variable
#' @param paired - logical. Type of analysis paired or independent
#' @param my_comparisons - list. List of 2-dimensional vectors with the comparisons to make. (default:NULL)
#' @param method - character. Method of analysis to select between 'wilcox.test' and 't.test'
#' @param pval - character. Allowed values include "p.signif" (shows the significance levels), "p.format" (shows the formatted p value).
#' @param save - logical. Save the plot in pdf
#' @param path.output - character. Path to save the results.
#' @param filename - character. Name



compare_groups_table = function(data,
                                vars,
                                id.var,
                                group.var,
                                paired=FALSE,
                                method = 'wilcox.test',
                                my_comparisons = NULL,
                                save=TRUE,
                                path.output = './',
                                filename = 'compare_groups_table',
                                sep=';',
                                date = FALSE){


  pToSign = function(x){

    ELSE=TRUE
    case_when(
      (x>0.05) ~ 'ns',
      (x<0.0001) ~ '****',
      (x<0.001) ~ '***',
      (x<0.01)~ '**',
      ELSE ~ '*'
    )

  }

  # Results table for Independent group comparison:
  if(!paired){
    tab = lapply(1:length(vars),function(i){

      mydata = data %>% data.frame

      # test statistics table_
      form = reformulate(termlabels = paste0(group.var), response = vars[i])
      tab = compare_means(form,data=mydata %>% arrange_(group.var),
                          method=method,paired=paired)

      if(!is.null(my_comparisons)) tab = tab %>% dplyr::filter(group1%in%unlist(my_comparisons) & group2%in%unlist(my_comparisons))

      # Median/mean value of each group_
      form1 = reformulate(termlabels = paste0(group.var), response = vars[i])
      if(method=='wilcox.test'){
        med1 = aggregate(form1,data=mydata %>% arrange_(group.var), median) %>%
          mutate_if(is.factor,as.character)
        colnames(med1) =c('group1','median1')
        med2 = aggregate(form1,data=mydata %>% arrange_(group.var), median) %>%
          mutate_if(is.factor,as.character)
        colnames(med2) =c('group2','median2')

        # Create output table_
        tab = left_join(tab,med1) %>% left_join(.,med2)
        tab = tab %>% mutate(Param = vars[i],
                             Comp=paste0(group1,'-',group2),
                             p.adj.signif = pToSign(p.adj),
                             test = method,
                             non.parametric = ifelse(test=='Wilcoxon',TRUE,FALSE),
                             paired = paired) %>%
          select(Param,Comp,median1,median2,p.format,
                 p.signif,Holm.p.adj=p.adj,p.adj.signif,test,non.parametric,paired) %>%
          data.frame

        return(tab)

      } else{
        med1 = aggregate(form1,data=mydata %>% arrange_(group.var), mean) %>% mutate_if(is.factor,as.character)
        colnames(med1) =c('group1','mean1')
        med2 = aggregate(form1,data=mydata %>% arrange_(group.var), mean) %>% mutate_if(is.factor,as.character)
        colnames(med2) =c('group2','mean2')

        # Create output table_
        tab = left_join(tab,med1) %>% left_join(.,med2)
        tab = tab %>% mutate(Param = vars[i],
                             Comp=paste0(group1,'-',group2),
                             p.adj.signif = pToSign(p.adj),
                             test = method,
                             non.parametric = ifelse(test=='Wilcoxon',TRUE,FALSE),
                             paired = paired) %>%
          select(Param,Comp,mean1,mean2,p.format,
                 p.signif,Holm.p.adj=p.adj,p.adj.signif,test,non.parametric,paired) %>%
          data.frame

        return(tab)

      }


    })

    tab = tab %>% bind_rows()

    if(save){
      p = 'Independent'
      uniexport(tab,type = 'excel',
                path = path.output,
                filename = paste0(filename,'_',method,'_',p),
                date=date)
    } else{
      return(tab)
    }

  }

  # Results table for Paired group comparison:
  if(paired){
    # Make sure we use samples
    mydata = data %>% group_by_(id.var) %>%
      mutate(n=n()) %>% dplyr::filter(n==2) %>% select(-n) %>%
      ungroup %>% data.frame                  # only individuals with data at both condition

    tab = lapply(1:length(vars),function(i){

      # test statistics table_
      form = reformulate(termlabels = paste0(group.var), response = vars[i])
      tab = compare_means(form,data=mydata %>% arrange_(group.var),
                          method=method,paired=TRUE)

      # Median/mean value of each group_
      form1 = reformulate(termlabels = paste0(group.var), response = vars[i])
      if(method=='wilcox.test'){
        med1 = aggregate(form1,data=mydata %>% arrange_(group.var), median) %>%
          mutate_if(is.factor,as.character)
        colnames(med1) =c('group1','median1')
        med2 = aggregate(form1,data=mydata %>% arrange_(group.var), median) %>%
          mutate_if(is.factor,as.character)
        colnames(med2) =c('group2','median2')

        # Create output table_
        tab = left_join(tab,med1) %>% left_join(.,med2)
        tab = tab %>% mutate(Param = vars[i],
                             Comp=paste0(group1,'-',group2),
                             p.adj.signif = pToSign(p.adj),
                             test = method,
                             non.parametric = ifelse(test=='Wilcoxon',TRUE,FALSE),
                             paired=paired) %>%
          select(Param,Comp,median1,median2,p.format,
                 p.signif,Holm.p.adj=p.adj,p.adj.signif,test,non.parametric,paired) %>%
          data.frame

      } else{
        med1 = aggregate(form1,data=mydata %>% arrange_(group.var), mean) %>%
          mutate_if(is.factor,as.character)
        colnames(med1) =c('group1','mean1')
        med2 = aggregate(form1,data=mydata %>% arrange_(group.var), mean) %>%
          mutate_if(is.factor,as.character)
        colnames(med2) =c('group2','mean2')

        # Create output table_
        tab = left_join(tab,med1) %>% left_join(.,med2)
        tab = tab %>% mutate(Param = vars[i],
                             Comp=paste0(group1,'-',group2),
                             p.adj.signif = pToSign(p.adj),
                             test = method,
                             non.parametric = ifelse(test=='Wilcoxon',TRUE,FALSE),
                             paired=paired) %>%
          select(Param,Comp,mean1,mean2,p.format,
                 p.signif,Holm.p.adj=p.adj,p.adj.signif,test,non.parametric,paired) %>%
          data.frame
      }

      return(tab)
    })
    tab = tab %>% bind_rows()

    if(save){
      p = 'Paired'
      uniexport(tab,type = 'excel',
                path = path.output,
                filename = paste0(filename,'_',method,'_',p),
                date=date)
    } else{
      return(tab)
    }

  }



}
