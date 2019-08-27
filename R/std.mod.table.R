


# standard model summary table --------------------------------------------

std.mod.table = function(x){
  summary(x) %>% .$coefficients %>% data.frame %>% round(6) %>%
    select(Estimate = 1,Pval = ncol(.)) %>%
    mutate(p.sign = pToSign(Pval),Parameters = rownames(.)) %>%
    select(Parameters,Estimate,Pval,p.sign)
}

