


## FlowJo biexponential transformation:
FlowJo_biexp = function(data,params,channelRange = 4096, maxValue = 262144, pos = 4.5, neg = 0,
                        widthBasis = -10, inverse = FALSE){

  fjbiexp = flowWorkspace::flowJoTrans(channelRange = channelRange, maxValue = maxValue, pos = pos, neg = neg,
                        widthBasis = widthBasis, inverse = inverse)
  fcsData_Fj_biexp = data %>% mutate_at(params,fjbiexp)

}


