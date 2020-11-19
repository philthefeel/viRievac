


# phenograph_hm2 ----------------------------------------------------------

phenograph_hm2 = function(data,
                          group.var,
                          vars,
                          color=colorRampPalette(c("blue4","#0571B0","white","red1","#CA0020"))(100),
                          statistic='median',
                          palette = NULL){

  aux = data %>% dplyr::select(all_of(group.var),all_of(vars)) %>% na.omit() %>% group_by_(group.var)
  if(statistic=='median') aux = aux %>% dplyr::summarise_all(median) %>% select(-group.var) %>% t() %>% data.frame
  if(statistic=='mean') aux = aux %>% dplyr::summarise_all(mean) %>% select(-group.var) %>% t() %>% data.frame
  colnames(aux) = table(data[,group.var]) %>% names

  # Calculate cluster frequencies
  clustering_table <- as.numeric(table(data %>% select(all_of(group.var))))
  labels_col <- paste0(colnames(aux), " (",formatC(clustering_table,format="g"),', ',
                       round(clustering_table / sum(clustering_table) * 100, 2), "%)")

  # heatmap parameters
  pheatmap::pheatmap(aux,
           color = color,
           scale = "row",
           breaks = seq(-3.5,3.5,7/100),
           cellwidth =20,cellheight=15,
           kmeans_k = NA,
           show_rownames = T, show_colnames = T,
           main = "",
           clustering_method = "ward.D2",
           cluster_rows = T,
           cluster_cols = T,
           clustering_distance_rows = "euclidean",
           clustering_distance_cols = "euclidean",
           labels_col = labels_col,
           annotation_colors=palette,
           display_numbers = TRUE, number_color = "black",
           fontsize = 8,
           fontsize_number = 0)

}
