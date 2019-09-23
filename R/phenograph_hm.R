

# Create a Heatnap from Rphenograph results:
# data must have a variable named 'louvain', the output from Rphenograph()


phenograph_hm = function(data,
                         vars,
                         color=colorRampPalette(rev(RColorBrewer::brewer.pal(n = 9, name = "RdYlBu")))(15),
                         statistic='median'){

  med = data %>% dplyr::select(louvain,vars) %>%
    group_by(louvain)
  if(statistic=='median') med = med %>% dplyr::summarise_all(median) %>% select(-louvain) %>% t() %>% data.frame
  if(statistic=='mean') med = med %>% dplyr::summarise_all(mean) %>% select(-louvain) %>% t() %>% data.frame
  colnames(med) = colnames(med) %>% stringr::str_replace_all('X','')

  # Calculate cluster frequencies
  clustering_table <- as.numeric(table(data$louvain))
  labels_col <- paste0(colnames(med), " (",
                       round(clustering_table / sum(clustering_table) * 100, 2), "%)")

  # heatmap parameters
  hm.parameters <- list(med,
                        color = color,
                        cellwidth =20,
                        cellheight=15,
                        kmeans_k = NA,
                        show_rownames = T, show_colnames = T,
                        main = "",
                        clustering_method = "ward.D2",
                        cluster_rows = T,
                        cluster_cols = T,
                        clustering_distance_rows = "euclidean",
                        clustering_distance_cols = "euclidean",
                        labels_col = labels_col,
                        display_numbers = TRUE, number_color = "black",
                        fontsize = 8, fontsize_number = 5
  )

  do.call('pheatmap',hm.parameters)

}
