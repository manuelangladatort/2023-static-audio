# methods
run_pca = function(data, data_to_clust, k){
  pca_res = prcomp(data_to_clust)
  sum_pca = summary(pca_res)
  
  elbow = fviz_nbclust(data_to_clust, kmeans, method = "wss") +
    geom_vline(xintercept = 4, linetype = 2)+
    labs(subtitle = "Elbow method")
  
  silhouette = fviz_nbclust(data_to_clust, kmeans, method = "silhouette")+
    labs(subtitle = "Silhouette method")
  
  # Assuming k is the chosen number of clusters
  cluster <- kmeans(data_to_clust, k)
  data$cluster <- as.factor(cluster$cluster)
  
  plot_2D_pca = autoplot(pca_res,  data = data, colour = 'cluster', size = 1) +
    geom_text_repel(aes(label = participant_id), 
                    box.padding = 0.5, 
                    point.padding = 0.5, 
                    segment.color = 'grey',
                    size = 2.5) + 
    theme(axis.text.x = element_text(size=6),
          axis.text.y=element_text(size=8),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "right",
          legend.text = element_text(size=10),
          legend.title = element_blank()) +
    # ggtitle("World Survey Data Space") +
    theme_classic()
  
  return(list(
    pca_res=pca_res, 
    sum_pca=sum_pca, 
    elbow=elbow, 
    silhouette=silhouette, 
    plot_2D_pca=plot_2D_pca, 
    data=data))
}

