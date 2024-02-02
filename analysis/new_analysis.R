library(tidyverse)

# data_clean.csv dataframe saved in prepare-data.R
data_clean = read_csv("~/Documents/github/2023-static-audio/data/mel_pref1/anonymous/data/data_clean.csv")

# pilot (n = 10)
# data_clean = read_csv("~/Documents/github/2023-static-audio/data/pilot_mel_pref1/anonymous/data/data_clean.csv")

# N participants
length(table(data_clean$participant_id)) # 80
# N stimuli
length(table(data_clean$audio_name)) # 40

ggplot(data_clean, aes(reorder(audio_name, z_answer), z_answer, color=audio_name)) +
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  ylab("Liking rating (z- scored)") +
  xlab("stimuli") +
  theme_classic() +
  theme(legend.position = "none")

ggsave("song_ratings_main.png", height = 10, width = 20, units = "cm")


# clustering
library(ggrepel) # avoid overlapping labels in plot
library(factoextra) # cluster analysis
library(ggfortify) # plot PCAs
library(plotly) # plot 3D PCAs

data_to_clust = data_clean %>%  select(-definition, -answer, -gender) %>% 
    drop_na()# %>%
 # pivot_wider(names_from = "audio_name", values_from = "z_answer") - no longer needed



x = run_pca(data_to_clust, 4)

x$pca_res
x$sum_pca
x$elbow
x$silhouette
x$plot_2D_pca

  
run_pca = function(data, k){
  pca_res = prcomp(data[,-1:-2])
  sum_pca = summary(pca_res)
  
  elbow = fviz_nbclust(data[,-1:-8], kmeans, method = "wss") +
    geom_vline(xintercept = 4, linetype = 2)+
    labs(subtitle = "Elbow method")
  
  silhouette = fviz_nbclust(data[,-1:-8], kmeans, method = "silhouette")+
    labs(subtitle = "Silhouette method")
  
  # Assuming k is the chosen number of clusters
  cluster <- kmeans(data[,-1:-8], k)
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


# Perform hierarchical clustering
hc_res <- hclust(dist(data_to_clust[,-1:-2]))

# Determine the number of clusters (k)
k = 3  # You can change this based on your desired number of clusters

# Cut the hierarchical clustering tree into k clusters
clusters <- cutree(hc_res, k = k)

# Add cluster assignments as a new column to the dataset
data_to_clust$cluster <- clusters

# Plot dendrogram
plot(hc_res, hang = -1, cex = 0.6,
     main = "Dendrogram - Hierarchical Clustering", xlab = "Cities", sub = "",
     labels = data_to_clust$participant_id)

# Add clusters to the dendrogram
cluster_colors <- c("red", "blue", "green", "purple", "orange", "brown")

rect.hclust(hc_res, k = k, border = cluster_colors[1:k])


# Plot dendrogram with different cut heights and highlight six clusters with distinct colors
# png("plots/plot_dendrogram_UA_gov.confidence.png", height = 14, width = 14, units = "cm", res = 300)
# plot(hc_res, hang = -1, cex = 0.6,
#      main = "Dendrogram UKRAINE - HCLUST", xlab = "Cities", sub = "",
#      labels = wvs_merged_data$city_name)
# rect.hclust(hc_res, k = k, border = cluster_colors[1:k])  # Highlight six clusters with distinct colors
# dev.off()


# clustering + individual differences 
#Now not needed with data_clean combined already
#data_combined_long = merge(x = data_to_clust, y = participant_data_clean, by = "participant_id", all = TRUE)
#data_combined = tibble(data_combined_long[,-3:-42]) %>% drop_na()

#still need to remove data for plot
data_combined = tibble(data_to_clust[,c(2, 3, 11, 52)]) %>% drop_na() #participant_id, age, gmsi_score, cluster

data_combined %>% 
  group_by(cluster) %>% 
  summarise(n = n(), 
            mean_gold = mean(gmsi_score, na.rm = T), 
            sd_gold = sd(gmsi_score, na.rm = T), 
            mean_age = mean(age, na.rm = T),
            sd_age = mean(age, na.rm = T))


# replot long format
data_to_plot = tibble(data_to_clust) %>% 
  #pivot_longer(cols = `117`:`86`, names_to = "audio_name", values_to = "z_answer") %>% - not needed
  select(-gmsi_12:-gmsi_35)  %>% 
  drop_na(z_answer) %>% 
  mutate(audio_name = factor(audio_name), z_answer = z_answer)


ggplot(data_to_plot, aes(reorder(audio_name, z_answer), z_answer, 
                         color=factor(cluster)
                         )) +
    geom_boxplot() +
    # geom_jitter(shape=16, position=position_jitter(0.2)) +
    ylab("Liking rating (z- scored)") +
    xlab("stimuli") +
    theme_classic() +
    theme(legend.position = "right")

ggsave("song_ratings_main.png", height = 10, width = 20, units = "cm")
