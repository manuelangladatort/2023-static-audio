library(tidyverse)

# data_clean.csv dataframe saved in prepare-data.R
data_clean = read_csv("~/Documents/github/2023-static-audio/data/mel_pref1/data_clean.csv")

# pilot (n = 10)
# data_clean = read_csv("~/Documents/github/2023-static-audio/data/pilot_mel_pref1/data_clean.csv")

# N participants
length(table(data_clean$participant_id)) # 80
# N stimuli
length(table(data_clean$audio_name)) # 40

data_clean$audio_name = factor(data_clean$audio_name)


## EXPLORE MUSIC RATINGS
ggplot(data_clean, aes(reorder(audio_name, z_answer), z_answer, color=audio_name)) +
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  ylab("Liking rating (z- scored)") +
  xlab("stimuli") +
  theme_classic() +
  theme(legend.position = "none")

ggsave("song_ratings_main.png", height = 10, width = 20, units = "cm")


## EXPLORE CORRELATIONS OF INDIVIDUAL DIFFERENCES

library(corrplot)
library(data.table)


col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

#converting gender to numeric variable
data_clean$gender <- case_when(
    data_clean$gender == "female" ~ 1,
    data_clean$gender == "male" ~ 2,
    data_clean$gender == "non_binary" ~ 3
)

# only one set of participant's individual differences variables per participant
data_cor <- data_clean %>%
  ungroup() %>%
  arrange(participant_id)
data_cor <- subset(data_cor, !duplicated(subset(data_cor, select=c(participant_id, gender))))

#selecting for correlation variables
data_cor <- data_cor %>%
  select(age, gender, gmsi_score, openness, extraversion, agreeableness, neuroticism, conscientiousness, mellow, unpretentious, sophisticated, intense, contemporary, Challenge_skill, Action_awareness, Clear_goals, Unambiguous_feedback, Task_concentration, Sense_of_control, Transformation_of_time, Autotelic_experience, DFS_all_score) %>%
  mutate_all(as.numeric) 
  
p1 <- corrplot(cor(data_cor, use="complete.obs"),
         method="color",
         type="upper",
         p.mat = res1$p,
         insig = "label_sig",
         sig.level = .05,
         #pch.cex = 0.8,
         #pch.col = "black",
         tl.pos = "td",
         tl.col="black",
         tl.cex=0.8,
         tl.srt=50,
         #addCoef.col = "black",
         outline=TRUE,
         diag = FALSE,
         )

p1

####Save corrplot

## PNG device
png("individual_differences_corrplot.png")

##Plot Code
corrplot(cor(data_cor, use="complete.obs"),
         method="color",
         type="upper",
         p.mat = res1$p,
         insig = "label_sig",
         sig.level = .05,
         #pch.cex = 0.8,
         #pch.col = "black",
         tl.pos = "td",
         tl.col="black",
         tl.cex=0.8,
         tl.srt=50,
         #addCoef.col = "black",
         outline=TRUE,
         diag = FALSE,
         )

## Close device
dev.off()


################################################################################
# what is the role of indiviudal differences on music preferences?
################################################################################
## Linear models
model1 = lm(answer ~ gmsi_score + openness + World, data = data_clean)
summary.lm(model1)

model2 = lm(answer ~ gmsi_score + age + gender + gmsi_score + openness + extraversion + agreeableness + neuroticism + conscientiousness + mellow + unpretentious + sophisticated + intense + contemporary + World + Challenge_skill + Action_awareness + Clear_goals + Unambiguous_feedback + Task_concentration + Sense_of_control + Transformation_of_time + Autotelic_experience, data = data_clean)
summary.lm(model2)

library(lme4)
library(lmerTest)
#model1.mixed = lmer(answer ~ gmsi_score + openness + World +
#                (1 | participant_id), data = data_clean)
#summary(model1.mixed)


## CLUSTERING
library(ggrepel) # avoid overlapping labels in plot
library(factoextra) # cluster analysis
library(ggfortify) # plot PCAs
library(plotly) # plot 3D PCAs


# we want to convert the data to wide format with rows as particaiptns and columns as songs
data_to_clust = ratings_data_clean %>%  select(-definition, -answer) %>% 
  pivot_wider(names_from = "audio_name", values_from = "z_answer")


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



data_to_clust %>% 
  group_by(cluster) %>% 
  summarise(n = n())

# given these groups, can we see if they differ in their individual differences?
## calculate the mean and SD of individual differences for each group
## bar plot of groups for each individual differences


# old materials below

# Plot dendrogram with different cut heights and highlight six clusters with distinct colors
# png("plots/plot_dendrogram_UA_gov.confidence.png", height = 14, width = 14, units = "cm", res = 300)
# plot(hc_res, hang = -1, cex = 0.6,
#      main = "Dendrogram UKRAINE - HCLUST", xlab = "Cities", sub = "",
#      labels = wvs_merged_data$city_name)
# rect.hclust(hc_res, k = k, border = cluster_colors[1:k])  # Highlight six clusters with distinct colors
# dev.off()


# clustering + individual differences 
data_combined_long = merge(x = data_to_clust, y = participant_data_clean, by = "participant_id", all = TRUE)
data_combined = tibble(data_combined_long[,-3:-42]) %>% drop_na()


data_combined %>% 
  group_by(cluster) %>% 
  summarise(n = n(), 
            mean_gold = mean(gmsi_score, na.rm = T), 
            sd_gold = sd(gmsi_score, na.rm = T), 
            mean_age = mean(age, na.rm = T),
            sd_age = mean(age, na.rm = T))


# replot long format
data_to_plot = tibble(data_to_clust) %>% 
  pivot_longer(cols = `117`:`86`, names_to = "audio_name", values_to = "z_answer") %>%
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
