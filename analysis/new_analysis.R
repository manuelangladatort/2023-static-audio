library(tidyverse)

# data_clean.csv dataframe saved in prepare-data.R
data_clean = read_csv("~/Documents/github/2023-static-audio/data/mel_pref1/data_clean.csv")

ratings_data = read_csv("~/Documents/github/2023-static-audio/data/mel_pref1/RatingTrial.csv")

ratings_data_clean = ratings_data %>% 
  filter(failed == FALSE) %>%  # filter out participants who failed
  filter(is_repeat_trial == "FALSE") %>%  # filter out repeated trials
  select(participant_id, definition, audio_name, answer) %>% 
#   extract audio name
  mutate(audio_name = as.factor(as.numeric(str_extract(audio_name, "\\d+")))) %>% 
  # count number trials per participant 
  group_by(participant_id) %>% 
  mutate(n_ratings = n()) %>% 
  # z-score ratings per particiapnt
  mutate(z_answer = scale(answer)) %>% 
  # exclude participants with less than <40 trials
  filter(n_ratings >= 40)

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


res1 <- cor.mtest(data_cor[, c(1:22)], conf.level = .95)

testRes = cor.mtest(data_cor[, c(1:22)], conf.level = 0.95)



################################################################################
# what is the role of indiviudal differences on musi preferences?
################################################################################
## Linear models
model1 = lm(answer ~ gmsi_score + openness + World, data = data_clean)
summary.lm(model1)

library(lme4)
library(lmerTest)
library(ggplot2)

# add stimuli as a random effect?
mixed.lmer = lmer(answer ~ age + (1|participant_id), data = data_clean)
summary(mixed.lmer)
anova(mixed.lmer)
# not significant

mixed.lmer = lmer(answer ~ gmsi_score + (1|participant_id), data = data_clean)
summary(mixed.lmer)
# not sig

mixed.lmer = lmer(answer ~ mellow + (1|participant_id), data = data_clean)
summary(mixed.lmer)
# significant

mixed.lmer = lmer(answer ~ unpretentious + (1|participant_id), data = data_clean)
summary(mixed.lmer)
# significant

mixed.lmer = lmer(answer ~ sophisticated + (1|participant_id), data = data_clean)
summary(mixed.lmer)
# significant

mixed.lmer = lmer(answer ~ intense + (1|participant_id), data = data_clean)
summary(mixed.lmer)
# not sig

mixed.lmer = lmer(answer ~ contemporary + (1|participant_id), data = data_clean)
summary(mixed.lmer)
# significant

mixed.lmer = lmer(answer ~ extraversion + (1|participant_id), data = data_clean)
summary(mixed.lmer)
# not sig

mixed.lmer = lmer(answer ~ neuroticism + (1|participant_id), data = data_clean)
summary(mixed.lmer)
# not sig

mixed.lmer = lmer(answer ~ openness + (1|participant_id), data = data_clean)
summary(mixed.lmer)
# significant

mixed.lmer = lmer(answer ~ agreeableness + (1|participant_id), data = data_clean)
summary(mixed.lmer)
# not significant

mixed.lmer = lmer(answer ~ conscientiousness + (1|participant_id), data = data_clean)
summary(mixed.lmer)
# not sig

#model1.mixed = lmer(answer ~ gmsi_score + mellow + unpretentious + sophisticated + intense + contemporary + extraversion + agreeableness + neuroticism + conscientiousness + openness + (1 | participant_id) + (1 | audio_name ), data = data_clean)
#summary(model1.mixed)

#### Summary of Mixed Effects Model ###
# mellow, unpretentious, sophisticated, contemporary, and openness significantly predict liking ratings

##############

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
k = 2  # You can change this based on your desired number of clusters

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
data_combined_long = merge(x = data_to_clust, y = data_clean, by = "participant_id", all = TRUE)
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

data_barplot <- data_combined %>% 
  group_by(cluster) %>% 
  mutate(mean_gold = mean(gmsi_score, na.rm = T), 
            sd_gold = sd(gmsi_score, na.rm = T), 
            mean_age = mean(age, na.rm = T),
            sd_age = mean(age, na.rm = T),
            mean_mellow = mean(mellow, na.rm = T),
            sd_mellow = mean(mellow, na.rm = T),
            mean_unpretentious = mean(unpretentious, na.rm = T),
            sd_unpretentious = mean(unpretentious, na.rm = T),
            mean_sophisticated = mean(sophisticated, na.rm = T),
            sd_sophisticated = mean(sophisticated, na.rm = T),
            mean_intense = mean(intense, na.rm = T),
            sd_intense = mean(intense, na.rm = T),
            mean_contemporary = mean(contemporary, na.rm = T),
            sd_contemporary = mean(contemporary, na.rm = T),
            mean_extraversion = mean(extraversion, na.rm = T),
            sd_extraversion = mean(extraversion, na.rm = T),
            mean_agreeableness = mean(agreeableness, na.rm = T),
            sd_agreeableness = mean(agreeableness, na.rm = T),
            mean_neuroticism = mean(neuroticism, na.rm = T),
            sd_neuroticism = mean(neuroticism, na.rm = T),
            mean_conscientiousness = mean(conscientiousness, na.rm = T),
            sd_conscientiousness = mean(conscientiousness, na.rm = T),
            mean_openness = mean(openness, na.rm = T),
            sd_openness = mean(openness, na.rm = T)
         )

ggplot(data_barplot, aes(cluster, mean_gold), color = cluster) +
  geom_bar(position = "dodge", stat = "identity")

t.test(gmsi_score ~ cluster, data = data_barplot)
# significant, p = 2.065e-11 - clusters differ by music experience, musicians in group 2 higher music sophistication score?

ggplot(data_barplot, aes(cluster, mean_age), color = cluster) +
  geom_bar(position = "dodge", stat = "identity")

t.test(age ~ cluster, data = data_barplot)
# significant, p < 2.2e-16 - clusters differ by age

ggplot(data_barplot, aes(cluster, mean_extraversion), color = cluster) +
  geom_bar(position = "dodge", stat = "identity")

t.test(extraversion ~ cluster, data = data_barplot)
# significant, extraversion lower in group 1

ggplot(data_barplot, aes(cluster, mean_agreeableness), color = cluster) +
  geom_bar(position = "dodge", stat = "identity")

t.test(agreeableness ~ cluster, data = data_barplot)
# not significant

ggplot(data_barplot, aes(cluster, mean_neuroticism), color = cluster) +
  geom_bar(position = "dodge", stat = "identity")

t.test(neuroticism ~ cluster, data = data_barplot)
# significant, neuroticism higher in group 1

ggplot(data_barplot, aes(cluster, mean_conscientiousness), color = cluster) +
  geom_bar(position = "dodge", stat = "identity")

t.test(conscientiousness ~ cluster, data = data_barplot)
# significant, conscientiousness higher in group 1

ggplot(data_barplot, aes(cluster, mean_openness ), color = cluster) +
  geom_bar(position = "dodge", stat = "identity")

t.test(openness  ~ cluster, data = data_barplot)
# significant, openness lower in group 1

ggplot(data_barplot, aes(cluster, mean_mellow), color = cluster) +
  geom_bar(position = "dodge", stat = "identity")

t.test(mellow  ~ cluster, data = data_barplot)
# significant, mellow higher in group 1

ggplot(data_barplot, aes(cluster, mean_unpretentious), color = cluster) +
  geom_bar(position = "dodge", stat = "identity")

t.test(unpretentious  ~ cluster, data = data_barplot)
# not significant

ggplot(data_barplot, aes(cluster, mean_sophisticated), color = cluster) +
  geom_bar(position = "dodge", stat = "identity")

t.test(sophisticated  ~ cluster, data = data_barplot)
# not significant

ggplot(data_barplot, aes(cluster, mean_intense), color = cluster) +
  geom_bar(position = "dodge", stat = "identity")

t.test(intense  ~ cluster, data = data_barplot)
# not significant

ggplot(data_barplot, aes(cluster, mean_contemporary), color = cluster) +
  geom_bar(position = "dodge", stat = "identity")

t.test(contemporary  ~ cluster, data = data_barplot)
# significant, contemporary higher in group 1

######

# Summary: 
# gmsi, age, extraversion, neuroticism, agreeableness, openness, mellow, contemporary are significant individual differences between clusters

# not significant: conscientiousness, unpretentious, sophisticated, intense

#######
