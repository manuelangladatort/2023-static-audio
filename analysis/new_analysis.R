################################################################################
# analysis script
################################################################################


################################################################################
# imports
################################################################################
library(tidyverse)
source("utils.R") # functions for analysis and visualization

root_path = "/Users/manu/Library/CloudStorage/OneDrive-GoldsmithsCollege/Documents/github/"


# study 1: original audio
data_clean = read_csv("~/Documents/github/2023-static-audio/data/mel_pref1/data_clean.csv")
data_pids = read_csv("~/Documents/github/2023-static-audio/data/mel_pref1/participants_data_clean.csv")

# pilot (n = 10)
# data_clean = read_csv("~/Documents/github/2023-static-audio/data/pilot_mel_pref1/data_clean.csv")

# study 2: synth f0
data_clean = read_csv(paste0(root_path, "2023-static-audio/data/melf0_pref1/data_clean.csv"))
data_pids = read_csv(paste0(root_path, "2023-static-audio/data/melf0_pref1/participants_data_clean.csv"))


# N participants
length(table(data_clean$participant_id)) # 80
# N stimuli
length(table(data_clean$audio_name)) # 40


data_clean = data_clean %>% 
  separate(audio_name, c("noise", "audio_name"), sep = "-") %>% 
  select(-noise) %>%
  separate(audio_name, c("audio_name", "noise"), sep = "_") %>% 
  select(-noise) %>% 
  mutate(audio_name = as.numeric(audio_name)) 
  
data_clean$audio_name = factor(data_clean$audio_name)


################################################################################
# Diversity of music ratings
################################################################################
ggplot(data_clean, aes(reorder(audio_name, z_answer), z_answer, color=audio_name)) +
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  ylab("Liking rating (z- scored)") +
  xlab("stimuli") +
  theme_classic() +
  theme(legend.position = "none")

ggsave("plots/song_ratings_mainf0.png", height = 10, width = 20, units = "cm")


################################################################################
# Correlations of individual differences
################################################################################
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
  
res1 <- cor.mtest(data_cor[, c(1:22)], conf.level = .95)

testRes = cor.mtest(data_cor[, c(1:22)], conf.level = 0.95)

png("plots/correlation_plot.png", width = 800, height = 800) # Adjust width and height as needed
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
dev.off()


################################################################################
# Clustering approach
################################################################################
library(ggrepel) # avoid overlapping labels in plot
library(factoextra) # cluster analysis
library(ggfortify) # plot PCAs
library(plotly) # plot 3D PCAs


# we want to convert the data to wide format with rows as participants and columns as songs
data_to_clust = data_clean %>%  
  select(participant_id, audio_name, z_answer) %>% 
  # select(-definition, -answer) %>% 
  # relocate(any_of(c("audio_name", "z_answer")), .after = DFS_all_score) %>% 
  pivot_wider(id_cols = participant_id, names_from = "audio_name", values_from = "z_answer")


# number of clusters to extract
k = 3


# KMEANS + PCA approach
x = run_pca(data_to_clust, data_to_clust[,-1], k)

x$pca_res
x$sum_pca
x$elbow
x$silhouette
x$plot_2D_pca

cluster_kmeans = x$plot_2D_pca$data$cluster
  
data_pids$cluster_kmeans <- cluster_kmeans


# Hierarchical clustering
hc_res <- hclust(dist(data_to_clust[,-1]))

# Cut the hierarchical clustering tree into k clusters
cluster_hlc <- cutree(hc_res, k = k)

# Add cluster assignments as a new column to the dataset
data_to_clust$cluster_hlc <- cluster_hlc

# Plot dendrogram
plot(hc_res, hang = -1, cex = 0.6,
     main = "Dendrogram - Hierarchical Clustering", xlab = "Cities", sub = "",
     labels = data_to_clust$participant_id)

# Add clusters to the dendrogram
cluster_colors <- c("red", "blue", "green", "purple", "orange", "brown")

rect.hclust(hc_res, k = k, border = cluster_colors[1:k])

data_pids$cluster_hlc <- cluster_hlc


data_pids %>% 
  group_by(cluster_kmeans) %>% 
  summarise(n = n())

data_pids %>% 
  group_by(cluster_hlc) %>% 
  summarise(n = n())


# given these groups, can we see if they differ in their individual differences?
## calculate the mean and SD of individual differences for each group
## bar plot of groups for each individual differences

data_pids %>% 
  group_by(cluster_kmeans) %>% 
  summarise(n = n(), 
            mean_gold = mean(gmsi_mt, na.rm = T), 
            mean_open = mean(openness, na.rm = T), 
            mean_age = mean(age, na.rm = T),
            mean_world = mean(World, na.rm = T))


# clustering + individual differences 
data_clean_ratings = data_clean %>%  select(participant_id, audio_name:z_answer)

data_combined_ratings = left_join(data_pids, 
                                  data_clean_ratings, by = "participant_id") 


# plots linking by clusters
ggplot(data_combined_ratings, aes(reorder(audio_name, z_answer), z_answer, 
                         color=factor(cluster_kmeans))) +
    geom_boxplot() +
    ylab("Liking rating (z- scored)") +
    xlab("stimuli") +
    theme_classic() +
    theme(legend.position = "bottom")

ggsave("plots/song_ratings_clusters.png", height = 10, width = 20, units = "cm")



data_summary <- data_combined_ratings %>% 
  group_by(cluster_kmeans) %>% 
  summarize_at(
    vars(gmsi_mt, gmsi_score, age, 
         extraversion, openness, agreeableness, neuroticism,
         RefCom:EneRhy, World
         ),
    list(
      mean = ~mean(., na.rm = TRUE),
      # sd = ~sd(., na.rm = TRUE),
      se = ~sd(., na.rm = TRUE) / sqrt(length(.))
    )
  )


# create data long format
data_summary_mean = data_summary %>% 
  select(cluster_kmeans, contains("_mean")) %>% 
  pivot_longer(cols = gmsi_score_mean:World_mean, names_to = "variable", values_to = "mean")
data_summary_se = data_summary %>% 
  select(cluster_kmeans, contains("_se")) %>% 
  pivot_longer(cols = gmsi_score_se:World_se, names_to = "variable", values_to = "se")

data_summary_mean$se = data_summary_se$se


# plot
ggplot(data_summary_mean, aes(x = cluster_kmeans, y = mean, fill = cluster_kmeans)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(x = "Group", y = "Mean Value", fill = "cluster")


######

# Summary: 
## significant individual differences between clusters: 
### gmsi, age, extraversion, neuroticism, IntReb, RefCom, UpbCon, World 

#######


################################################################################
# Linear mixed-effect model
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

