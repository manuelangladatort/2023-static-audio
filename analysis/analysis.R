library(tidyverse)

# pilot (n = 10)
# ratings_data = read_csv("~/Documents/github/2023-static-audio/data/pilot_mel_pref1/anonymous/data/RatingTrial.csv")
ratings_data = read_csv("~/Documents/github/2023-static-audio/data/mel_pref1/anonymous/data/RatingTrial.csv")

participant_data = read_csv("~/Documents/github/2023-static-audio/data/anonymous/data/Participant.csv")

ratings_data_clean = ratings_data %>% 
  filter(failed == FALSE) %>%  # filter out participants who failed
  filter(is_repeat_trial == "FALSE") %>%  # filter out repeated trials
  select(participant_id, definition, audio_name, answer) %>% 
  # extract audio name
  mutate(audio_name = as.factor(as.numeric(str_extract(audio_name, "\\d+")))) %>% 
  # count number trials per participant 
  group_by(participant_id) %>% 
  mutate(n_ratings = n()) %>% 
  # z-score ratings per particiapnt
  mutate(z_answer = scale(answer)) %>% 
  # exclude participants with less than <40 trials
  filter(n_ratings >= 40)
  

# N participants
length(table(ratings_data_clean$participant_id)) # 80
# N stimuli
length(table(ratings_data_clean$audio_name)) # 40

ggplot(ratings_data_clean, aes(reorder(audio_name, z_answer), z_answer, color=audio_name)) +
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  ylab("Liking rating (z- scored)") +
  xlab("stimuli") +
  theme_classic() +
  theme(legend.position = "none")

ggsave("song_ratings_main.png", height = 10, width = 20, units = "cm")
  
participant_data_clean = participant_data %>% 
  filter(failed == FALSE) %>%  # filter out participants who failed
  select(id, gender, age, gmsi) %>%
  rename(participant_id = id) %>%
  mutate(gmsi_12 = as.numeric(str_extract(str_extract(str_extract(gmsi, '"q_12": [0-9]'), ': [0-9]'), '[0-9]'))) %>%
  mutate(gmsi_2 = as.numeric(str_extract(str_extract(str_extract(gmsi, '"q_02": [0-9]'), ': [0-9]'), '[0-9]'))) %>%
  mutate(gmsi_16 = as.numeric(str_extract(str_extract(str_extract(gmsi, '"q_16": [0-9]'), ': [0-9]'), '[0-9]'))) %>%
  mutate(gmsi_18 = as.numeric(str_extract(str_extract(str_extract(gmsi, '"q_18": [0-9]'), ': [0-9]'), '[0-9]'))) %>%
  mutate(gmsi_21 = as.numeric(str_extract(str_extract(str_extract(gmsi, '"q_21": [0-9]'), ': [0-9]'), '[0-9]'))) %>%
  mutate(gmsi_22 = as.numeric(str_extract(str_extract(str_extract(gmsi, '"q_22": [0-9]'), ': [0-9]'), '[0-9]'))) %>%
  mutate(gmsi_35 = as.numeric(str_extract(str_extract(str_extract(gmsi, '"q_35": [0-9]'), ': [0-9]'), '[0-9]'))) %>%
  group_by(participant_id) %>%
  mutate(gmsi_score = rowMeans(cbind(gmsi_12, gmsi_2, gmsi_16, gmsi_18, gmsi_21, gmsi_22, gmsi_35))) %>%
  select(-gmsi)

data_combined = merge(x = participant_data_clean, y = ratings_data_clean, by = "participant_id", all = TRUE)
