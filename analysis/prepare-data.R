library(tidyverse)
library(misty) #for reverse scoring items

ratings_data = read_csv("~/Documents/github/2023-static-audio/data/mel_pref1/anonymous/data/RatingTrial.csv")

participant_data = read_csv("~/Documents/github/2023-static-audio/data/mel_pref1/anonymous/data/Participant.csv")

response_data = read_csv("~/Documents/github/2023-static-audio/data/mel_pref1/anonymous/data/Response.csv")

# pilot (n = 10)
#ratings_data = read_csv("~/Documents/github/2023-static-audio/data/pilot_mel_pref1/anonymous/data/RatingTrial.csv")

#participant_data = read_csv("~/Documents/github/2023-static-audio/data/pilot_mel_pref1/anonymous/data/Participant.csv")

#response_data = read_csv("~/Documents/github/2023-static-audio/data/pilot_mel_pref1/anonymous/data/Response.csv")

ratings_data_clean = ratings_data %>% 
  filter(failed == "FALSE") %>%  # filter out participants who failed
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
length(table(ratings_data_clean$participant_id))
# N stimuli
length(table(ratings_data_clean$audio_name)) 

ggplot(ratings_data_clean, aes(reorder(audio_name, z_answer), z_answer, color=audio_name)) +
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  ylab("Liking rating (z- scored)") +
  xlab("stimuli") +
  theme_classic() +
  theme(legend.position = "none")

ggsave("song_ratings.png", height = 10, width = 20, units = "cm")
  
participant_data_clean = participant_data %>% 
  filter(failed == "False") %>%  # filter out participants who failed
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
  select(-gmsi, -gmsi_12, -gmsi_2, -gmsi_16, -gmsi_18,-gmsi_21, -gmsi_22, -gmsi_35)

data_combined = right_join(participant_data_clean, ratings_data_clean, by = "participant_id")

response_data_clean = response_data %>% 
  filter(failed == FALSE) %>%  # filter out participants who failed
  select(participant_id, question, answer) 
  
tipi_data = response_data_clean %>%
  filter(str_detect(question, 'tipi')) %>%
  mutate(extraverted = as.numeric(str_extract(str_extract(str_extract(answer, '"Extraverted": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(critical = as.numeric(str_extract(str_extract(str_extract(answer, '"Critical": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(dependable = as.numeric(str_extract(str_extract(str_extract(answer, '"Dependable": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(anxious = as.numeric(str_extract(str_extract(str_extract(answer, '"Anxious": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(open = as.numeric(str_extract(str_extract(str_extract(answer, '"Open": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(reserved = as.numeric(str_extract(str_extract(str_extract(answer, '"Reserved": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(sympathetic = as.numeric(str_extract(str_extract(str_extract(answer, '"Sympathetic": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(disorganized = as.numeric(str_extract(str_extract(str_extract(answer, '"Disorganized": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(calm = as.numeric(str_extract(str_extract(str_extract(answer, '"Calm": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(conventional = as.numeric(str_extract(str_extract(str_extract(answer, '"Conventional": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
  group_by(participant_id) %>%
  mutate(reserved_r = item.reverse(reserved, min = 1, max = 7)) %>%
  mutate(conventional_r = item.reverse(conventional, min = 1, max = 7)) %>%
  mutate(critical_r = item.reverse(critical, min = 1, max = 7)) %>%
  mutate(calm_r = item.reverse(calm, min = 1, max = 7)) %>%
  mutate(disorganized_r = item.reverse(disorganized, min = 1, max = 7)) %>%
  mutate(extraversion = mean(c(extraverted, reserved_r))) %>% 
  mutate(openness = mean(c(open, conventional_r))) %>%
  mutate(agreeableness = mean(c(sympathetic, critical_r))) %>%
  mutate(neuroticism = mean(c(anxious, calm_r))) %>%
  mutate(conscientiousness = mean(c(dependable, disorganized_r))) %>%
  select(participant_id, extraversion, agreeableness, neuroticism, conscientiousness, openness)
  
data_combined =left_join(data_combined, tipi_data, by = "participant_id")
  
stompr_data = response_data_clean %>%
  filter(str_detect(question, 'stompr')) %>%
  mutate(Alternative = as.numeric(str_extract(str_extract(str_extract(answer, '"Alternative": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Bluegrass = as.numeric(str_extract(str_extract(str_extract(answer, '"Bluegrass": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Blues = as.numeric(str_extract(str_extract(str_extract(answer, '"Blues": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Classical = as.numeric(str_extract(str_extract(str_extract(answer, '"Classical": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Country = as.numeric(str_extract(str_extract(str_extract(answer, '"Country": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Dance_Electronica = as.numeric(str_extract(str_extract(str_extract(answer, '"Dance/Electronica": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Folk = as.numeric(str_extract(str_extract(str_extract(answer, '"Folk": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Funk = as.numeric(str_extract(str_extract(str_extract(answer, '"Funk": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Gospel = as.numeric(str_extract(str_extract(str_extract(answer, '"Gospel": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Heavy_Metal = as.numeric(str_extract(str_extract(str_extract(answer, '"Heavy Metal": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(World = as.numeric(str_extract(str_extract(str_extract(answer, '"World": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Jazz = as.numeric(str_extract(str_extract(str_extract(answer, '"Jazz": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(New_Age = as.numeric(str_extract(str_extract(str_extract(answer, '"New Age": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Oldies = as.numeric(str_extract(str_extract(str_extract(answer, '"Oldies": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Opera = as.numeric(str_extract(str_extract(str_extract(answer, '"Opera": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Pop = as.numeric(str_extract(str_extract(str_extract(answer, '"Pop": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Punk = as.numeric(str_extract(str_extract(str_extract(answer, '"Punk": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Rap_hip_hop = as.numeric(str_extract(str_extract(str_extract(answer, '"Rap/hip-hop": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Reggae = as.numeric(str_extract(str_extract(str_extract(answer, '"Reggae": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Religious = as.numeric(str_extract(str_extract(str_extract(answer, '"Religious": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Rock = as.numeric(str_extract(str_extract(str_extract(answer, '"Rock": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Soul_R_B = as.numeric(str_extract(str_extract(str_extract(answer, '"Soul/R&B": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Soundtracks_theme_song = as.numeric(str_extract(str_extract(str_extract(answer, '"Soundtracks/theme song": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
  group_by(participant_id) %>%
  mutate(mellow = mean(c(Dance_Electronica, World, New_Age))) %>%
  mutate(unpretentious = mean(c(Pop, Country, Religious))) %>%
  mutate(sophisticated = mean(c(Blues, Jazz, Bluegrass, Folk, Classical, Gospel, Opera))) %>%
  mutate(intense = mean(c(Rock, Punk, Alternative, Heavy_Metal))) %>%
  mutate(contemporary = mean(c(Rap_hip_hop, Soul_R_B, Funk, Reggae))) %>%
  #select(participant_id, Alternative, Bluegrass, Blues, Classical, Country, Dance_Electronica, Folk, Funk, Gospel, Heavy_Metal, World, Jazz, New_Age, Oldies, Opera, Pop, Punk, Rap_hip_hop, Reggae, Religious, Rock, Soul_R_B, Soundtracks_theme_song)
  select(participant_id, mellow, unpretentious, sophisticated, intense, contemporary, World)
data_combined = left_join(data_combined, stompr_data, by = "participant_id")

DFS_data = response_data_clean %>%
  filter(str_detect(question, 'DFS')) %>%
  mutate(Challenge_skill1 = as.numeric(str_extract(str_extract(str_extract(answer, '"Challenge_skill1": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Action_awareness1 = as.numeric(str_extract(str_extract(str_extract(answer, '"Action_awareness1": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Clear_goals1 = as.numeric(str_extract(str_extract(str_extract(answer, '"Clear_goals1": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Unambiguous_feedback1 = as.numeric(str_extract(str_extract(str_extract(answer, '"Unambiguous_feedback1": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Task_concentration1 = as.numeric(str_extract(str_extract(str_extract(answer, '"Task_concentration1": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Sense_of_control1 = as.numeric(str_extract(str_extract(str_extract(answer, '"Sense_of_control1": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Transformation_of_time1 = as.numeric(str_extract(str_extract(str_extract(answer, '"Transformation_of_time1": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Loss_of_self1 = as.numeric(str_extract(str_extract(str_extract(answer, '"Loss_of_self1": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Autotelic_experience1 = as.numeric(str_extract(str_extract(str_extract(answer, '"Autotelic_experience1": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Challenge_skill2 = as.numeric(str_extract(str_extract(str_extract(answer, '"Challenge_skill2": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Action_awareness2 = as.numeric(str_extract(str_extract(str_extract(answer, '"Action_awareness2": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Clear_goals2 = as.numeric(str_extract(str_extract(str_extract(answer, '"Clear_goals2": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Action_awareness2 = as.numeric(str_extract(str_extract(str_extract(answer, '"Action_awareness2": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Unambiguous_feedback2 = as.numeric(str_extract(str_extract(str_extract(answer, '"Unambiguous_feedback2": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Task_concentration2 = as.numeric(str_extract(str_extract(str_extract(answer, '"Task_concentration2": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
      mutate(Sense_of_control2 = as.numeric(str_extract(str_extract(str_extract(answer, '"Sense_of_control2": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
      mutate(Transformation_of_time2 = as.numeric(str_extract(str_extract(str_extract(answer, '"Transformation_of_time2": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
      mutate(Autotelic_experience2 = as.numeric(str_extract(str_extract(str_extract(answer, '"Autotelic_experience2": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
      mutate(Challenge_skill3 = as.numeric(str_extract(str_extract(str_extract(answer, '"Challenge_skill3": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
      mutate(Action_awareness3 = as.numeric(str_extract(str_extract(str_extract(answer, '"Action_awareness3": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
      mutate(Clear_goals3 = as.numeric(str_extract(str_extract(str_extract(answer, '"Clear_goals3": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
      mutate(Unambiguous_feedback3 = as.numeric(str_extract(str_extract(str_extract(answer, '"Unambiguous_feedback3": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
      mutate(Task_concentration3 = as.numeric(str_extract(str_extract(str_extract(answer, '"Task_concentration3": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
      mutate(Sense_of_control3 = as.numeric(str_extract(str_extract(str_extract(answer, '"Sense_of_control3": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Transformation_of_time3 = as.numeric(str_extract(str_extract(str_extract(answer, '"Transformation_of_time3": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Autotelic_experience3 = as.numeric(str_extract(str_extract(str_extract(answer, '"Autotelic_experience3": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Challenge_skill4 = as.numeric(str_extract(str_extract(str_extract(answer, '"Challenge_skill4": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Action_awareness4 = as.numeric(str_extract(str_extract(str_extract(answer, '"Action_awareness4": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Clear_goals4 = as.numeric(str_extract(str_extract(str_extract(answer, '"Clear_goals4": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Unambiguous_feedback4 = as.numeric(str_extract(str_extract(str_extract(answer, '"Unambiguous_feedback4": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Task_concentration4 = as.numeric(str_extract(str_extract(str_extract(answer, '"Task_concentration4": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
    mutate(Sense_of_control4 = as.numeric(str_extract(str_extract(str_extract(answer, '"Sense_of_control4": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
   mutate(Transformation_of_time4 = as.numeric(str_extract(str_extract(str_extract(answer, '"Transformation_of_time4": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
   mutate(Autotelic_experience4 = as.numeric(str_extract(str_extract(str_extract(answer, '"Autotelic_experience4": "[0-9]'), ': "[0-9]'), '[0-9]'))) %>%
  group_by(participant_id) %>%
  mutate(Challenge_skill = mean(c(Challenge_skill1, Challenge_skill2, Challenge_skill3, Challenge_skill4))) %>% 
  mutate(Action_awareness = mean(c(Action_awareness1, Action_awareness2, Action_awareness3, Action_awareness4))) %>%
  mutate(Clear_goals = mean(c(Clear_goals1, Clear_goals2, Clear_goals3, Clear_goals4))) %>%
  mutate(Unambiguous_feedback = mean(c(Unambiguous_feedback1, Unambiguous_feedback2, Unambiguous_feedback3, Unambiguous_feedback4))) %>%
  mutate(Task_concentration = mean(c(Task_concentration1, Task_concentration2, Task_concentration3, Task_concentration4))) %>%
  mutate(Sense_of_control = mean(c(Sense_of_control1, Sense_of_control2, Sense_of_control3, Sense_of_control4))) %>%
  mutate(Transformation_of_time = mean(c(Transformation_of_time1, Transformation_of_time2, Transformation_of_time3, Transformation_of_time4))) %>%
  mutate(Autotelic_experience = mean(c(Autotelic_experience1, Autotelic_experience2, Autotelic_experience3, Autotelic_experience4))) %>%
  mutate(DFS_all_score = mean(c(Challenge_skill, Action_awareness, Clear_goals, Unambiguous_feedback, Task_concentration, Sense_of_control, Transformation_of_time, Autotelic_experience))) %>%  
  select(participant_id, Challenge_skill, Action_awareness, Clear_goals, Unambiguous_feedback, Task_concentration, Sense_of_control, Transformation_of_time, Autotelic_experience, DFS_all_score)

data_combined = left_join(data_combined, DFS_data, by = "participant_id")

#Pilot n=10
#write.csv(data_combined, "~/Documents/github/2023-static-audio/data/pilot_mel_pref1/data_clean.csv", row.names=FALSE) 

write.csv(data_combined, "~/Documents/github/2023-static-audio/data/mel_pref1/data_clean.csv", row.names=FALSE) 
