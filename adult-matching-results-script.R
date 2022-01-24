library(tidyverse)
library(data.table)
library(dplyr)
library(readr)
library(ggplot2)
library(ggpubr)
library(rstatix)

# This code reads in pcibex result files and computes the number of correct answers for the adult verb matching study.
# Author: Abimael Hernandez-Jimenez

# Assumptions:
# It assumes all annotation files are .csv, and sit in you current working directory

getwd()

read.pcibex <- function(filepath, auto.colnames=TRUE, fun.col=function(col,cols){cols[cols==col]<-paste(col,"Ibex",sep=".");return(cols)}) {
  n.cols <- max(count.fields(filepath,sep=",",quote=NULL),na.rm=TRUE)
  if (auto.colnames){
    cols <- c()
    con <- file(filepath, "r")
    while ( TRUE ) {
      line <- readLines(con, n = 1, warn=FALSE)
      if ( length(line) == 0) {
        break
      }
      m <- regmatches(line,regexec("^# (\\d+)\\. (.+)\\.$",line))[[1]]
      if (length(m) == 3) {
        index <- as.numeric(m[2])
        value <- m[3]
        if (is.function(fun.col)){
          cols <- fun.col(value,cols)
        }
        cols[index] <- value
        if (index == n.cols){
          break
        }
      }
    }
    close(con)
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=cols))
  }
  else{
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=seq(1:n.cols)))
  }
}

#-- video first data --#

#setwd("~/Penn/studies/adult-matching/adult_matching_video_first_2_results")

# VIDEO FIRST WITH PRACTICE "./adult_matching_video_first_results/"
# VIDEO FIRST NO PRACTICE "./adult_matching_video_first_2_results/"

#VERB FIRST WITH PRACTICE  "./adult_matching_verb_first_results/"
#VERB FIRST NO PRACTICE "./adult_matching_verb_first_2_results/"

files = list.files(path = "./adult_matching_verb_first_2_results/",pattern="*.csv")
files

num_of_files <- length(files)

for (curr_file_num in 1:num_of_files) {
  dframe <- read.pcibex(file = paste("./adult_matching_verb_first_2_results/",files[curr_file_num], sep=""))  
  
  # now create a column with the file name in every row
  dframe$filename <- files[curr_file_num]
  
  # create a file name with "expanded_" at start
  myfilename<-paste("cleaned", files[curr_file_num], sep="_")
  
  # add condition column
  dframe$condition <- c("verb_first_2")
  
  # save the file
  write.csv( file = paste("./verb_first_2_cleaned/",myfilename,sep=""),
             dframe)
  
  #combine all the cleaned csv files
  verb_first_no_practice <- list.files(path="./verb_first_2_cleaned/", full.names = TRUE) %>% 
    lapply(read_csv) %>% 
    bind_rows 
  
  verb_first_no_practice$...1 <- NULL
  
  verb_first_no_practice <- filter(verb_first_no_practice, PROLIFIC_PID != "NULL")
  
  verb_first_no_practice <- verb_first_no_practice %>%
    mutate(
      predicate_pair = case_when(
        (audio_name == "choosing_or_matching") ~ "choose_match",
        (audio_name == "matching_or_choosing") ~ "choose_match",
        (audio_name == "pushing_or_separating") ~ "push_separate",
        (audio_name == "separating_or_pushing") ~ "push_separate",
        (audio_name == "fighting_or_punching") ~ "punch_fight",
        (audio_name == "punching_or_fighting") ~ "punch_fight",
        (audio_name == "hanging_or_connecting") ~ "hang_connect",
        (audio_name == "connecting_or_hanging") ~ "hang_connect",
        (audio_name == "pulling_or_joining") ~ "pull_join",
        (audio_name == "joining_or_pulling") ~ "pull_join",
        (audio_name == "tickling_or_hugging") ~ "tickle_hug",
        (audio_name == "hugging_or_tickling") ~ "tickle_hug",
        (audio_name == "marrying_or_inviting") ~ "invite_marry",
        (audio_name == "inviting_or_marrying") ~ "invite_marry",
        (audio_name == "grabbing_or_sharing") ~ "grab_share",
        (audio_name == "sharing_or_grabbing") ~ "grab_share",
        (audio_name == "licking_or_kissing") ~ "lick_kiss",
        (audio_name == "kissing_or_licking") ~ "lick_kiss",
        (audio_name == "teaching_or_meeting") ~ "teach_meet",
        (audio_name == "meeting_or_teaching") ~ "teach_meet"
      ))
  
  write.csv(verb_first_no_practice,"./verb_first_2_cleaned_all/verb_first_no_practice.csv")
  
}

# add scoring column after we combine all of them.
# video dfs don't have an audio_1 column, but I think we can add one when we merge the dfs. They would get populated with NA's for the video dfs.
# dfs with practice have character values in the group column which interferes with the int values of the dfs with no practice.

video_first_with_practice <- read.csv("./video_first_1_cleaned_all/video_first_with_practice.csv")
colnames(video_first_with_practice)
video_first_with_practice$X <- NULL

video_first_with_practice_only <- filter(video_first_with_practice, Label =="practice")

#check overall score
video_first_with_practice_only_scoring <- video_first_with_practice_only %>%
  filter(Label == "practice") %>%
  group_by(PROLIFIC_PID) %>%
  summarise( correct = sum(Value[Parameter=="Choice"] == target[Parameter=="Choice"]))
# all correct

verb_first_with_practice <- read.csv("./verb_first_1_cleaned_all/verb_first_with_practice.csv")
verb_first_with_practice_only <- filter(verb_first_with_practice, Label == "practice")

#check overall score
verb_first_with_practice_only_scoring <- verb_first_with_practice_only %>%
  filter(Label == "practice") %>%
  group_by(PROLIFIC_PID) %>%
  summarise( correct = sum(Value[Parameter=="Choice"] == target[Parameter=="Choice"]))
#two pax got one wrong 	6121be9a77c602b1d877d7f0 and 5ddae85b393bd3a44d7cbaaa

length(unique(verb_first_with_practice$PROLIFIC_PID)) #32
#will remove two subjects from this df

#removing X column
colnames(verb_first_with_practice)
colnames(verb_first_with_practice_only)
verb_first_with_practice$X <- NULL
verb_first_with_practice_only$X <- NULL

length(unique(video_first_with_practice$PROLIFIC_PID)) #31

#now subset by test df
video_first_with_practice_test_only <- filter(video_first_with_practice, Label =="test")
length(video_first_with_practice_test_only$predicate_pair) #3137, 31 columns

verb_first_with_practice_test_only <- filter(verb_first_with_practice, Label =="test")
verb_first_with_practice_test_only$X <- NULL
length(verb_first_with_practice_test_only$predicate_pair) #2135, 32 columns

#removing people that didn't answer all three practice trials correctly.
length(unique(verb_first_with_practice$PROLIFIC_PID)) #32
verb_first_with_practice_test_only <- filter(verb_first_with_practice_test_only,PROLIFIC_PID != "6121be9a77c602b1d877d7f0")
length(unique(verb_first_with_practice_test_only$PROLIFIC_PID)) #31
verb_first_with_practice_test_only <- filter(verb_first_with_practice_test_only,PROLIFIC_PID != "5ddae85b393bd3a44d7cbaaa")
length(unique(verb_first_with_practice_test_only$PROLIFIC_PID)) #30
length(verb_first_with_practice_test_only$predicate_pair) #2009, 32 columns

#importing dfs - no practice
verb_first_no_practice <- read.csv("./verb_first_2_cleaned_all/verb_first_no_practice.csv")
colnames(verb_first_no_practice)
verb_first_no_practice$X <- NULL

video_first_no_practice <- read.csv("./video_first_2_cleaned_all/video_first_no_practice.csv")
colnames(video_first_no_practice)
video_first_no_practice$X <- NULL

#now isolating the test trials in the dfs with no practice.
#verb first no practice
verb_first_no_practice_test_only <- filter(verb_first_no_practice, Label =="test")
length(verb_first_no_practice_test_only$predicate_pair) #2025, 32 columns

#video first no practice
video_first_no_practice_test_only <- filter(video_first_no_practice, Label =="test")
length(video_first_no_practice_test_only$predicate_pair) #2816, 31 columns

#save the test only files.
write.csv(video_first_with_practice_test_only, "./video_first_1_cleaned_all/video_first_with_practice_test_only.csv")
write.csv(video_first_no_practice_test_only, "./video_first_2_cleaned_all/video_first_no_practice_test_only.csv")

write.csv(verb_first_with_practice_test_only, "./verb_first_1_cleaned_all/verb_first_with_practice_test_only.csv")
write.csv(verb_first_no_practice_test_only, "./verb_first_2_cleaned_all/verb_first_no_practice_test_only.csv")

# video_first_no_practice_test_only: 2816 + verb_first_no_practice_test: 2025 + 2009 + 3137 = 9,987 expected rows after merging

typeof(video_first_no_practice_test_only$group) #double
typeof(video_first_with_practice_test_only$group) #character
typeof(verb_first_no_practice_test_only$group) #double
typeof(verb_first_with_practice_test_only$group) #character

video_first_with_practice_test_only$group <- as.double(video_first_with_practice_test_only$group)
verb_first_with_practice_test_only$group <- as.double(verb_first_with_practice_test_only$group)

typeof(video_first_no_practice_test_only$trial_number) #double
typeof(video_first_with_practice_test_only$trial_number) #character
typeof(verb_first_no_practice_test_only$trial_number) #double
typeof(verb_first_with_practice_test_only$trial_number) #character

video_first_with_practice_test_only$trial_number <- as.double(video_first_with_practice_test_only$trial_number)
verb_first_with_practice_test_only$trial_number <- as.double(verb_first_with_practice_test_only$trial_number)

#add audio_1 column for video condition.

video_first_no_practice_test_only$audio_1 <- NA
colnames(video_first_no_practice_test_only)
video_first_with_practice_test_only$audio_1 <- NA
colnames(video_first_with_practice_test_only)

merged_df <- dplyr::bind_rows(list(video_first_no_practice_test_only=video_first_no_practice_test_only,video_first_with_practice_test_only=video_first_with_practice_test_only,verb_first_no_practice_test_only=verb_first_no_practice_test_only,verb_first_with_practice_test_only=verb_first_with_practice_test_only), .id = 'source')
#9987 rows, 33 columns
write.csv(merged_df,"./merged_data/merged_df.csv")

clean_test_df <- subset(merged_df, select = c("Parameter","Value","EventTime","PROLIFIC_PID","target","target_order","random_order","group","audio_name","video_name","audio_first_predicate_type","video_type","trial_number","predicate_pair","condition","source","filename"))
#9987, 17

clean_test_df <- clean_test_df %>% mutate(condition=recode(condition, 
                                                           `verb_first_1`="verb_first_with_practice",
                                                           `verb_first_2`="verb_first_no_practice",
                                                           `video_first_1`="video_first_with_practice",
                                                           `video_first_2`="video_first_no_practice"))

clean_test_df <- filter(clean_test_df, Parameter == "_Trial_" | Parameter == "Choice")
clean_test_df <- filter(clean_test_df, Value != "End")
clean_test_df <- filter(clean_test_df, Parameter == "Choice")
clean_test_df$score <- as.integer(clean_test_df$Value[clean_test_df$Parameter=="Choice"] == clean_test_df$target[clean_test_df$Parameter=="Choice"])

#remove any participants with less than 10 trials
clean_test_df_num_of_trials <- clean_test_df %>%
  group_by(PROLIFIC_PID) %>%
  summarise( number_of_trials = length(trial_number))
#23 participants with less than 10 trials! 22 of them submitted 9 trials and 1 submitted 8 trials.

typeof(clean_test_df_num_of_trials$number_of_trials)
less_than_10_trials <- filter(clean_test_df_num_of_trials, number_of_trials < 10)

#filter out the participants with less than 10 trials
length(unique(clean_test_df$PROLIFIC_PID)) #122
clean_test_df_full <- clean_test_df %>%
  filter(!PROLIFIC_PID %in% less_than_10_trials$PROLIFIC_PID)
length(unique(clean_test_df_full$PROLIFIC_PID)) #99
length(clean_test_df_full$PROLIFIC_PID) #990

#adds a summed score column
clean_test_df_full <- clean_test_df_full %>%
  group_by(PROLIFIC_PID) %>%
  mutate(
    summed_score = sum(score)
  )

write.csv(clean_test_df_full, "./merged_data/clean_test_df_full.csv")

mean(clean_test_df_full$score) #0.5141414
mean(clean_test_df_full$summed_score) #5.141414

test_df_by_random_order <- group_by(clean_test_df_full, random_order) %>%
  summarise(
    number_of_participants = n()/10,
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
  )
# count divide by 10 to get number of participants in a list/random order
sum(test_df_by_random_order$number_of_participants) #99
write.csv(test_df_by_random_order,"./Output/summary-stats/test_df_by_random_order.csv")


test_df_by_condition <- group_by(clean_test_df_full, condition) %>%
  summarise(
    number_of_participants = length(unique(PROLIFIC_PID)),
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    min = min(summed_score),
    max = max(summed_score),
  )

test_df_by_pred_pair <- group_by(clean_test_df_full, predicate_pair) %>%
  summarise(
    count = n(),
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    min = min(summed_score),
    max = max(summed_score),
  )

test_df_by_video <- clean_test_df_full %>% group_by(video_name) %>%
  summarise(
    count = n(),
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE)
  )
test_df_by_video
write.csv(test_df_by_video,"./Output/summary-stats/test_df_by_video.csv")

#next split these up between video/no video conditions.
test_df_by_condition_and_pred_pair <- group_by(clean_test_df_full, condition,predicate_pair) %>%
  summarise(
    count = n(),
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    min = min(summed_score),
    max = max(summed_score),
  )


#First compare video vs audio first conditions. Then compare practice vs no practice to see if there are any differences.

#what lists were the people that we excluded in?

#random order and subjects
test_df_by_random_order_and_subjects <- clean_test_df_full %>%
  group_by(random_order,PROLIFIC_PID) %>%
  summarise(Mean = mean(score),
            SD = sd(score),
  )
test_df_by_random_order_and_subjects
write.csv(test_df_by_random_order_and_subjects,"./Output/summary-stats/test_df_by_random_order_and_subjects.csv")

#group/list and subjects
test_df_by_group_and_subjects <- clean_test_df_full %>%
  group_by(group,PROLIFIC_PID) %>%
  summarise(Mean = mean(score),
            SD=sd(score)
  )
test_df_by_group_and_subjects
write.csv(test_df_by_group_and_subjects,"./Output/summary-stats/test_df_by_group_and_subjects.csv")

#video name and predicate_pair
test_df_by_video_and_predicate_pair <- clean_test_df_full %>%
  group_by(video_name,predicate_pair) %>%
  summarise(Mean = mean(score),
            SD=sd(score)
  )
test_df_by_video_and_predicate_pair
write.csv(test_df_by_video_and_predicate_pair,"./Output/summary-stats/test_df_by_video_and_predicate_pair.csv")


#0 for assist?
checking_assist_grab_share <- filter(clean_test_df_full, video_name == "assist")
checking_assist_grab_share <- filter(checking_assist_grab_share, predicate_pair == "grab_share")
checking_assist_grab_share #19 different participants, find new video to pair with grab_share

length(unique(checking_assist_grab_share$PROLIFIC_PID)) #19 people got this pairing wrong

# test_df_by_condition_and_pred_pair <- clean_test_df_full %>%
#   group_by(condition,predicate_pair) %>%
#   summarise(Mean = mean(score),
#             SD=sd(score))
# 
# clean_test_df_full %>%
#   group_by(predicate_pair,condition) %>%
#   summarise(Mean = mean(score)) %>%
#   ggplot(aes(x = predicate_pair, y = Mean, fill = condition)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   theme_bw() +
#   scale_fill_manual("conditions", values = c("verb_first_1" = "coral1", "verb_first_2" = "coral4", "video_first_1" = "paleturquoise2", "video_first_2" = "paleturquoise3" ))
#   labs(
#     x = "predicate_pair",
#     y = "Average score",
#     title = paste(
#       "Summary Based on Condition"
#     )
#   )
# 
# clean_test_df_full_recode <- clean_test_df_full
# 
# # clean_test_df_full_recode <- clean_test_df_full %>% mutate(condition=recode(condition, 
# #                          `verb_first_1`="verb_first_with_practice",
# #                          `verb_first_2`="verb_first_no_practice",
# #                          `video_first_1`="video_first_with_practice",
# #                          `video_first_2`="video_first_no_practice"))
# 
# clean_test_df_full_recode %>%
#   group_by(predicate_pair,condition) %>%
#   summarise(Mean = mean(score)) %>%
#   ggplot(aes(x = predicate_pair, y = Mean, fill = condition)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   theme_bw() +
#   scale_fill_manual("conditions", values = c("verb_first_with_practice" = "coral1", "verb_first_no_practice" = "coral4", "video_first_with_practice" = "paleturquoise2", "video_first_no_practice" = "paleturquoise3" ))
# labs(
#   x = "predicate_pair",
#   y = "Average score",
#   title = paste(
#     "Summary Based on Condition"
#   )
# )

pred_pairs_and_conditions_graph <- clean_test_df_full %>%
  group_by(predicate_pair,condition) %>%
  summarise(Mean = mean(score)) %>%
  ggplot(aes(x = predicate_pair, y = Mean, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  scale_fill_manual("conditions", values = c("verb_first_with_practice" = "coral1", "verb_first_no_practice" = "coral4", "video_first_with_practice" = "paleturquoise2", "video_first_no_practice" = "paleturquoise3" )) +
  labs(
    x = "Predicate pairs",
    y = "Average score",
    title = paste(
      "Mean score for each predicate pair and condition"
    )
  )
pred_pairs_and_conditions_graph
ggsave("./Output/graphs/pred_pairs_and_conditions_graph.png")

#--
pred_pairs_and_conditions_graph <- clean_test_df_full %>%
  group_by(condition) %>%
  summarise(Mean = mean(score)) %>%
  ggplot(aes(x = condition, y = Mean)) +
  geom_bar(stat = "identity", position = "dodge") + ylim(0,1) +
  theme_bw() +
  scale_fill_manual("conditions", values = c("verb_first_with_practice" = "coral1", "verb_first_no_practice" = "coral4", "video_first_with_practice" = "paleturquoise2", "video_first_no_practice" = "paleturquoise3" )) +
  labs(
    x = "Predicate pairs",
    y = "Average score",
    title = paste(
      "Mean score for each condition"
    )
  )
pred_pairs_and_conditions_graph
ggsave("./Output/graphs/conditions_graph.png")

# -- 
install.packages("sjPlot")
library(sjPlot)
tab_itemscale(collapsed_test$predicate_pair)

library(ShinyItemAnalysis)
startShinyItemAnalysis()

#--
collapsed_test <- clean_test_df_full

#collapsing verb and video conditions
collapsed_test <- collapsed_test %>%
  mutate(
    collapsed_conditions = case_when(
      (condition == "verb_first_with_practice") ~ "verb_first",
      (condition == "verb_first_no_practice") ~ "verb_first",
      (condition == "video_first_with_practice") ~ "video_first",
      (condition == "video_first_no_practice") ~ "video_first"
    ))

collapsed_predicate_pairs <- collapsed_test %>%
  group_by(predicate_pair,collapsed_conditions) %>%
  summarise(Mean = mean(score)) %>%
  ggplot(aes(x = predicate_pair, y = Mean, fill = collapsed_conditions)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  scale_fill_manual("conditions", values = c("verb_first" = "coral1", "video_first" = "paleturquoise3" )) +
  labs(
    x = "Predicate pairs",
    y = "Average score",
    title = paste(
      "Collapsed mean score for each predicate pair"
    )
  )
collapsed_predicate_pairs
ggsave("./Output/graphs/collapsed_predicate_pairs.png")

#predicate pair means
test_df_by_predicate_pair <- clean_test_df_full %>%
  group_by(predicate_pair) %>%
  summarise(Mean = mean(score),
            SD=sd(score)
  )
test_df_by_predicate_pair
write.csv(test_df_by_predicate_pair,"./Output/summary-stats/test_df_by_predicate_pair.csv")

#collapsed conditions mean
collapsed_conditions_mean <- collapsed_test %>%
  group_by(collapsed_conditions) %>%
  summarise(Mean = mean(score),
            SD=sd(score)
  )
collapsed_conditions_mean

#prep df for t test
predicate_pair_and_number_correct <- clean_test_df_full %>%
  select(predicate_pair, summed_score)
predicate_pair_and_number_correct

#test for significance
t.test(data = predicate_pair_and_number_correct, summed_score ~ predicate_pair)
# Error in t.test.formula(data = predicate_pair_and_number_correct, summed_score ~  : 
#                           grouping factor must have exactly 2 levels

small_test_df <- collapsed_test %>%
  group_by(PROLIFIC_PID,random_order,group,condition,summed_score,collapsed_conditions,source,filename) %>%
  ungroup() %>%
  select(PROLIFIC_PID,random_order,group,condition,summed_score,collapsed_conditions,source,filename) %>%
  distinct()

t.test(data=small_test_df, summed_score ~ collapsed_conditions)

write.csv(collapsed_test,"./merged_data/collapsed_test.csv")

random_order_and_pred_pairs <- collapsed_test %>%
  group_by(predicate_pair,random_order) %>%
  summarise(Mean = mean(score)) %>%
  ggplot(aes(x = predicate_pair, y = Mean, fill = random_order)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  #scale_fill_manual("conditions", values = c("verb_first" = "coral1", "video_first" = "paleturquoise3" )) +
  labs(
    x = "Predicate pairs",
    y = "Average score",
    title = paste(
      "Collapsed mean score for each predicate pair"
    )
  )
random_order_and_pred_pairs
ggsave("./Output/graphs/random_order_and_pred_pairs.png")

#collapse these too?

test_df_by_collapsed_condition <- clean_test_df_full %>%
  group_by(collapsed_conditions) %>%
  summarise(Mean = mean(score),
            SD=sd(score)
  )

t.test(data=collapsed_conditions_mean, Mean ~ collapsed_conditions)


main_df <- collapsed_test

subject_score_main_df <- small_test_df

colnames(subject_score_main_df)
subject_score_main_df$subject_means <- subject_score_main_df$summed_score/10
t.test(data=subject_score_main_df, subject_means ~ collapsed_conditions)

collapsed_test <- collapsed_test %>%
  mutate(
    subject_means = summed_score/10
  )

collapsed_test <- collapsed_test %>%
  mutate(
    collapsed_random_order = case_when(
      (random_order == "A1") ~ "A",
      (random_order == "A2") ~ "A",
      (random_order == "A3") ~ "A",
      (random_order == "A4") ~ "A",
      (random_order == "B1") ~ "B",
      (random_order == "B2") ~ "B",
      (random_order == "B3") ~ "B",
      (random_order == "B4") ~ "B",
      (random_order == "C1") ~ "C",
      (random_order == "C2") ~ "C",
      (random_order == "C3") ~ "C",
      (random_order == "C4") ~ "C",
      (random_order == "D1") ~ "D",
      (random_order == "D2") ~ "D",
      (random_order == "D3") ~ "D",
      (random_order == "D4") ~ "D",
      (random_order == "E1") ~ "E",
      (random_order == "E2") ~ "E",
      (random_order == "E3") ~ "E",
      (random_order == "E4") ~ "E",
      (random_order == "F1") ~ "F",
      (random_order == "F2") ~ "F",
      (random_order == "F3") ~ "F",
      (random_order == "F4") ~ "F",
      (random_order == "G1") ~ "G",
      (random_order == "G2") ~ "G",
      (random_order == "G3") ~ "G",
      (random_order == "G4") ~ "G",
      (random_order == "H1") ~ "H",
      (random_order == "H2") ~ "H",
      (random_order == "H3") ~ "H",
      (random_order == "H4") ~ "H",
      
    ))

# #collapsed conditions mean
# predicate_pair_means <- collapsed_test %>%
#   group_by(predicate_pair) %>%
#   summarise(Mean = mean(score),
#             SD=sd(score)
#   )
# collapsed_conditions_mean

collapsed_test <- collapsed_test %>%
  group_by(predicate_pair) %>% 
  mutate(
    predicate_pair_means = case_when(
      (predicate_pair == "choose_match") ~ 0.5353535,
      (predicate_pair == "grab_share") ~ 0.6767677,
      (predicate_pair == "hang_connect") ~ 0.4242424,
      (predicate_pair == "invite_marry") ~ 0.4444444,
      (predicate_pair == "lick_kiss") ~ 0.5151515,
      (predicate_pair == "pull_join") ~ 0.3636364,
      (predicate_pair == "punch_fight") ~ 0.6060606,
      (predicate_pair == "push_separate") ~ 0.4747475,
      (predicate_pair == "teach_meet") ~ 0.4848485,
      (predicate_pair == "tickle_hug") ~ 0.6161616,
    )
  )

collapsed_random_order_and_pred_pairs <- collapsed_test %>%
  group_by(predicate_pair,collapsed_random_order) %>%
  summarise(Mean = mean(score)) %>%
  ggplot(aes(x = predicate_pair, y = Mean, fill = collapsed_random_order)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  #scale_fill_manual("conditions", values = c("verb_first" = "coral1", "video_first" = "paleturquoise3" )) +
  labs(
    x = "Predicate pairs",
    y = "Average score",
    title = paste(
      "Collapsed mean score for random orders"
    )
  )
collapsed_random_order_and_pred_pairs

grouping_collapsed_random_order_and_predicate_pair <- collapsed_test %>%
  group_by(predicate_pair,collapsed_random_order) %>%
  summarise(Mean = mean(score)) %>%
  ggplot(aes(x = predicate_pair, y = Mean, fill = collapsed_random_order)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  #scale_fill_manual("conditions", values = c("verb_first" = "coral1", "video_first" = "paleturquoise3" )) +
  labs(
    x = "Predicate pairs",
    y = "Average score",
    title = paste(
      "Collapsed mean score for random orders"
    )
  )
grouping_collapsed_random_order_and_predicate_pair
ggsave("./Output/graphs/grouping_collapsed_random_order_and_predicate_pair.png")

#so if I got invite/marry, pull/join, in list A then I always matched the correct predicate and video?
# and lick/kiss for random order C?

#collapsed random order means
collapsed_random_order_means <- collapsed_test %>%
  group_by(collapsed_random_order) %>%
  summarise(Mean = mean(score),
            SD=sd(score)
  )
collapsed_random_order_means

#checking lick/kiss for random order C
subset_lick_kiss_and_list_c <- filter(collapsed_test, predicate_pair == "lick_kiss" & collapsed_random_order == "C")
length(unique(subset_lick_kiss_and_list_c$PROLIFIC_PID))

#collapsed_test$predicate_pair_means <- NULL

#checking invite/marry, pull/join, and  for random order C
grouping_collapsed_random_order_and_predicate_pair_df <- collapsed_test %>%
  group_by(predicate_pair,collapsed_random_order, video_name) %>%
  summarise(Mean = mean(score), n = n())
grouping_collapsed_random_order_and_predicate_pair_df

write.csv(grouping_collapsed_random_order_and_predicate_pair_df,"./merged_data/grouping_collapsed_random_order_and_predicate_pair_df.csv")
grouping_collapsed_random_order_and_predicate_pair_df %>% 
  # desc orders from largest to smallest
  arrange(desc(Mean))

#adding number of subjects in those conditions
# collapsed_test %>% group_by(predicate_pair, collapsed_random_order) %>% summarise(n = n())

getwd()

random_clean_test_df_full <- read.csv("./random_preprocess/random_clean_test_df_full.csv")
length(random_clean_test_df_full)
colnames(random_clean_test_df_full)
length(collapsed_test)
colnames(collapsed_test)

collapsed_test$collapsed_random_order <- NULL

#adding subject means to random_clean_test_df_full
random_num_of_trials <- random_clean_test_df_full %>%
  group_by(PROLIFIC_PID) %>%
  summarise( number_of_trials = length(trial_number))

random_clean_test_df_full <- random_clean_test_df_full %>%
  mutate(
    subject_means = summed_score/10
  )

length(random_clean_test_df_full)
colnames(random_clean_test_df_full)
length(collapsed_test)

#remove collapsed conditions to get the same number of columns as random df
collapsed_test$collapsed_conditions <- NULL

colnames(random_clean_test_df_full)
colnames(collapsed_test)
random_clean_test_df_full$X <- NULL
random_clean_test_df_full$predicate_order <- NULL
collapsed_test$random_order <- NULL

colnames(random_clean_test_df_full)
colnames(collapsed_test)

collapsed_test$source <-NULL

main_df <- rbind(collapsed_test,random_clean_test_df_full)

# random_pair_score <- random_clean_test_df_full %>%
#   select(predicate_pair,video_type,score) %>%
#   group_by(predicate_pair,video_type) %>%
#   mutate(
#     pair_summed_score = sum(score),
#     pair_means = mean(score)
#   )

#mean score for each predicate pair by video condition
random_pair_score <- random_clean_test_df_full %>%
  group_by(predicate_pair,video_type) %>%
  summarise(mean = mean(score))

random_pair_score <- random_pair_score %>% arrange(desc(video_type))

random_pair_score <- random_pair_score %>%
  mutate(
    rating_diff = case_when(
      (predicate_pair == "choose_match") ~ 1.71,
      (predicate_pair == "push_separate") ~ 1.74,
      (predicate_pair == "punch_fight") ~ 1.61,
      (predicate_pair == "hang_connect") ~ 1.63,
      (predicate_pair == "pull_join") ~ 1.30,
      (predicate_pair == "tickle_hug") ~ 2.04,
      (predicate_pair == "invite_marry") ~ 2.12,
      (predicate_pair == "grab_share") ~ 1.38,
      (predicate_pair == "lick_kiss") ~ 2.00,
      (predicate_pair == "teach_meet") ~ 2.12
    ))

#-- main df pair score --


#mean score for each predicate pair by video condition
main_df_pair_score <- main_df %>%
  group_by(predicate_pair,video_type) %>%
  summarise(mean = mean(score))

main_df_pair_score <- main_df_pair_score %>% arrange(desc(video_type))

main_df_pair_score <- main_df_pair_score %>%
  mutate(
    rating_diff = case_when(
      (predicate_pair == "choose_match") ~ 1.71,
      (predicate_pair == "push_separate") ~ 1.74,
      (predicate_pair == "punch_fight") ~ 1.61,
      (predicate_pair == "hang_connect") ~ 1.63,
      (predicate_pair == "pull_join") ~ 1.30,
      (predicate_pair == "tickle_hug") ~ 2.04,
      (predicate_pair == "invite_marry") ~ 2.12,
      (predicate_pair == "grab_share") ~ 1.38,
      (predicate_pair == "lick_kiss") ~ 2.00,
      (predicate_pair == "teach_meet") ~ 2.12
    ))

# ggplot(random_pair_score, aes(rating_diff, mean, col = video_type)) + 
#   geom_point(size = 3) + # change size and color
#   labs(y = "Mean score", x = "Symmetry rating difference") + # rename axes
#   scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.00)) + # y axis limits/range 
#   scale_x_continuous(limits = c(0, 2.5)) + # x axis limits/range 
#   geom_smooth(method = 'lm', se = F) # fit linear regression line


ggplot(main_df_pair_score, aes(rating_diff, mean, col = video_type, label = predicate_pair)) +
  geom_point() + geom_text(hjust=0, vjust=0) +
  stat_smooth(method = lm)
ggsave("./random_preprocess/lin_regression_graph.png")

main_df_pair_score

write.csv(main_df, "./merged_data/full_data/main_df.csv")
write.csv(main_df_pair_score, "./merged_data/full_data/main_df_pair_score.csv")
