library(dplyr)
getwd()

set.seed(12)

items <- read.csv("final_adult_test_list.csv")

items <- items %>% filter(random_order ==  "A1" | random_order == "B1") 

items_a <- items %>% filter(random_order == "A1")
items_b <- items %>% filter(random_order == "B1")
# randomly pair 5 videos that are video_type == sym with a predicate and 5 videos
# that are video_type == nonsym with a unique predicate.

#works but how do I include a nested conditional?
items_a %>%
  select(audio_name,video_name) %>%
    mutate(rand_video = sample(video_name))

#empty list to store the new dfs
datalist = list()

for (i in 1:16) {
  df1 <- items_a %>%
    select(audio_name,video_name,video_type,audio_first_pred_type) %>%
    mutate(rand_video = sample(video_name))
  
  df1$list <- i
  
  datalist[[i]] <- df1
}

random_pairs_a = do.call(rbind, datalist)

#now for list b
datalist_b = list()

for (i in 1:16) {
  df2 <- items_b %>%
    select(audio_name,video_name) %>%
    mutate(rand_video = sample(video_name))

  df2$list <- i
  
  datalist_b[[i]] <- df2
}

random_pairs_b = do.call(rbind, datalist_b)

#add order column
random_pairs_a$order <- "A"
random_pairs_b$order <- "B"

merged_df <- rbind(random_pairs_a,random_pairs_b)

merged_df$paste <- paste(merged_df$audio_name,merged_df$rand_video,sep="_")

# merged_df <-merged_df %>%
#   mutate(
#     bad_pairs = as.integer(
#       merged_df$paste == "fighting_or_punching_quarrel" | merged_df$paste == "punching_or_fighting_quarrel",
#       merged_df$paste == "matching_or_choosing_equal" | merged_df$paste == "choosing_or_matching_equal",
#       merged_df$paste == "separating_or_pushing_diverge" | merged_df$paste == "pushing_or_separating_diverge",
#       merged_df$paste == "marrying_or_inviting_divorce" | merged_df$paste == "inviting_or_marrying_divorce",
#       merged_df$paste == "separating_or_pushing_divorce" | merged_df$paste == "pushing_or_separating_divorce",
#       merged_df$paste == "hugging_or_tickling_embrace" | merged_df$paste == "tickling_or_hugging_embrace",
#       merged_df$paste == "sharing_or_grabbing_touch" | merged_df$paste == "grabbing_or_sharing_touch",
#       merged_df$paste == "hugging_or_tickling_touch" | merged_df$paste == "tickling_or_hugging_touch",
#       merged_df$paste == "connecting_or_hanging_touch" | merged_df$paste == "hanging_or_connecting_touch"
#       
#       )
#   )

merged_df <-merged_df %>%
  mutate(
    bad_pairs = case_when(
      (paste == "fighting_or_punching_quarrel" | paste == "punching_or_fighting_quarrel") ~ 1,
      (paste == "matching_or_choosing_equal" | paste == "choosing_or_matching_equal") ~ 1,
      (paste == "separating_or_pushing_diverge" | paste == "pushing_or_separating_diverge") ~ 1,
      (paste == "marrying_or_inviting_divorce" | paste == "inviting_or_marrying_divorce") ~ 1,
      (paste == "separating_or_pushing_divorce" | paste == "pushing_or_separating_divorce") ~ 1,
      (paste == "hugging_or_tickling_embrace" | paste == "tickling_or_hugging_embrace") ~ 1,
      (paste == "sharing_or_grabbing_touch" | paste == "grabbing_or_sharing_touch") ~ 1,
      (paste == "hugging_or_tickling_touch" | paste == "tickling_or_hugging_touch") ~ 1,
      (paste == "connecting_or_hanging_touch" | paste == "hanging_or_connecting_touch") ~ 1,
      is.na(bad_pairs) ~ 0
    )
  )

bad_pairs <- merged_df %>%
  filter(bad_pairs == 1)
write.csv(bad_pairs,"./output/bad_pairs.csv")

#write.csv(merged_df,"./output/merged_df.csv")

merged_df$video_name <- NULL

merged_df <- merged_df %>%
  mutate( video_type = 
    case_when(
      (rand_video == "expand") ~ "nonsym",
      (rand_video == "reject") ~ "nonsym",
      (rand_video == "drown") ~ "nonsym",
      (rand_video == "assist") ~ "nonsym",
      (rand_video == "touch") ~ "nonsym",
      (rand_video == "equal") ~ "sym",
      (rand_video == "diverge") ~ "sym",
      (rand_video == "quarrel") ~ "sym",
      (rand_video == "divorce") ~ "sym",
      (rand_video == "embrace") ~ "sym"
    )
  )

merged_df <- merged_df %>%
  mutate(audio_name_first_predicate_type =
           case_when(
             (audio_name == "meeting_or_teaching") ~ "sym",
             (audio_name == "inviting_or_marrying") ~ "nonsym",
             (audio_name == "pulling_or_joining") ~ "nonsym",
             (audio_name == "fighting_or_punching") ~ "sym",
             (audio_name == "grabbing_or_sharing") ~ "nonsym",
             (audio_name == "kissing_or_licking") ~ "sym",
             (audio_name == "hugging_or_tickling") ~ "sym",
             (audio_name == "hanging_or_connecting") ~ "nonsym",
             (audio_name == "choosing_or_matching") ~ "nonsym",
             (audio_name == "pushing_or_separating") ~ "nonsym",
             (audio_name == "connecting_or_hanging") ~ "sym",
             (audio_name == "separating_or_pushing") ~ "sym",
             (audio_name == "sharing_or_grabbing") ~ "sym",
             (audio_name == "tickling_or_hugging") ~ "nonsym",
             (audio_name == "licking_or_kissing") ~ "nonsym",
             (audio_name == "matching_or_choosing") ~ "sym",
             (audio_name == "joining_or_pulling") ~ "sym",
             (audio_name == "marrying_or_inviting") ~ "sym",
             (audio_name == "teaching_or_meeting") ~ "nonsym",
             (audio_name == "punching_or_fighting") ~ "nonsym",
           ))
