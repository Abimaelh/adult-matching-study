library(tidyr)
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

merged_df <- merged_df %>%
  mutate(target_order =
           case_when(
             (audio_name_first_predicate_type == "sym" & video_type == "sym") ~ "first",
             (audio_name_first_predicate_type == "nonsym" & video_type == "nonsym") ~ "first",
             (audio_name_first_predicate_type == "sym" & video_type == "nonsym") ~ "second",
             (audio_name_first_predicate_type == "nonsym" & video_type == "sym") ~ "second"
           )
           )

merged_df <- merged_df %>%
  mutate(collapsed_predicate_pair = 
           case_when(
             (audio_name == "meeting_or_teaching") ~ "meet_teach",
             (audio_name == "teaching_or_meeting") ~ "meet_teach",
             (audio_name == "inviting_or_marrying") ~ "marry_invite",
             (audio_name == "marrying_or_inviting") ~ "marry_invite",
             
             (audio_name == "pulling_or_joining") ~ "join_pull",
             (audio_name == "joining_or_pulling") ~ "join_pull",
             
             (audio_name == "fighting_or_punching") ~ "fight_punch",
             (audio_name == "punching_or_fighting") ~ "fight_punch",
             
             (audio_name == "grabbing_or_sharing") ~ "share_grab",
             (audio_name == "sharing_or_grabbing") ~ "share_grab",
             
             (audio_name == "kissing_or_licking") ~ "kiss_lick",
             (audio_name == "licking_or_kissing") ~ "kiss_lick",
             
             (audio_name == "hugging_or_tickling") ~ "hug_tickle",
             (audio_name == "tickling_or_hugging") ~ "hug_tickle",
             
             (audio_name == "hanging_or_connecting") ~ "connect_hang",
             (audio_name == "connecting_or_hanging") ~ "connect_hang",
             
             (audio_name == "choosing_or_matching") ~ "match_choose",
             (audio_name == "matching_or_choosing") ~ "match_choose",
             
             (audio_name == "pushing_or_separating") ~ "separate_push",
             (audio_name == "separating_or_pushing") ~ "separate_push"
           )
           )

write.csv(merged_df,"merged_df_2.csv")

clean_df <- read.csv("merged_df_2.csv")

clean_df$paste <- NULL
clean_df$bad_pairs <- NULL
clean_df$video_type <- NULL
clean_df$target_order <- NULL

clean_df$paste <- paste(clean_df$audio_name,clean_df$rand_video,sep="_")
clean_df <- clean_df %>%
  mutate(
    bad_pairs = case_when(
      (collapsed_predicate_pair == "fight_punch" & rand_video == "quarrel") ~ 1,
      (collapsed_predicate_pair == "match_choose" & rand_video == "equal") ~ 1,
      (collapsed_predicate_pair == "separate_push" & rand_video == "diverge") ~ 1,
      (collapsed_predicate_pair == "marry_invite" & rand_video == "divorce") ~ 1,
      (collapsed_predicate_pair == "separate_push" & rand_video == "divorce") ~ 1,
      (collapsed_predicate_pair == "hug_tickle" & rand_video == "embrace") ~ 1,
      (collapsed_predicate_pair == "share_grab" & rand_video == "touch") ~ 1,
      (collapsed_predicate_pair == "hug_tickle" & rand_video == "touch") ~ 1,
      (collapsed_predicate_pair == "connect_hang" & rand_video == "touch") ~ 1
    )
  )

unique(clean_df$audio_name)

bad_pairs_clean_df <- clean_df %>%
  filter(bad_pairs == 1)

merged_df_v3 <- read.csv("merged_df_v3.csv")

all_videos <- as.data.frame(all_videos <- c("touch","drown","reject","diverge","quarrel","assist","expand","equal","embrace","divorce"))
all_videos<-rename(all_videos, "videos" = "all_videos <- c(\"touch\", \"drown\", \"reject\", \"diverge\", \"quarrel\", \"assist\", \"expand\", \"equal\", \"embrace\", \"divorce\")")
colnames(all_videos)

merged_df_v3$all_videos <- c("touch","drown","reject","diverge","quarrel","assist","expand","equal","embrace","divorce")
merged_df_v3$all_videos <- NULL

checking_my_work <- merged_df_v3 %>%
  group_by(collapsed_predicate_pair,rand_video) %>%
    count(rand_video)

checking_my_work <- rename(checking_my_work, videos = rand_video)

all_videos_sliced <- all_videos %>% slice(rep(1:n(), each = 10))


merged_check <- merge(checking_my_work,all_videos_sliced, all =TRUE)

pred_pairs <- as.data.frame(unique(merged_df_v3$collapsed_predicate_pair))
colnames(pred_pairs)
rename(pred_pairs, "pred_pairs" = "unique(merged_df_v3$collapsed_predicate_pair)")
pred_pairs<- pred_pairs %>% slice(rep(1:n(),each=10))
colnames(pred_pairs)
rename(pred_pairs, "pred_pairs" = "unique(merged_df_v3$collapsed_predicate_pair)")

n <- 10

all_videos <- do.call("rbind", replicate(n, all_videos, simplify = FALSE))

all_video_pred_pair <- cbind(pred_pairs, all_videos)
colnames(all_video_pred_pair)
all_video_pred_pair <- rename(all_video_pred_pair, "collapsed_predicate_pair" = "pred_pair")

merged_df_v3 <- read.csv("merged_df_v3.csv")

checking_my_work <- merged_df_v3 %>%
  group_by(collapsed_predicate_pair,rand_video) %>%
  count(rand_video)

checking_my_work <- rename(checking_my_work, "videos" = "rand_video")

merged_check <- merge(checking_my_work,all_video_pred_pair, all =TRUE)
merged_check$n[is.na(merged_check$n)] <- 0

merged_df_v3 <- rename(merged_df_v3, "videos" = "rand_video")

merged_df_v4 <- merged_df_v3 %>%
  mutate( video_type = 
            case_when(
              (videos == "expand") ~ "nonsym",
              (videos == "reject") ~ "nonsym",
              (videos == "drown") ~ "nonsym",
              (videos == "assist") ~ "nonsym",
              (videos == "touch") ~ "nonsym",
              (videos == "equal") ~ "sym",
              (videos == "diverge") ~ "sym",
              (videos == "quarrel") ~ "sym",
              (videos == "divorce") ~ "sym",
              (videos == "embrace") ~ "sym"
            )
  )

write.csv(merged_df_v4,"merged_df_v4.csv")

order_info <- merged_df_v4 %>%
  group_by(collapsed_predicate_pair,videos,order) %>%
    count(videos)

write.csv(order_info, "order_info.csv")

write.csv(merged_check, "predicate_pair_and_video_count.csv")

random_pred_vid_list <- read.csv("random_pred_vid_list.csv")

random_pred_vid_list <- random_pred_vid_list %>%
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

random_pred_vid_list <- random_pred_vid_list %>%
  mutate(collapsed_predicate_pair = 
           case_when(
             (audio_name == "meeting_or_teaching") ~ "meet_teach",
             (audio_name == "teaching_or_meeting") ~ "meet_teach",
             (audio_name == "inviting_or_marrying") ~ "marry_invite",
             (audio_name == "marrying_or_inviting") ~ "marry_invite",
             
             (audio_name == "pulling_or_joining") ~ "join_pull",
             (audio_name == "joining_or_pulling") ~ "join_pull",
             
             (audio_name == "fighting_or_punching") ~ "fight_punch",
             (audio_name == "punching_or_fighting") ~ "fight_punch",
             
             (audio_name == "grabbing_or_sharing") ~ "share_grab",
             (audio_name == "sharing_or_grabbing") ~ "share_grab",
             
             (audio_name == "kissing_or_licking") ~ "kiss_lick",
             (audio_name == "licking_or_kissing") ~ "kiss_lick",
             
             (audio_name == "hugging_or_tickling") ~ "hug_tickle",
             (audio_name == "tickling_or_hugging") ~ "hug_tickle",
             
             (audio_name == "hanging_or_connecting") ~ "connect_hang",
             (audio_name == "connecting_or_hanging") ~ "connect_hang",
             
             (audio_name == "choosing_or_matching") ~ "match_choose",
             (audio_name == "matching_or_choosing") ~ "match_choose",
             
             (audio_name == "pushing_or_separating") ~ "separate_push",
             (audio_name == "separating_or_pushing") ~ "separate_push"
           )
  )

checking_random_pred_vid <- random_pred_vid_list %>%
  group_by(audio_name,video_type) %>%
  count(video_type)

write.csv(random_pred_vid_list,"random_pred_vid_list_r.csv")

#adding audio and video file name columns
rand_pred_list <- read.csv("random_pred_vid_list_r.csv")
colnames(rand_pred_list)
rand_pred_list <- rename(rand_pred_list, "predicate_order" = "ï..random_order" )

rand_pred_list <- rand_pred_list %>% 
  mutate(video = 
           case_when(
              (video_name == "diverge") ~ "diverge.mp4",
              (video_name == "equal") ~ "equal.mp4",
              (video_name == "divorce") ~ "divorce.mp4",
              (video_name == "embrace") ~ "embrace.mp4",
              (video_name == "quarrel") ~ "quarrel.mp4",
              (video_name == "expand") ~ "expand.mp4",
              (video_name == "assist") ~ "assist.mp4",
              (video_name == "touch") ~ "touch.mp4",
              (video_name == "reject") ~ "reject.mp4",
              (video_name == "drown") ~ "drown.mp4"
             
           )
        )

rand_pred_list <- rand_pred_list %>%
  mutate(audio_2 =
           case_when(
             (audio_name == "meeting_or_teaching") ~ "9A_did_he_say_meeting_or_teaching.wav",
             (audio_name == "inviting_or_marrying") ~ "6A_did_he_say_inviting_or_marrying.wav",
             (audio_name == "pulling_or_joining") ~ "7B_did_he_say_pulling_or_joining.wav",
             (audio_name == "fighting_or_punching") ~ "3A_did_he_say_fighting_or_punching.wav",
             (audio_name == "grabbing_or_sharing") ~ "4A_did_he_say_grabbing_or_sharing.wav",
             (audio_name == "kissing_or_licking") ~ "8A_did_he_say_kissing_or_licking.wav",
             (audio_name == "hugging_or_tickling") ~ "5A_did_he_say_hugging_or_tickling.wav",
             (audio_name == "hanging_or_connecting") ~ "2B_did_he_say_hanging_or_connecting.wav",
             (audio_name == "choosing_or_matching") ~ "1A_did_he_say_choosing_or_matching.wav",
             (audio_name == "pushing_or_separating") ~ "10A_did_he_say_pushing_or_separating.wav",
             
             (audio_name == "teaching_or_meeting") ~ "9B_did_he_say_teaching_or_meeting.wav",
             (audio_name == "marrying_or_inviting") ~ "6B_did_he_say_marrying_or_inviting.wav",
             (audio_name == "joining_or_pulling") ~ "7A_did_he_say_joining_or_pulling.wav",
             (audio_name == "punching_or_fighting") ~ "3B_did_he_say_punching_or_fighting.wav",
             (audio_name == "sharing_or_grabbing") ~ "4B_did_he_say_sharing_or_grabbing.wav",
             (audio_name == "licking_or_kissing") ~ "8B_did_he_say_licking_or_kissing.wav",
             (audio_name == "tickling_or_hugging") ~ "5B_did_he_say_tickling_or_hugging.wav",
             (audio_name == "connecting_or_hanging") ~ "2A_did_he_say_connecting_or_hanging.wav",
             (audio_name == "matching_or_choosing") ~ "1B_did_he_say_matching_or_choosing.wav",
             (audio_name == "separating_or_pushing") ~ "10B_did_he_say_separating_or_pushing.wav"
           )
           )

rand_pred_list <- rand_pred_list %>%
  mutate(target_order =
          case_when(
            (video_type == "sym" & audio_name_first_predicate_type == "sym") ~ "first",
            (video_type == "nonsym" & audio_name_first_predicate_type == "nonsym") ~ "first",
            (video_type == "sym" & audio_name_first_predicate_type == "nonsym") ~ "second",
            (video_type == "nonsym" & audio_name_first_predicate_type == "sym") ~ "second"
          )
           )

# creating first pred second pred columns
rand_pred_list$audio_name2 <- rand_pred_list$audio_name

rand_pred_list <- rand_pred_list %>%
      separate(audio_name2,c("first_predicate","or","second_predicate"))

rand_pred_list$or <- NULL

getwd()
write.csv(rand_pred_list,"rand_pred_list.csv")
