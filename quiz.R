library(dplyr)
library(tidyr)

library(testthat)

# PREPROCESSING 
# read in quiz results, convert columns to q# 
raw = read.csv("~/Documents/github/compatibility_quiz/quiz_full.csv", 
               stringsAsFactors = FALSE, header=TRUE, na.strings = c(""))
raw = dplyr::select(raw, -Timestamp)
new_cols = paste0("q", seq(1:(ncol(raw)-1)))
colnames(raw) = c("user_id", new_cols)
raw$user_id = gsub("@thredup.com", "", raw$user_id)

# wide to long 
raw %>%
  gather(question, answer, -user_id) -> raw

# we have different types of questions 
# 1. multiple choice 
# 2. linear scale
# 3. checkboxes 
# 4. text input

# for linear scale qs, the conversion to numeric is straightforward 
# for multiple choice, we convert some responses to numeric (conversions read in below)
# for the rest of the multiple choice questions, the numeric representation is a vector of binary values 
# ex. if user selects 1st option out of 5 options -> [1 0 0 0 0] , if he selects the 2nd -> [0 1 0 0 0], etc. 
# for checkboxes, the numeric representation is also a vector of binary values 
# ex. if user selects the first 2 options out of 5 options -> [1 1 0 0 0]
# for text input, the numeric representation is the sentiment score 

# to get the similarity between the same linear q, multiple choice q of the 1st type, and text input 
# of 2 different users, we simply subtract, since the numeric representation is a single number
# to get similarity between the same multiple choice q of the 2nd type and checkbox qs, 
# we use the binary distance 
# at the end, each user is represented by another vector with size (# of questions) 
# and we use the canberra distance to get the similarity between users 

# NUMERIC CONVERSIONS
# mcs to numeric 
mc_scores = read.csv("~/Documents/github/compatibility_quiz/mc_scores.csv", 
                     stringsAsFactors = FALSE, header=TRUE, na.strings = c(""))

# raw + mc scores 
raw %>% 
  left_join(mc_scores, by=c("question", "answer")) %>% 
  mutate(answer = ifelse(!is.na(score), score, 
                         ifelse(is.na(answer), "no response", answer))) %>% # take care of no response by q type
  dplyr::select(-score) -> raw2 

# check to make sure join worked correctly 
raw2 %>% filter(answer == "no response") 

# mc to binary vectors
# checkbox to binary vectors 
mc_choices = read.csv("~/Documents/github/compatibility_quiz/mc_choices.csv", 
                      stringsAsFactors = FALSE, header=TRUE, na.strings = c(""))
mc_choices %>% fill(question) -> mc_choices

# given a question, its possible values, and the response 
# get the binary vector representation 
to_binary_vector = function(my_question, response, possibs=mc_choices) {
  possibs %>% 
    filter(question==my_question) -> this_question
  
  res_vector = rep(0, nrow(this_question)) # prep the vector 
  
  if(response == "no response") 
    return(res_vector)
  
  # fill in the vector 
  # checkbox - need to parse response 
  if(my_question %in% c("q9", "q22")) {
    selected_choices = unlist(strsplit(response, ", ")) # be careful of trailing spaces here
    
    if(!(any(selected_choices %in% this_question$choice)))
      stop(paste("response not a possible answer choice", selected_choices)) 
    
    choice_position = which(this_question$choice %in% selected_choices) # this is the position that needs to be 1 
    res_vector[choice_position] = 1 
  }
  # mc - no need to parse response 
  else {
    if(!(response %in% this_question$choice)) 
      stop(paste("response not a possible answer choice", response))
    
    choice_position = which(this_question$choice == response) # this is the position that needs to be 1 
    res_vector[choice_position] = 1 
  }
  
  return(res_vector)
}

# text to sentiment score 
# simplified sentiment analysis 
# just counts the number of positive words 
positive_words = readLines("~/Documents/github/compatibility_quiz/positive-words.txt")

to_sentiment_score = function(response, question="q24", pos_words_dict = positive_words, type="total") {
  if(is.na(response)) 
    return(-1) 
  
  my_words = gsub("[[:punct:]]", "", response) 
  my_words = unlist(strsplit(response, " "))
  
  total_words = length(my_words) 
  num_pos_words = length(na.omit(match(my_words, pos_words_dict))) # attn: match returns NA for no match 
  
  if(type=="total") # if not unstemming, pos dict doesn't work so well 
    return(total_words)
  else
    return(num_pos_words/total_words)
}

# QUESTION MATRICES
# each question has its own matrix with size (# participants) x (# participants) 
get_matrix = function(my_question, my_raw = raw2) {
  raw2 %>% filter(question==my_question) -> this_question
  
  if(my_question %in% mc_choices$question) {
    dist_matrix_raw_1 = do.call(rbind, 
                              lapply(this_question$answer, 
                                     function(x) to_binary_vector(my_question, x)))
    dist_matrix_raw = dist(dist_matrix_raw_1, "binary") %>% as.matrix() 
  }
  else if(my_question == "q24") {
    dist_matrix_raw_1 = do.call(rbind, 
                                lapply(this_question$answer, 
                                       function(x) to_sentiment_score(x)))
    dist_matrix_raw = dist(dist_matrix_raw_1, "euclidean") %>% as.matrix() 
  }
  else {
    vals = replace(this_question$answer, this_question$answer == "no response", 0)
    dist_matrix_raw = dist(vals, "euclidean") %>% as.matrix() 
  }
  
  dist_matrix = sweep(dist_matrix_raw, 2, colSums(dist_matrix_raw), FUN="/")
  
  return(dist_matrix) 
}

# results 
final_matrix = Reduce('+', lapply(unique(raw2$question), 
                                  function(x) get_matrix(x))) # this function = amazin

final_matrix[ final_matrix == 0 ] = NA # this won't work for final_results_max

final_results = data.frame(user_id = 1:nrow(final_matrix), 
                           match = apply(final_matrix, 2, which.min), # index of min 
                           match2 = apply(final_matrix, 2, function(x) order(x)[2]), 
                           match3 = apply(final_matrix, 2, function(x) order(x)[3])) 

final_results_max = data.frame(user_id = 1:nrow(final_matrix), 
                               worst_match = apply(final_matrix, 2, which.max),
                               second_worst_match = apply(final_matrix, 2, function(x) order(x, decreasing = TRUE)[2]))  

rows_user_id = data.frame(user_name = unique(raw2$user_id), 
                          row_number = 1:length(unique(raw2$user_id)))

final_results %>% 
  left_join(rows_user_id, by=c("match" = "row_number")) %>% 
  rename(match1 = match,
         match_name = user_name) %>% 
  left_join(rows_user_id, by=c("match2" = "row_number")) %>%
  rename(match2_name = user_name) %>%
  left_join(rows_user_id, by=c("match3" = "row_number")) %>%
  rename(match3_name = user_name) %>%
  left_join(rows_user_id, by=c("user_id" = "row_number")) %>% 
  left_join(final_results_max, by="user_id") %>% 
  left_join(rows_user_id, by=c("worst_match" = "row_number")) %>% 
  rename(worst_match_name = user_name.y,
         user_name = user_name.x) %>%
  left_join(rows_user_id, by=c("second_worst_match" = "row_number")) %>%
  rename(second_worst_match_name = user_name.y,
         user_name = user_name.x) %>% 
  dplyr::select(user_name, match_name, match2_name, match3_name, worst_match_name, second_worst_match_name) -> results_df
  
options(max.print=9999)
results_df

# visualization
# divide the questions into 2 sets and average the distance scores 
# the match distance plots will be from the perspective of the user 
# since distances are relative 
half_q = round(length(unique(raw2$question))/2) - 1 # 1:this
half_q2 = length(unique(raw2$question)) # prev+1:this 

final_matrix_1 = Reduce('+', lapply(unique(raw2$question)[1:half_q], 
                                    function(x) get_matrix(x)))
final_matrix_2 = Reduce('+', lapply(unique(raw2$question)[(half_q+1):half_q2], 
                                    function(x) get_matrix(x)))

get_match_vis = function(my_user_name, rui = rows_user_id, fm1=final_matrix_1, fm2=final_matrix_2) {
  library(ggplot2)
  library(ggrepel) 
  
  col = rui[rui$user_name == my_user_name,]$row_number %>% return()
  
  vis_df = data.frame(dimension_1 = fm1[,col],
                      dimension_2 = fm2[,col],
                      match_name = rui$user_name) 
  
  ggplot(vis_df, 
         aes(x=dimension_1, y=dimension_2, label=match_name)) + 
    geom_point() + geom_label_repel() +
    ggtitle(paste("compatibility plot for", my_user_name)) + theme_light() #+
    #xlim(c(0,3.5)) + ylim(c(0,3.5)) # this prob needs to be adjusted 
}

# run tests
test_dir("tests/")