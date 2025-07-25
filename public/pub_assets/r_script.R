library(dplyr)
library(plyr)


card_data <- read.table("/Users/annahuntisaak/Desktop/card_game.txt", T)


#Organizing Data
card_data$Onset_sec[((card_data$Filename == "NEWCARD_01alter") & 
                       (card_data$Onset_sec >= 1200))]  = card_data[((card_data$Filename == "NEWCARD_01alter") 
                                                                     & (card_data$Onset_sec >= 1200)), 5] + 1200
card_data$Offset_sec[((card_data$Filename == "NEWCARD_01alter") & 
                        (card_data$Offset_sec >= 1200))]  = card_data[((card_data$Filename == "NEWCARD_01alter")
                                                                       & (card_data$Offset_sec >= 1200)), 6] + 1200

card_data$Filename[card_data$Filename == 'NEWCARD_01alter_WAV'] = 'NEWCARD_01alter'

card_data$TierOrder <- ifelse(card_data$TierName == "Hand", "1", ifelse(card_data$TierName == "Trial", "2", "3"))

card_data_ordered <- card_data[order(card_data$Filename, round(card_data$Onset_sec), card_data$TierOrder), ]

card_data_numbered <- ddply(card_data_ordered, .(Filename), function(x){
  for(i in 1:nrow(x)){ 
    if(x[i, c("TierName")] == "Hand"){
      OnsetHand <- round(x[i, c("Onset_sec")])
      HandNb <- x[i, c("Annotation")]
      x[i, c("TrialNb")] <- "none"
      x[i, c("TrialOnset")] <- "none"
      x[i, c("TrialOffset")] <- "none"
      x[i, c("HandNb")] <- "none"
      x[i, c("OnsetHand")] <- "none"
    }
    else{
      if(x[i, c("TierName")] == "Trial"){
        OnsetTrial <- round(x[i, c("Onset_sec")])
        OffsetTrial <- round(x[i, c("Offset_sec")])
        TrialNb <- x[i, c("Annotation")]
        x[i, c("TrialNb")] <- "none"
        x[i, c("TrialOnset")] <- "none"
        x[i, c("TrialOffset")] <- "none"
        x[i, c("HandNb")] <- "none"
        x[i, c("OnsetHand")] <- "none"
      }
      else{
        x[i, c("HandNb")] <- HandNb
        x[i, c("OnsetHand")] <- OnsetHand
        if((round(x[i, c("Onset_sec")]) >= OnsetTrial) & (round(x[i, c("Offset_sec")]) <= OffsetTrial)){
          x[i, c("TrialNb")] <- TrialNb
          x[i, c("TrialOnset")] <- OnsetTrial
          x[i, c("TrialOffset")] <- OffsetTrial
        }
        else{
          x[i, c("TrialNb")] <- "error"
          x[i, c("TrialOnset")] <- "error"
          x[i, c("TrialOffset")] <- "error"
        }
      }
    }
  }
  return(x)
})


#Checking for errors so they can be dealt with
table(card_data_numbered$TrialNb, useNA="always")


#Getting hands/trials only when confederate is the Director
hand_comp <- read.table("/Users/annahuntisaak/Desktop/HandComp.txt", T)
hand_comp <- rename(hand_comp, replace = c("FILE" = "Filename", "SEQUENTIALHAND" = "HandNb"))

for(h in 1:nrow(hand_comp)){
  if(nchar(as.character(hand_comp[h, "HandNb"])) == 1){
    hand_comp[h, "HandNb"] <- paste('H0', as.character(hand_comp[h, "HandNb"]), sep = "")
  }
  if(nchar(as.character(hand_comp[h, "HandNb"])) == 2){
    hand_comp[h, "HandNb"] <- paste('H', as.character(hand_comp[h, "HandNb"]), sep = "")
  }
}

right <- merge(card_data_numbered[card_data_numbered$TierName == "RightIndividualSpeech", ],
               card_data_numbered[card_data_numbered$TierName == "RightSpeechContents", ],
               by = c("Filename", "Onset_sec", "Offset_sec", "Duration_msec", "TierOrder",
                      "TrialNb", "TrialOnset", "TrialOffset", "HandNb", "OnsetHand"),
               all = TRUE)

left <- merge(card_data_numbered[card_data_numbered$TierName == "LeftIndividualSpeech", ],
              card_data_numbered[card_data_numbered$TierName == "LeftSpeechContents", ],
              by = c("Filename", "Onset_sec", "Offset_sec", "Duration_msec", "TierOrder",
                     "TrialNb", "TrialOnset", "TrialOffset", "HandNb", "OnsetHand"),
              all = TRUE)

right_gestures <- merge(card_data_numbered[card_data_numbered$TierName == "RightGestures", ],
                        card_data_numbered[card_data_numbered$TierName == "RightGestureContents", ],
                        by = c("Filename", "Onset_sec", "Offset_sec", "Duration_msec", "TierOrder",
                               "TrialNb", "TrialOnset", "TrialOffset", "HandNb", "OnsetHand"),
                        all = TRUE)

left_gestures <- merge(card_data_numbered[card_data_numbered$TierName == "LeftGestures", ],
                        card_data_numbered[card_data_numbered$TierName == "LeftGestureContents", ],
                        by = c("Filename", "Onset_sec", "Offset_sec", "Duration_msec", "TierOrder",
                               "TrialNb", "TrialOnset", "TrialOffset", "HandNb", "OnsetHand"),
                        all = TRUE)

speech <- rbind(right, left)
gestures <- rbind(right_gestures, left_gestures)
card_and_hand <- rbind(speech, gestures)
card_and_hand <- ddply(card_and_hand, .(Filename))
card_and_hand <- card_and_hand[order(card_and_hand$Filename,
                                     round(card_and_hand$Onset_sec)), ]

all <- subset((join(card_and_hand, hand_comp, by = c("Filename", "HandNb"), 
                    type = "left")), select = 1:15)

nums <- c(1:20)
c_director <- all[all$HANDCODE %in% nums, ]


#In new column called "QuestionType", keeping track of the questions being
#asked as well as who is doing the asking (Director or Helper) in another
#column called "Questioner"
director <- "RightIndividualSpeech"
helper <- "LeftIndividualSpeech"
for(r in 1:nrow(c_director)){
  if(endsWith(c_director[r, "Annotation.x"], "?")){
    file <- c_director[r, "Filename"]
    trial <- c_director[r, "TrialNb"]
    question <- c_director[r, "Annotation.x"]
    
    if(c_director[r, "TierName.x"] == director){
      questioner <- "Director"
    }
    else if(c_director[r, "TierName.x"] == helper){
      questioner <- "Helper"
    }
    c_director[r, c("QuestionType")] <- question
    c_director[r, c("Questioner")] <- questioner
  }
  else{
    c_director[r, c("QuestionType")] <- "NA"
    c_director[r, c("Questioner")] <- "NA"
  }
}


#Creating new column that reflects the utterance/gesture's turn number within each set 
#of question and corresponding answer(s). For a trial where multiple questions are asked,
#this means there will be more than one set of turn rankings
rank = 1
for(r in 1:nrow(c_director)){
  if(r == nrow(c_director)){
    c_director[r, c("TurnRank")] <- rank
  }
  else if(endsWith(c_director[r + 1, "QuestionType"], "?")){
    c_director[r, c("TurnRank")] <- rank
    rank = 1
    next
  }
  c_director[r, c("TurnRank")] <- rank
  rank <- rank + 1
}


#Now that each question is labeled with question type/questioner and each set of question
#and answer(s) is has turn ranks, want to go back over and label all of the turns that fall under
#a given question with its question type and the questioner
for(r in 1:nrow(c_director)){
  if(c_director[r, "TurnRank"] == 1){
    question <- c_director[r, "QuestionType"]
    questioner <- c_director[r, "Questioner"]
    if(r + 1 == nrow(c_director)){
      c_director[r + 1, "QuestionType"] <- question
      c_director[r + 1, "Questioner"] <- questioner
      break
    }
    r <- r + 1
    while(c_director[r, "TurnRank"] > 1){
      c_director[r, "QuestionType"] <- question
      c_director[r, "Questioner"] <- questioner
      r <- r + 1
    }
  }
}


for(r in 1:nrow(c_director)){
  #Looking at only those sets where the question is spec
  if((c_director[r, "TurnRank"] == 1) & (c_director[r, "Annotation.x"]) == "spec?"){
    original_r <- r
    rank_one_counter <- 1
    
    #Counters to keep track of different things that might occur in any given spec set
    yes <- 0
    no <- 0
    offer <- 0
    prompt <- 0
    handing <- 0
    none <- 0
    
    while(rank_one_counter == 1){
      if(c_director[r, "Annotation.x"] == "q_yes"){
        yes <- 1
      }
      else if(c_director[r, "Annotation.x"] == "q_no"){
        no <- 1
      }
      if(c_director[r, "Annotation.x"] == "prompt_to_get_card"){
        prompt <- 1
      }
      if(c_director[r, "Annotation.x"] == "HandingCard"){
        handing <- 1
        handing_rank <- c_director[r, "TurnRank"]
      }
      if(c_director[r, "Annotation.x"] %in% c("r_num_type", "r_num_suit", "r_num_neither",
                                              "r_id_type", "r_id_suit", "r_id_neither")){
        if((c_director[r, "Annotation.x"] %in% c("r_num_type", "r_num_suit")) &
           endsWith(c_director[r, "Annotation.y"], "-0")){
          none <- 1
        }
        else{
          offer <- 1
          offer_rank <- c_director[r, "TurnRank"]
        }
      }
      if(r == nrow(c_director)){
        break
      }
      #Checking that the next row is not the begining of another question set
      else{
        r <- r + 1
        if(c_director[r, "TurnRank"] == 1){
          rank_one_counter <- rank_one_counter + 1
        }
      }
    }

    #Returning the row number to the value it was at the beginning of the set 
    r <- original_r

    category <- ""
    if(no == 1){
      if(offer == 0 & handing == 0 & none == 0){
        category <- "no_only"
      }
      else if(offer >= 1){
        category <- "no_offer"
      }
      else if(offer == 0 & handing == 0 & none == 1){
        category <- "none"
      }
      else if(offer == 0 & handing == 1){
        category <- "no_handing"
      }
    }
    else if(yes == 1){
      if(offer == 0 & handing == 0){
        category <- "yes_only"
      }
      else if(offer == 0 & handing == 1){
        if(prompt == 1){
          category <- "yes_handing_prompted"
        }
        else if(prompt == 0){
          category <- "yes_handing_unprompted"
        }
      }
      else if(offer >= 1 & handing == 0){
        category <- "yes_offer"
      }
      else if(offer >= 1 & handing == 1){
        if(offer_rank < handing_rank){
          category <- "yes_offer_before_handing"
        }
        else if(offer_rank > handing_rank){
          category <- "yes_offer_after_handing"
        }
      }
    }
    else if(yes == 0 & no == 0){
      if(none == 1){
        category <- "none"
      }
      else if(offer == 1){
        category <- "not_directly_acknowledged"
      }
      else if(offer == 0 & handing == 1){
        category <- "handing_only"
      }
    }
    
    #Labeling the entire set with the appropriate category
    if(category != ""){
      rank_one_counter <- 1
      while(rank_one_counter == 1){
        c_director[r, c("Category")] <- category
        if(r == nrow(c_director)){
          break
        }
        r <- r + 1
        if(c_director[r, "TurnRank"] == 1){
          rank_one_counter <- rank_one_counter + 1
        }
      }
    }
  }
}

spec <- subset(c_director, c_director$"QuestionType" == "spec?")
nrow(spec[is.na(spec$Category) == TRUE, ])
spec[is.na(spec$Category) == TRUE, ]
table(spec$Category)


for(r in 1:nrow(c_director)){
  #Looking at only those sets where the question is any_suit
  if((c_director[r, "TurnRank"] == 1) & (c_director[r, "Annotation.x"]) == "any_suit?"){
    original_r <- r
    rank_one_counter <- 1
    
    #Didn't look at offers in relation to handing (unlike spec), instead, chose to
    #just break once a handing was reached (this could easily be changed though)
    yes <- 0
    no <- 0
    list <- 0
    number <- 0
    offer <- 0
    
    while(rank_one_counter == 1){
      if(c_director[r, "Annotation.x"] == "q_yes"){
        yes <- 1
      }
      else if(c_director[r, "Annotation.x"] == "q_no"){
        no <- 1
      }
      if(c_director[r, "Annotation.x"] == "HandingCard"){
        break
        
      }
      if(c_director[r, "Annotation.x"] %in% c("r_num_suit", "r_num_neither", "r_id_suit", "r_id_neither")){
        if(c_director[r, "Annotation.x"] == "r_id_suit"){
          list <- 1
        }
        else if(c_director[r, "Annotation.x"] == "r_num_suit"){
          number <- 1
        }
        else{
          offer <- 1
        }
      }
      if(r == nrow(c_director)){
        break
      }
      else{
        r <- r + 1
        if(c_director[r, "TurnRank"] == 1){
          rank_one_counter <- rank_one_counter + 1
        }
      }
    }
    
    r <- original_r
    
    category <- ""
    if(no == 1){
      if(offer == 0){
        category <- "no_only"
      }
      else if(offer >= 1){
        category <- "no_offer"
      }
    }
    else if(yes == 1){
      if(list == 0 & number == 0 & offer == 0){
        category <- "yes_only"
      }
      else if(list == 0 & number == 1 & offer == 0){
        category <- "yes_number"
      }
      else if(list == 1 & offer == 0){
        category <- "yes_list"
      }
      else if(list == 1 & offer == 1){
        category <- "yes_list_offer"
      }
    }
    else if(yes == 0 & no == 0){
      if(list == 1 & offer == 0){
        category <- "list"
      }
      else if(list == 1 & offer >= 1){
        category <- "list_offer"
      }
      else if(list == 0 & number == 1 & offer == 0){
        category <- "number_only"
      }
    }
    
    if(category != ""){
      rank_one_counter <- 1
      while(rank_one_counter == 1){
        c_director[r, c("Category")] <- category
        if(r == nrow(c_director)){
          break
        }
        r <- r + 1
        if(c_director[r, "TurnRank"] == 1){
          rank_one_counter <- rank_one_counter + 1
        }
      }
    }
  }
}

any_suit <- subset(c_director, c_director$"QuestionType" == "any_suit?")
nrow(any_suit[is.na(any_suit$Category) == TRUE, ])
any_suit[is.na(any_suit$Category) == TRUE, ]
table(any_suit$Category)


for(r in 1:nrow(c_director)){
  #Looking at only those sets where the question is any_type
  if((c_director[r, "TurnRank"] == 1) & (c_director[r, "Annotation.x"]) == "any_type?"){
    original_r <- r
    rank_one_counter <- 1

    #Bringing back consideration for handing here (could also add in prompting
    #and then look at speech/gestures in relation to it, like was done with spec)
    yes <- 0
    no <- 0
    list <- 0
    number <- 0
    offer <- 0
    handing <- 0

    while(rank_one_counter == 1){
      if(c_director[r, "Annotation.x"] == "q_yes"){
        yes <- 1
      }
      else if(c_director[r, "Annotation.x"] == "q_no"){
        no <- 1
      }
      if(c_director[r, "Annotation.x"] == "HandingCard"){
        handing <- 1
        break
      }
      if(c_director[r, "Annotation.x"] %in% c("r_num_type", "r_num_neither", "r_id_type", "r_id_neither")){
        if(c_director[r, "Annotation.x"] == "r_id_type"){
          list <- 1
        }
        else if(c_director[r, "Annotation.x"] == "r_num_type"){
          number <- 1
        }
        else{
          offer <- 1
        }
      }
      if(r == nrow(c_director)){
        break
      }
      else{
        r <- r + 1
        if(c_director[r, "TurnRank"] == 1){
          rank_one_counter <- rank_one_counter + 1
        }
      }
    }

    r <- original_r

    category <- ""
    if(no == 1){
      if(offer == 0){
        category <- "no_only"
      }
      else if(offer >= 1){
        category <- "no_offer"
      }
    }
    else if(yes == 1){
      if(list == 0 & number == 0 & offer == 0 & handing == 0){
        category <- "yes_only"
      }
      else if(list == 0 & number == 0 & offer == 0 & prompt == 0 & handing == 1){
        category <- "yes_handing"
      }
      else if(list == 0 & number == 1 & offer == 0){
        category <- "yes_number"
      }
      else if(list == 1 & offer == 0){
        category <- "yes_list"
      }
      else if(list == 1 & offer == 1){
        category <- "yes_list_offer"
      }
    }
    else if(yes == 0 & no == 0){
      if(list == 1 & offer == 0){
        category <- "list"
      }
      else if(list == 1 & offer >= 1){
        category <- "list_offer"
      }
      else if(list == 0 & number == 1 & offer == 0){
        category <- "number_only"
      }
      else if(offer == 0 & handing == 1){
        category <- "handing_only"
      }
    }

    if(category != ""){
      rank_one_counter <- 1
      while(rank_one_counter == 1){
        c_director[r, c("Category")] <- category
        if(r == nrow(c_director)){
          break
        }
        r <- r + 1
        if(c_director[r, "TurnRank"] == 1){
          rank_one_counter <- rank_one_counter + 1
        }
      }
    }
  }
}

any_type <- subset(c_director, c_director$"QuestionType" == "any_type?")
nrow(any_type[is.na(any_type$Category) == TRUE, ])
any_type[is.na(any_type$Category) == TRUE, ]
table(any_type$Category)
