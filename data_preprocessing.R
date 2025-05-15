

rm(list = ls())
library(readxl)
library(writexl)
library(stringr)
library(rstudioapi)

current_path = dirname(getSourceEditorContext()$path)

while (1) 
{
  if (grepl('raw_files', current_path, fixed=TRUE) || grepl('processed_files', current_path, fixed=TRUE)){
    current_path = dirname(current_path)
  } else 
  {
    break
  }
}

raw_file_path = paste(current_path, '/raw_files', sep="")
processed_file_path = paste(current_path, '/processed_files', sep='')
setwd(raw_file_path)
raw_file_list = list.files(path=raw_file_path, pattern=NULL, all.files=FALSE, full.names=FALSE)

pos_bitrate_indx = c("n6" = 0, "n5" = 0, "n4" = 0, "n3" = 0, "n2" = 0, "n1" = 0, "z0" = 0,
                "p1" = 0, "p2" = 0, "p3" = 0, "p4" = 0, "p5" = 0, "p6" = 0)


pos_bitrate_keys = names(pos_bitrate_indx)

value_bitrate_keys = as.character(list("bitn6", "bitn5", "bitn4", "bitn3", "bitn2", "bitn1",
                          "bitz0", "bitp1", "bitp2", "bitp3", "bitp4", "bitp5", "bitp6"))
final_df <- data.frame(subid = rep(NA, 500),
                       PROLIFIC_PID = rep(NA, 500),
                       n6 = rep(NA, 500),
                       n5 = rep(NA, 500),
                       n4 = rep(NA, 500),
                       n3 = rep(NA, 500),
                       n2 = rep(NA, 500),
                       n1 = rep(NA, 500),
                       z0 = rep(NA, 500),
                       p1 = rep(NA, 500),
                       p2 = rep(NA, 500),
                       p3 = rep(NA, 500),
                       p4 = rep(NA, 500),
                       p5 = rep(NA, 500),
                       p6 = rep(NA, 500),
                       bitn6 = rep(NA, 500),
                       bitn5 = rep(NA, 500),
                       bitn4 = rep(NA, 500),
                       bitn3 = rep(NA, 500),
                       bitn2 = rep(NA, 500),
                       bitn1 = rep(NA, 500),
                       bitz0 = rep(NA, 500),
                       bitp1 = rep(NA, 500),
                       bitp2 = rep(NA, 500),
                       bitp3 = rep(NA, 500),
                       bitp4 = rep(NA, 500),
                       bitp5 = rep(NA, 500),
                       bitp6 = rep(NA, 500),
                       sumbit1 = rep(NA, 500),
                       sumbit2 = rep(NA, 500))

checkMatch = NA
participant_indx = 0 
for (raw_file in raw_file_list){
  
  setwd(raw_file_path)
  
  df <- read_excel(raw_file)
  
  processed_ind_df <- data.frame(subid = rep(NA, 67),
                                 PROLIFIC_PID = rep(NA, 67),
                                 position = rep(NA, 67), 
                                 response1 = rep(NA, 67), 
                                 response2 = rep(NA, 67),
                                 response3 = rep(NA, 67), 
                                 stimulus1 = rep(NA, 67), 
                                 stimulus2 = rep(NA, 67), 
                                 stimulus3 = rep(NA, 67),
                                 check1 = rep(NA, 67),
                                 check2 = rep(NA, 67),
                                 check3 = rep(NA, 67))
  
  bitrate_df <- data.frame(n6 = rep(NA, 15),
                           n5 = rep(NA, 15),
                           n4 = rep(NA, 15),
                           n3 = rep(NA, 15),
                           n2 = rep(NA, 15),
                           n1 = rep(NA, 15),
                           z0 = rep(NA, 15),
                           p1 = rep(NA, 15),
                           p2 = rep(NA, 15),
                           p3 = rep(NA, 15),
                           p4 = rep(NA, 15),
                           p5 = rep(NA, 15),
                           p6 = rep(NA, 15))
  
  pos_bitrate_indx = c("n6" = 0, "n5" = 0, "n4" = 0, "n3" = 0, "n2" = 0, "n1" = 0, "z0" = 0,
                "p1" = 0, "p2" = 0, "p3" = 0, "p4" = 0, "p5" = 0, "p6" = 0)
  
  subid <- as.character(df['subid'][1, ])
  date <- as.character(df['date'][1, ])
  prolid <- as.character(df['PROLIFIC_PID'][1, ]) 
  row_count <- nrow(df)
  indx <- 0 
  checkForPractice = TRUE
  for (rowi in 1:row_count){
    
    if (!is.na(df['textbox.text'][rowi,])){
      indx = indx + 1
      if (indx < 11 & checkForPractice){
        next
      } else if (indx == 11 & checkForPractice){
        indx = 1
        checkForPractice = FALSE
        next
      }
      split_response = unlist(strsplit(unlist(df['textbox.text'][rowi,]), split=""))
      split_stimulus = unlist(strsplit(unlist(df['stimulus_triagram'][rowi,]), split=""))
      
      processed_ind_df['position'][indx, ] <- df['position'][rowi, ]
      processed_ind_df['subid'][indx, ] <- df['subid'][rowi, ]
      processed_ind_df['PROLIFIC_PID'][indx, ] <- df['PROLIFIC_PID'][rowi, ]
      
      pos_org <- unlist(df['position'][rowi, ])
      
      for (i in 1:3){
        processed_ind_df[paste('response', as.character(i), sep = "")][indx, 1] <- split_response[i]
        processed_ind_df[paste('stimulus', as.character(i), sep = "")][indx, 1] <- split_stimulus[i]
        checkMatch <- as.integer(split_response[i] == split_stimulus[i])
        processed_ind_df[paste('check', as.character(i), sep = "")][indx, 1] <- checkMatch
        
        letter_position = pos_org-1+i
        pos_bitrate_indx[pos_bitrate_keys[letter_position]] <- pos_bitrate_indx[pos_bitrate_keys[letter_position]] + 1
        bitrate_df[pos_bitrate_keys[letter_position]][pos_bitrate_indx[pos_bitrate_keys[letter_position]], 1] <- checkMatch
      }
      
    }
    
  }
  
  participant_indx = participant_indx + 1
  
  processed_ind_df <- processed_ind_df[-which(is.na(processed_ind_df)), ]
  processed_ind_df <- processed_ind_df[order(processed_ind_df$position, decreasing = FALSE), ]
  
  final_df['subid'][participant_indx, ] <- subid
  final_df['PROLIFIC_PID'][participant_indx, ] <- prolid
  final_df[participant_indx, pos_bitrate_keys] <- colMeans(bitrate_df[,pos_bitrate_keys], na.rm=TRUE)
  final_df[participant_indx, value_bitrate_keys] <- final_df[participant_indx, pos_bitrate_keys]*4.6761-0.036996
  final_df[participant_indx, "sumbit1"] <- sum(subset(final_df[participant_indx, ], select=bitn5:bitp5), na.rm=TRUE)
  final_df[participant_indx, "sumbit2"] <- sum(subset(final_df[participant_indx, ], select=bitn4:bitp4), na.rm=TRUE)
  setwd(processed_file_path)
  write_xlsx(processed_ind_df, paste(paste(subid, date, sep='_'), '.xlsx', sep=''))
  
}


write_xlsx(final_df, 'final_data.xlsx')

cat("Processing complete /n")

