

rm(list = ls())
library(readxl)
library(writexl)
library(stringr)
library(hash)
current_path = getwd()

while (1) {
  if (grepl('raw_files', current_path, fixed=TRUE) || grepl('processed_files', current_path, fixed=TRUE)){
    current_path = dirname(current_path)
  } else{
    break
  }
}

raw_file_path = paste(current_path, '/raw_files', sep="")
processed_file_path = paste(current_path, '/processed_files', sep='')
setwd(raw_file_path)


final_df <- data.frame(subid = rep(NA, 500),
                       PROLIFIC_PID = rep(NA, 500))

raw_file_list = list.files(path=raw_file_path, pattern=NULL, all.files=FALSE, full.names=FALSE)


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
  
  subid <- as.character(df['subid'][1, ])
  date <- as.character(df['date'][1, ])
  row_count <- nrow(df)
  indx <- 0 
  
  for (rowi in 1:row_count){
    
    if (!is.na(df['textbox.text'][rowi,])){
      indx = indx + 1
      print(unlist(df['textbox.text'][rowi,]))
      print(typeof(df['textbox.text'][rowi,]))
      split_response = unlist(strsplit(unlist(df['textbox.text'][rowi,]), split=""))
      split_stimulus = unlist(strsplit(unlist(df['stimulus_triagram'][rowi,]), split=""))
      processed_ind_df['position'][indx, ] <- df['position'][rowi, ]
      processed_ind_df['subid'][indx, ] <- df['subid'][rowi, ]
      processed_ind_df['PROLIFIC_PID'][indx, ] <- df['PROLIFIC_PID'][rowi, ]
      
      for (i in 1:3){
        processed_ind_df[paste('response', as.character(i), sep = "")][indx, 1] <- split_response[i]
        processed_ind_df[paste('stimulus', as.character(i), sep = "")][indx, 1] <- split_stimulus[i]
        processed_ind_df[paste('check', as.character(i), sep = "")][indx, 1] <- as.integer(split_response[i] == split_stimulus[i])
        
      }
      
    }
    
  }
  processed_ind_df <- processed_ind_df[-which(is.na(processed_ind_df)), ] 
  processed_ind_df <- processed_ind_df[order(processed_ind_df$position, decreasing = FALSE), ]
  
  setwd(processed_file_path)
  write_xlsx(processed_ind_df, paste(paste(subid, date, sep='_'), '.xlsx', sep=''))  
  
}





cat("Processing complete /n")

