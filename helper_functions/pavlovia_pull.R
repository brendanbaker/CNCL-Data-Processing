############################
## Pavlovia Pull Function ##
## Updated: BB 5.17.22 #####
############################
# Strip data from csvs

#' pavlovia_pull
#'
#' Function to aggregate data from many csvs.  Primarily designed for use with pavlovia.org experiment output in CSV format.  Input a file destination and receive a list with two components: data and participant_info. 
#' $data is a list of dataframes where each dataframe is one participant's csv contents.  
#' $participant_info contains id numbers, new id numbers if re_number = T, and filenames. 
#'
#' @param filepath String. filepath to a folder containing all participant csv files.  This wrangling function is intended to work with a file structure of 1 csv per participant. 
#' @param id_label String. Label for the participant identifier.  Defaults to "id" 
#' @param col_target NA, numeric, or "auto".  If NA, all files will be read in.  If numeric, only csvs with the specified number of columns will be read in.  If col_target = "auto", only csvs with the mode number of columns will be read.  
#' @param re_number Boolean.  If True, the id numbers will be changed to consecutive numbers.  Original identifiers will be retained in $participant_info
#' @param choose_best Boolean.  If True, duplicate files will be detected.  If there are duplicate files, only the single file with the most columns will be read.
#'
#' @return List with two objects: $data and $participant_info
#' @export
#'
#' @examples 
#' 
#' results <- pavlovia_pull(filepath = "./data", re_number = T) # Get results
#' data <- results$data  # Separate list with data
#' info <- results$participant_info #Separate dataframe with participant information
#' 
#' 
pavlovia_pull <- function(filepath = NA, id_label = "id", col_target = NA, re_number = F, choose_best = T){
  
  
  require(tidyverse)
  
  og_path <- getwd()
  
  # File location 
  if (is.na(filepath)){
    setwd(getwd())
  }
  else {
    setwd(filepath)  
  }
  
  # Initialize list
  val_list <- list()
  
  # Get names #
  raw_filenames <- as.data.frame(list.files(pattern = ".csv")) 
  colnames(raw_filenames) <- c("filename") # Label Filename
  
  raw_filenames$id_label <- str_split(raw_filenames$filename, 
                                      pattern = "_", 
                                      simplify = T)[,1]# Strip to get participant from pavlovia format
  
  colnames(raw_filenames)[2] <- id_label # Give id label from input
  
  val_list$participant_info <- as_tibble(raw_filenames) # Assign to output list
  
  ## For duplicate files
  if (choose_best == T){
    multis <- raw_filenames %>% count(id) %>% filter(n>1) # Identify duplicates
    
    
    singular <- raw_filenames %>% count(id) %>% filter(n == 1) # Identify non-duplicates
    
    
    
    singular <- raw_filenames[raw_filenames$id %in% singular$id,] # Get frame of non-duplicates only
    
    
    bestFile <- data.frame(filename = 1:length(unique(multis$id)), 
                           id = 1:length(unique(multis$id))) # Create df with unique id length
    
    # For every duplicate ID..
    if (nrow(multis)>1) {
      message("Duplicate IDs detected in filenames.  Selecting the file with the most columns.")
      
      ind <- 1
      
      for (dup in unique(multis$id)) {
        
        thisDup <- raw_filenames[raw_filenames$id==dup,] # Select all files with that ID
        pickFile <- data.frame(filename = 1:nrow(thisDup), columns = 1:nrow(thisDup), id = 1:nrow(thisDup))
        
        # For each file with that ID...
        for (subdup in 1:nrow(thisDup)) {
          thisSub <- thisDup[subdup,]
          thisFile <- try(suppressWarnings(suppressMessages(read_csv(thisSub$filename)))) # Read the file
          pickFile[subdup,2] <- ncol(thisFile) # Determine ncols
          pickFile[subdup,1] <- thisSub$filename
          pickFile[subdup,3] <- thisSub$id
        }
        pickFile <- pickFile %>% arrange(desc(columns)) %>% 
          slice_max(1) %>% select(-columns) # Take file with most columns
        
        bestFile[ind,] <- pickFile
        remove(pickFile)
        ind <- ind + 1
      }
      
      
      fixed <- bind_rows(singular, bestFile)
      message(paste0(as.character(sum(multis$n)-nrow(bestFile)), " duplicate files removed."))
      
      
      raw_filenames <- fixed
      val_list$participant_info <- as_tibble(raw_filenames) 
    }
    
  }
  
  
  
  
  
  
  # Get data #
  part_list <- list()
  for (i in 1:nrow(raw_filenames)) {
    part_list[[i]] <- try(read.csv(file = raw_filenames$filename[i], 
                                   header = T, sep = ","), silent = T) # Loop to read each csv
    
    if (re_number == T){
      names(part_list)[i] <- as.character(i) # Assign consecutive number if true
    }
    if (re_number == F){
      names(part_list)[i] <- raw_filenames[,2][i]  # Leave IDs as number if false
    }
  }
  
  if (re_number == T & is.na(col_target)){
    val_list$participant_info$newID <- names(part_list) # And update this in info
    colnames(val_list$participant_info)[3] <- "new_id"
    
  }
  
  # Filter Columns #
  if (!is.na(col_target)){
    
    # Filter to frames with only desired number of columns 
    # (should be the same for all who actually completed it)
    missing <- vector()
    
    if (col_target == "auto"){
      getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
      }
      len_list <- vector()
      for (i in 1:length(part_list)){
        len_list[i] <- (length(part_list[[i]]))
      }
      
      col_target <- getmode(len_list)
      message(paste0("Automatic column target: ", col_target))
    }
    
    for (i in 1:length(part_list)){
      if (length(part_list[[i]])==col_target){
        missing[i] <- i
      }
    }
    
    
    # Remove those participants from the participant list
    new_part_list <- part_list[na.omit(missing)] # Omit frames that did not meet threshold
    message(paste0(as.character(length(missing[is.na(missing)])), " participants removed.")) # Message
    val_list$participant_info <- val_list$participant_info[na.omit(missing),] # Remove these from info as well
    
    message(paste("Returned "), length(new_part_list), " observations.")
    
    if (re_number == T){
      names(new_part_list) <- as.character(1:length(names(new_part_list)))  # Re-assign consecutive names
      val_list$participant_info$newID <- names(new_part_list) # And update this in info
      colnames(val_list$participant_info)[3] <- "new_id"
      
    }
    
    val_list$data <- new_part_list # Add to output
    
    message("List re-numbered.")
  }
  
  # If no target, only pull the info without making changes
  if (is.na(col_target)){
    message(paste("Returned "), length(part_list), " observations.")
    val_list$data <- part_list
    
    if (re_number == T){
      message("List re-numbered.")
    }
  }
  
  setwd(og_path)
  return(val_list)
}
