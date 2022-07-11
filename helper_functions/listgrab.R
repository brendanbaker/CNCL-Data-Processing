#######################
## Listgrab function ##

# Follow up function to pavlovia_pull.  Aggregates specific variables from the $data list into one tibble. 

listgrab <- function(data, vars, clean = T, is_pattern = F){
  #' listgrab
  #' A function for subsetting data returned by pavlovia_pull. 
  #'
  #' @param data List.  The $data object returned by pavlovia_pull 
  #' @param vars String.  The variable names you would like to subset
  #' @param clean Boolean.  The function will attempt to remove blank responses if true. 
  #' @param is_pattern Boolean.  If True, the function will return all variables containing the string specified in vars
  #'
  #' @return Returns a tibble with the desired variables as well as the associated IDs. 
  #' @export
  #'
  #' @examples AUTs <- listgrab(data, vars = "AUT", is_pattern = T)
  require(dplyr)
  
  ## Initializing returned data frame
  fulldf <- data.frame()
  
  
  # Loops through each frame in list and takes the vars specified in vars =
  for (name in names(data)) {
    
    tempdf <- data[[name]]
    
    if (is_pattern == F){
      if((F %in% (vars %in% colnames(tempdf)))==F){
        
        currdf <- tempdf %>% dplyr::select(vars)
        currdf$id <- name
        
        fulldf <- rbind.data.frame(fulldf, currdf)
        remove(currdf)
        
      }
      
    }
    
    # If there are multiple desired variables that follow a pattern, specify the pattern.
    if (is_pattern == T){
      coltarget <- str_subset(colnames(tempdf), pattern = vars)
      currdf <- tempdf %>% dplyr::select(coltarget)
      currdf$id <- name
      
      fulldf <- try(rbind.data.frame(fulldf, currdf))
      remove(currdf)
    }
    
    remove(tempdf)
  }
  
  fulldf <- as_tibble(fulldf)
  
  if (clean == T){
    fulldf <- na.omit(fulldf)
  }
  
  return(fulldf)
  
}
