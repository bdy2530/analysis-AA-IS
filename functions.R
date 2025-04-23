# Read MED file Active Avoidance function ----------------------------------------------
ReadNameMED_ActiveAvoidance <- function(MedFiles, i0, var_map_xlsx){
  if (missing(MedFiles) || length(MedFiles) < i0) {
    stop("No MED file available at index ", i0)
  }
  if (!file.exists(var_map_xlsx)) {
    stop("Mapping file not found: ", var_map_xlsx)
  }
  
  # Read the Excel file that contains a conversion table.
  # This table maps MED variable names (in column “DIM”) to their desired R names (in column “R”).
  # We drop the Instance and rename.R columns and keep only unique rows.
  MED.R.Conversion <- read_xlsx(var_map_xlsx) %>%
    select(!c(Instance, rename.R)) %>% unique()
  ###### This section reads the MED file, converts the text into numbers, and renames the output variables.
  # Read the content of the file (using the i0th file in MedFiles) into a list with a $text element.
  contents <- readtext(MedFiles[i0])
  
  #put -987.987 in for empty variables or DIMs, can remove later to make parsing file easier
  #-987.987 in first position of a DIM should not signify anything, can empty it later or ignore.
  contents<-stringr::str_replace_all(contents$text,"(?<=[[:upper:]]):(?=\\n[[:upper:]])",": -987.987")
  
  # Extract the Start Date from the file.
  # The regex looks for text following "\nStart Date: " until the next newline, then replaces "/" with "-".
  StartDate <- stringr::str_replace_all(stringr::str_extract(contents, "(?<=\\nStart Date: )\\S+(?=\\n)"),"/", "-")
  
  # Extract Start Time and End Time in a similar fashion.
  StartTime <- stringr::str_extract(contents, "(?<=\\nStart Time: )\\S+(?=\\n)")
  EndTime <- stringr::str_extract(contents, "(?<=\\nEnd Time: )\\S+(?=\\n)")
  
  # Find a list of variable names (which are single uppercase letters) that are immediately before a colon.
  # The regex uses a positive lookbehind for a newline and a positive lookahead for ":".
  MedVarNames <- stringr::str_extract_all(contents,"(?<=\\n)[[:upper:]](?=:)") #gives Variable Names
  MedVarNames2 <-  MedVarNames[[1]][1:length(MedVarNames[[1]])]
  
  # Split the contents into pieces by using the pattern of a newline followed by an uppercase letter and a colon.
  # This isolates the block of text corresponding to each variable.
  MedVars <- stringr::str_split(contents, "(?<=\\n)[[:upper:]]:") #split by variable name
  MedVars2 <- MedVars[[1]][2:length(MedVars[[1]])] #eliminate header
  
  
  # Prepare to convert the variable blocks into a list of numeric vectors.  
  out <- MedVars2
  out <- stringr::str_replace_all(out,"\\n","")  #remove newlines
  out <- stringr::str_replace_all(out,"\\s{1,}\\d{1,}:","") #remove line numbers
  out <- stringr::str_replace_all(out,"\\s{1,}",",") #remove spaces with a comma
  out <- stringr::str_replace_all(out,"^,","") #remove leading comma
  out <- stringr::str_split(out,",") #split each block into a character vector of values using commas as separators
  
  #next two lines just for testing
  #MedVarNamesOrg <- MedVarNames2
  #outOrg <- outOrg
  
  # Loop over each variable’s character vector and:  
  for (i in 1:length(out)){
    out[[i]] <- as.double(out[[i]]) #converts variable to double
    
    # If the original variable name (from MedVarNames2) is found in the conversion table's DIM column,
    # replace it with the corresponding R name from the conversion table.    
    if (MedVarNames2[[i]] %in% MED.R.Conversion$DIM){
      MedVarNames2[[i]] = MED.R.Conversion$R[which(MED.R.Conversion$DIM == MedVarNames2[[i]])]
    }
  }
  
  out <- lapply(out, function(x) replace(x, x==-987.987, NA)) #replaces placeholder numbers with NAs
  out <- lapply(out, function(xi) if(all(xi == 0) & (length(xi) > 10)){ #This removes any totally zero filled arrays
    xi <- as.double(NA)
  } else{
    xi <- xi
  }) #replaces placeholder numbers with NAs
  
  out <- lapply(out, function(x) x <- x[x != ""]) #remove empty character vectors
  out <- lapply(out, function(x) if (!all(is.na(x))){x <- x[!is.na(x)]}) #removes NAs that aren't alone
  
  # #this is for replacing bulk zeros, looks for 10 or more in a row and truncates variable just prior to first zero in the run of zeros
  # out <- lapply(out, function(x) if (length(x) > 9 & purrr::detect_index(zoo::rollapply(stringr::str_detect(as.character(x), "^0$"), 10, sum), ~ .x == 10) > 0){
  #   x <- x[1:purrr::detect_index(zoo::rollapply(stringr::str_detect(as.character(x), "^0$"), 10, sum), ~ .x == 10)-1]
  # } else {x <- x})
  
  names(out) <- MedVarNames2 #MAKE NAME CHANGES JUST ABOVE, BREAK OUT ANY COUNTERS YOU LIKE
  
  #cleanup output
  out <- out[which(stringr::str_length(names(out)) > 1)] #removes variables without names
  
  
  #extract data from file naming convention
  fileNameData <- stringr::str_split(MedFiles[i0],"/|\\\\")[[1]] #parse path
  fileNameData <- stringr::str_replace(fileNameData[length(fileNameData)], ".txt", "") #get filename
  fileNameDataSplt <- stringr::str_split(fileNameData,"_") #split parts of filename
  LfileNameDataSplt <- as.list(fileNameDataSplt[[1]]) #make list
  LfileNameDataSplt[[2]] <- stringr::str_extract(LfileNameDataSplt[[2]],"(?<=Subject).*") #remove Subject label from number
  LfileNameDataSplt[[4]] <- as.numeric(LfileNameDataSplt[[4]])
  LfileNameDataSplt[[5]] <- stringr::str_remove(LfileNameDataSplt[5],"Box")
  names(LfileNameDataSplt) <- c("Experiment", "Subject","Paradigm", "Paradigm.Day", "Box") #label list
  LfileNameDataSplt <- LfileNameDataSplt[1:5]
  
  #extract program title from file and add to other non-numeric data
  ProgramName <- stringr::str_extract_all(contents,"(?<=\\nMSN: ).*(?=\\n)") #gives Variable Names
  LfileNameDataSplt <- c(LfileNameDataSplt, ProgramName= ProgramName, Date = StartDate, StartTime = StartTime, EndTime = EndTime) #add filename data to output variable
  
  
  #out2 <- list(out = out, meta = LfileNameDataSplt)
  return(list(out = out, meta = LfileNameDataSplt))
}

# Active Avoidance Array Detangler ------------------------------------------

MED.array.detangle <- function(arr){
  # Define the number of columns (dimensions) in the array.
  # In this case, each trial’s data is expected to be arranged in 15 columns.
  dim_row_number <- 15
  
  # Convert the flat (linear) array into a tibble with three columns:
  # 'row' – which trial (row) the value belongs to,
  # 'col' – the position within that trial (column),
  # 'value' – the actual numeric value.
  data_tibble <- tibble(
    row = rep(1:ceiling(length(arr) / dim_row_number), each = dim_row_number)[1:length(arr)],
    col = rep(1:dim_row_number, length.out = length(arr)),
    value = arr
  )
  
  # Reshape the tibble from long format to wide format.
  # Each unique column number becomes a separate column in the resulting tibble,
  # with column names prefixed with "V" (e.g., V1, V2, …, V15).
  reshaped_tibble <- data_tibble %>%
    pivot_wider(names_from = col, values_from = value, names_prefix = "V") %>% 
    select(!row)
  
  # Rename the columns to meaningful variable names.
  # The renaming assumes exactly 15 columns are present.
  names(reshaped_tibble) <- c("Trial", "avoid.tag", "avoid.latency","escape.tag", "escape.latency", 
                              "left.move.activity", "right.move.activity", "crossings.count", "ITI.shocks", "Avoid", 
                              "Escape", "Shock", "Cue", "Cue.End", "unused")
  
  reshaped_tibble <- reshaped_tibble %>% 
    select(!c(avoid.tag, escape.tag, left.move.activity, right.move.activity, ITI.shocks, unused)) %>% #remove unused columns
    pivot_longer(!Trial, names_to = "name", values_to = "value") %>% #convert to long format
    filter((value > 0 | name == "Cue")) %>% group_by(name) %>% #remove zero values, keep Cue, group by name
    arrange(name, Trial) %>%  #arrange in order of Trial
    mutate(Instance = row_number()) %>% #add Instance column 
    arrange(Trial) #arrange in order of Trial
  
}


# Read MED file Shock.NoEscape function ----------------------------------------------
ReadNameMED_Shock.NoEscape <- function(MedFiles, i0, var_map_xlsx){
  if (missing(MedFiles) || length(MedFiles) < i0) {
    stop("No MED file available at index ", i0)
  }
  if (!file.exists(var_map_xlsx)) {
    stop("Mapping file not found: ", var_map_xlsx)
  }
  
  # Read the Excel file that contains a conversion table.
  # This table maps MED variable names (in column “DIM”) to their desired R names (in column “R”).
  # We drop the Instance and rename.R columns and keep only unique rows.
  MED.R.Conversion <- read_xlsx(var_map_xlsx) %>%
    select(!c(Instance, rename.R)) %>% unique()
  ###### This section reads the MED file, converts the text into numbers, and renames the output variables.
  # Read the content of the file (using the i0th file in MedFiles) into a list with a $text element.
  contents <- readtext(MedFiles[i0])
  
  #put -987.987 in for empty variables or DIMs, can remove later to make parsing file easier
  #-987.987 in first position of a DIM should not signify anything, can empty it later or ignore.
  contents<-stringr::str_replace_all(contents$text,"(?<=[[:upper:]]):(?=\\n[[:upper:]])",": -987.987")
  
  # Extract the Start Date from the file.
  # The regex looks for text following "\nStart Date: " until the next newline, then replaces "/" with "-".
  StartDate <- stringr::str_replace_all(stringr::str_extract(contents, "(?<=\\nStart Date: )\\S+(?=\\n)"),"/", "-")
  
  # Extract Start Time and End Time in a similar fashion.
  StartTime <- stringr::str_extract(contents, "(?<=\\nStart Time: )\\S+(?=\\n)")
  EndTime <- stringr::str_extract(contents, "(?<=\\nEnd Time: )\\S+(?=\\n)")
  
  # Find a list of variable names (which are single uppercase letters) that are immediately before a colon.
  # The regex uses a positive lookbehind for a newline and a positive lookahead for ":".
  MedVarNames <- stringr::str_extract_all(contents,"(?<=\\n)[[:upper:]](?=:)") #gives Variable Names
  MedVarNames2 <-  MedVarNames[[1]][1:length(MedVarNames[[1]])]
  
  # Split the contents into pieces by using the pattern of a newline followed by an uppercase letter and a colon.
  # This isolates the block of text corresponding to each variable.
  MedVars <- stringr::str_split(contents, "(?<=\\n)[[:upper:]]:") #split by variable name
  MedVars2 <- MedVars[[1]][2:length(MedVars[[1]])] #eliminate header
  
  
  # Prepare to convert the variable blocks into a list of numeric vectors.  
  out <- MedVars2
  out <- stringr::str_replace_all(out,"\\n","")  #remove newlines
  out <- stringr::str_replace_all(out,"\\s{1,}\\d{1,}:","") #remove line numbers
  out <- stringr::str_replace_all(out,"\\s{1,}",",") #remove spaces with a comma
  out <- stringr::str_replace_all(out,"^,","") #remove leading comma
  out <- stringr::str_split(out,",") #split each block into a character vector of values using commas as separators
  
  #next two lines just for testing
  #MedVarNamesOrg <- MedVarNames2
  #outOrg <- outOrg
  
  # Loop over each variable’s character vector and:  
  for (i in 1:length(out)){
    out[[i]] <- as.double(out[[i]]) #converts variable to double
    
    # If the original variable name (from MedVarNames2) is found in the conversion table's DIM column,
    # replace it with the corresponding R name from the conversion table.    
    if (MedVarNames2[[i]] %in% MED.R.Conversion$DIM){
      MedVarNames2[[i]] = MED.R.Conversion$R[which(MED.R.Conversion$DIM == MedVarNames2[[i]])]
    }
  }
  
  out <- lapply(out, function(x) replace(x, x==-987.987, NA)) #replaces placeholder numbers with NAs
  out <- lapply(out, function(xi) if(all(xi == 0) & (length(xi) > 10)){ #This removes any totally zero filled arrays
    xi <- as.double(NA)
  } else{
    xi <- xi
  }) #replaces placeholder numbers with NAs
  
  out <- lapply(out, function(x) x <- x[x != ""]) #remove empty character vectors
  out <- lapply(out, function(x) if (!all(is.na(x))){x <- x[!is.na(x)]}) #removes NAs that aren't alone
  
  # #this is for replacing bulk zeros, looks for 10 or more in a row and truncates variable just prior to first zero in the run of zeros
  # out <- lapply(out, function(x) if (length(x) > 9 & purrr::detect_index(zoo::rollapply(stringr::str_detect(as.character(x), "^0$"), 10, sum), ~ .x == 10) > 0){
  #   x <- x[1:purrr::detect_index(zoo::rollapply(stringr::str_detect(as.character(x), "^0$"), 10, sum), ~ .x == 10)-1]
  # } else {x <- x})
  
  names(out) <- MedVarNames2 #MAKE NAME CHANGES JUST ABOVE, BREAK OUT ANY COUNTERS YOU LIKE
  
  #cleanup output
  out <- out[which(stringr::str_length(names(out)) > 1)] #removes variables without names
  
  
  #extract data from file naming convention
  fileNameData <- stringr::str_split(MedFiles[i0],"/|\\\\")[[1]] #parse path
  fileNameData <- stringr::str_replace(fileNameData[length(fileNameData)], ".txt", "") #get filename
  fileNameDataSplt <- stringr::str_split(fileNameData,"_") #split parts of filename
  LfileNameDataSplt <- as.list(fileNameDataSplt[[1]]) #make list
  LfileNameDataSplt[[2]] <- stringr::str_extract(LfileNameDataSplt[[2]],"(?<=Subject).*") #remove Subject label from number
  LfileNameDataSplt[[4]] <- as.numeric(LfileNameDataSplt[[4]])
  LfileNameDataSplt[[5]] <- stringr::str_remove(LfileNameDataSplt[5],"Box")
  names(LfileNameDataSplt) <- c("Experiment", "Subject","Paradigm", "Paradigm.Day", "Box") #label list
  LfileNameDataSplt <- LfileNameDataSplt[1:5]
  
  #extract program title from file and add to other non-numeric data
  ProgramName <- stringr::str_extract_all(contents,"(?<=\\nMSN: ).*(?=\\n)") #gives Variable Names
  LfileNameDataSplt <- c(LfileNameDataSplt, ProgramName= ProgramName, Date = StartDate, StartTime = StartTime, EndTime = EndTime) #add filename data to output variable
  
  
  #out2 <- list(out = out, meta = LfileNameDataSplt)
  return(list(out = out, meta = LfileNameDataSplt))
  
}

# Inescp Shock Array Detangler ------------------------------------------
MED.array.detangle.inesc <- function(arr){
  # Define the number of columns (dimensions) in the array.
  # In this case, each trial’s data is expected to be arranged in 15 columns.
  dim_row_number <- 15
  
  # Convert the flat (linear) array into a tibble with three columns:
  # 'row' – which trial (row) the value belongs to,
  # 'col' – the position within that trial (column),
  # 'value' – the actual numeric value.
  data_tibble <- tibble(
    row = rep(1:ceiling(length(arr) / dim_row_number), each = dim_row_number)[1:length(arr)],
    col = rep(1:dim_row_number, length.out = length(arr)),
    value = arr
  )
  
  # Reshape the tibble from long format to wide format.
  # Each unique column number becomes a separate column in the resulting tibble,
  # with column names prefixed with "V" (e.g., V1, V2, …, V15).
  reshaped_tibble <- data_tibble %>%
    pivot_wider(names_from = col, values_from = value, names_prefix = "V") %>% 
    select(!row)
  
  # Rename the columns to meaningful variable names.
  # The renaming assumes exactly 15 columns are present.
  names(reshaped_tibble) <- c("Trial", "avoid.tag", "avoid.latency","escape.tag", "escape.latency", 
                              "left.move.activity", "right.move.activity", "crossings.count", "ITI.shocks", "Avoid", 
                              "Escape", "Shock", "Cue", "Cue.End", "unused")
  
  reshaped_tibble <- reshaped_tibble %>% 
    select(!c(avoid.tag, escape.tag, left.move.activity, right.move.activity, ITI.shocks, unused)) %>% #remove unused columns
    pivot_longer(!Trial, names_to = "name", values_to = "value") %>% #convert to long format
    filter((value > 0 | name == "Cue")) %>% group_by(name) %>% #remove zero values, keep Cue, group by name
    arrange(name, Trial) %>%  #arrange in order of Trial
    mutate(Instance = row_number()) %>% #add Instance column 
    arrange(Trial) #arrange in order of Trial
  
}