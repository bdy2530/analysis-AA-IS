# process one experiment folder for up to `max_day` days
process_experiment <- function(experiment,   # e.g. "ActiveAvoidance"
                               file_prefix,    # e.g. "Active.Avoidance_"
                               med_dir,        # e.g. "R:/…/ActiveAvoidance"
                               var_map_xlsx,   # Full path to the Excel mapping file
                               max_day = Inf){
  
  # Create variable mapping directory if it doesn't exist
  var_map_dir <- file.path(dirname(med_dir), "variable_mappings")
  dir.create(var_map_dir, showWarnings = FALSE, recursive = TRUE)
  
  # List MED files
  MedFiles <- list.files(med_dir, full.names=TRUE) %>% 
    setdiff(list.dirs(med_dir, full.names=TRUE, recursive=FALSE))
  
  # ensure ".txt" on every file
  walk(MedFiles, ~ if(!grepl("\\.txt$",.x)) file.rename(.x,paste0(.x,".txt")))
  MedFiles <- list.files(med_dir, full.names=TRUE) %>% 
    setdiff(list.dirs(med_dir, full.names=TRUE, recursive=FALSE))
  
  # Filter files that match the prefix
  MedFiles <- MedFiles[grepl(paste0("^", file_prefix), basename(MedFiles))]
  
  if(length(MedFiles) == 0) {
    warning("No matching files found in directory: ", med_dir)
    return(list(
      Operant_Data = tibble(),
      meta_Data = tibble(),
      settings_num = tibble()
    ))
  }
  
  # 2) load or init
  rd_prefix <- file.path(med_dir, paste0(experiment,"_"))
  op_rds   <- paste0(rd_prefix,"Operant_Data.Rds")
  meta_rds <- paste0(rd_prefix,"meta_Data.Rds")
  set_rds  <- paste0(rd_prefix,"settings_num.Rds")
  
  Operant_Data <- if(file.exists(op_rds))   readRDS(op_rds)   else tibble()
  meta_Data    <- if(file.exists(meta_rds)) readRDS(meta_rds) else tibble()
  settings_num <- if(file.exists(set_rds))  readRDS(set_rds)  else tibble()
  
  
  # 3) loop
  for(f in MedFiles){
    MED <- if (experiment=="ActiveAvoidance") {
      ReadNameMED_ActiveAvoidance(medFile = f, var_map_xlsx = var_map_xlsx)
    } else {
      ReadNameMED_Shock.NoEscape(medFile = f, var_map_xlsx = var_map_xlsx)
    }
    
    # --- build meta.add exactly as before ---
    # pull the day directly from the file’s name:
    fn  <- basename(f)            
    parts <- str_split(fn, "_|\\.")[[1]]     # split on “_” or “.”
    # parts will look like c("Active","Avoidance","Subject09497","active.avoidance","1","Box1","txt")
    file_day <- as.integer(parts[5])         # the 5th element is your “1”
    
    meta.add <- as_tibble(MED$meta) %>%
      mutate(
        DateTime    = mdy_hms(paste0(Date,"T",StartTime),    tz="US/Central"),
        DateTimeEnd = mdy_hms(paste0(Date,"T",EndTime),      tz="US/Central"),
        Day         = file_day                           # ← set Day here
      ) %>%
      select(-Date, -StartTime, -EndTime) %>%
      relocate(DateTime, Subject, Paradigm, Paradigm.Day, Day, Box, ProgramName)
    
    
    meta_Data <- bind_rows(meta_Data, meta.add) %>%
      group_by(Subject) %>%
      arrange(Paradigm, Paradigm.Day) %>%
      mutate(Day = row_number(),
             Paradigm.Day = as.numeric(Paradigm.Day)) %>%
      relocate(Day, .after=Paradigm.Day) %>%
      ungroup()
    
    # --- extract MEDOut, settings_num_add, op.summary.data, op.trial.data, etc. ---
    MEDOut <- tibble(enframe(MED$out)) %>%
      unnest_longer(value) %>%
      group_by(name) %>% mutate(Instance=row_number()) %>% ungroup() %>%
      left_join(meta.add    %>% select(DateTime,Subject,Paradigm,Paradigm.Day), by=character()) %>%
      left_join(meta_Data   %>% select(Subject,Paradigm,Paradigm.Day,Day),      by=c("Subject","Paradigm","Paradigm.Day")) %>%
      relocate(DateTime, Subject, Paradigm, Paradigm.Day)
    
    # settings
    settings_num_add <- MEDOut %>%
      filter(name=="settings") %>%
      select(Subject,Paradigm,Paradigm.Day,Day,name,value,Instance) %>%
      left_join(read_xlsx(var_map_xlsx) %>% select(-Instance,-rename.R) %>% unique() %>% rename(name=DIM),
                by=c("name","Instance")) %>%
      select(-name,-Instance) %>% rename(name=rename.R) %>%
      filter(name!="unused") %>%
      pivot_wider(names_from=name,values_from=value)
    
    # summary
    op.summary.data <- MEDOut %>% filter(name=="summary") %>%
      left_join(read_xlsx(var_map_xlsx) %>% select(-Instance,-rename.R) %>% unique() %>% rename(name=DIM),
                by=c("name","Instance")) %>%
      select(-name,-Instance) %>% rename(name=rename.R) %>%
      filter(name!="unused")
    
    # trial data
    arr_fn <- if(experiment=="ActiveAvoidance") MED.array.detangle else MED.array.detangle.inesc
    op.trial.data <- tibble(enframe(MED$out)) %>%
      unnest_longer(value) %>%
      group_by(name) %>% mutate(Instance=row_number()) %>% ungroup() %>%
      filter(name=="trial.data") %>% pull(value) %>% arr_fn() %>%
      left_join(meta.add %>% select(DateTime,Subject,Paradigm,Paradigm.Day), by=character()) %>%
      left_join(meta_Data   %>% select(Subject,Paradigm,Paradigm.Day,Day),      by=c("Subject","Paradigm","Paradigm.Day")) %>%
      relocate(DateTime, Subject, Paradigm, Paradigm.Day)
    
    # other MEDOut rows you care about
    op.others <- MEDOut %>% filter(name %in% c("elapsed.time","crossings","crossings.shock"))
    
    # bind
    Operant_Data   <- bind_rows(Operant_Data,   op.trial.data, op.summary.data, op.others)
    settings_num   <- bind_rows(settings_num,   settings_num_add)
    
    # move & save
    dir.create(file.path(med_dir,"movedByRScript"), showWarnings=FALSE)
    file.rename(f, file.path(med_dir,"movedByRScript",basename(f)))
    saveRDS(Operant_Data, rd_prefix %>% paste0("Operant_Data.Rds"))
    saveRDS(meta_Data,    rd_prefix %>% paste0("meta_Data.Rds"))
    saveRDS(settings_num, rd_prefix %>% paste0("settings_num.Rds"))
  }
  
  # only up to max_day
  res <- list(
    Operant_Data = Operant_Data %>% as_tibble(),
    meta_Data    = meta_Data %>% as_tibble(),
    settings_num = settings_num %>% as_tibble()
  )
  
  if (nrow(res$meta_Data) > 0 && "Day" %in% names(res$meta_Data)) {
    res <- map(res, ~ filter(.x, Day <= max_day))
  }
  return(res)
}


#— now combine both into “Combined” —
process_and_combine <- function(parent_medpc_dir,
                                var_map_aa,
                                var_map_shock,
                                days_aa    = 7,
                                days_shock = 1) {
  
  # Construct the actual MED file directories - don't add MedPC again
  aa_dir    <- file.path(parent_medpc_dir, "ActiveAvoidance")
  shock_dir <- file.path(parent_medpc_dir, "ShockNoEscape")
  
  # Print directories for debugging
  message("Processing directories in combine:")
  message("AA dir: ", aa_dir)
  message("Shock dir: ", shock_dir)
  
  # Process each experiment
  aa <- process_experiment(
    experiment   = "ActiveAvoidance",
    file_prefix  = "Active.Avoidance_",
    med_dir      = aa_dir,
    var_map_xlsx = var_map_aa,
    max_day      = days_aa
  )
  
  shock <- process_experiment(
    experiment   = "ShockNoEscape",
    file_prefix  = "Shock.NoEscape_",
    med_dir      = shock_dir,
    var_map_xlsx = var_map_shock,
    max_day      = days_shock
  )
  
  # Combine results
  combined_operant <- bind_rows(
    aa$Operant_Data %>% as_tibble(),
    shock$Operant_Data %>% as_tibble()
  ) %>% 
    arrange(Subject, Day, DateTime)
  
  combined_meta <- bind_rows(
    aa$meta_Data %>% as_tibble(),
    shock$meta_Data %>% as_tibble()
  ) %>% 
    arrange(Subject, Day, DateTime)
  
  combined_settings <- bind_rows(
    aa$settings_num %>% as_tibble(),
    shock$settings_num %>% as_tibble()
  ) %>% 
    arrange(Subject, Day)
  
  # Save combined results
  saveRDS(combined_operant,  file.path(parent_medpc_dir, "Combined_Operant_Data.Rds"))
  saveRDS(combined_meta,     file.path(parent_medpc_dir, "Combined_meta_Data.Rds"))
  saveRDS(combined_settings, file.path(parent_medpc_dir, "Combined_settings_num.Rds"))
  
  invisible(list(
    operant = combined_operant,
    meta = combined_meta,
    settings = combined_settings
  ))
}