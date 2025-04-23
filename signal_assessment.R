#' Assess photometry signal quality based on GuPPy screening notes
#'
#' @param synapse_eval_link URL or path to the Google sheet with synapse evaluations
#' @param setup_metadata_link URL or path to the Google sheet with subject metadata
#' @param operant_data_rds path to the Operant_Data.Rds file
#' @param meta_data_rds    path to the meta_Data.Rds file
#' @param min_days integer: minimum number of “keep” days per subject/structure (default 0)
#' @return a tibble of filtered (Subject,Structure,Date,eval,verdict)

assess_signal_quality <- function(
    synapse_eval_link,
    setup_metadata_link,
    operant_data_rds,
    meta_data_rds,
    min_days = 0
){
  # 1. read inputs
  synapse_eval    <- read_sheet(synapse_eval_link) %>% as_tibble()
  setup_metadata  <- read_sheet(setup_metadata_link) %>% 
    transmute(Subject = as.character(Subject), Group, Sex)
  Operant_Data    <- readRDS(operant_data_rds) %>% 
    mutate(Subject = as.character(Subject)) %>% 
    left_join(setup_metadata, by="Subject")
  meta_Data       <- readRDS(meta_data_rds)
  
  # 2. trim synapse evaluations
  synapse_eval_trim <- synapse_eval %>% 
    select(-Notes, -Artifacts_DMS, -Artifacts_DLS) %>%
    transmute(
      Subject = str_extract(Filename, "^\\d+(?=-)"),
      Date    = str_c("20", str_match(Filename, "(?<=-)\\d{6}")),
      Quality_DMS, Verdict_DMS,
      Quality_DLS, Verdict_DLS
    ) %>%
    pivot_longer(
      cols = starts_with("Quality"),
      names_to  = "Structure",
      values_to = "eval"
    ) %>%
    separate(eval, into = c("eval","verdict"), " ") %>%
    mutate(
      Structure = str_extract(Structure,"(?<=_).*")
    ) %>%
    filter(
      eval != "poor",
      str_detect(verdict,"keep")
    ) %>%
    group_by(Subject, Structure) %>%
    filter(n() >= min_days) %>%
    ungroup()
  
  # 3. return
  synapse_eval_trim
}