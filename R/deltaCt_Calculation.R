# Load the necessary libraries----
library(tidyverse)
library(readxl)
library(writexl)
library(str2str)

# Calculate deltaCt and delta delta Ct-----

deltaCt <- function(path, housekeeping, positiveCtrl){

  filesInFolder <- list.files(path, 
                              pattern = "^[^~]*Summary.*$",
                              all.files = FALSE,
                              full.names =TRUE)
  
  summary_list <- lapply(filesInFolder, read_csv)
  summary <- bind_rows(summary_list)
  
  summary_filtered <- summary %>% select(Sample, Target, Cq)
  summary_average <- summary_filtered %>% 
    group_by(Sample, Target) %>% 
    summarise(Avg_Cq = mean(Cq, na.rm = TRUE))
  
  housekeeping_values <- summary_average %>% filter(Target == housekeeping) %>%
    select(Sample, Avg_Cq)
  housekeepingName <- paste0(housekeeping, "_Cq")
  colnames(housekeeping_values) <- c("Sample", housekeepingName)
  
  summary_deltaCt <- summary_average %>% 
    filter(!Target %in% housekeeping) %>%
    left_join(housekeeping_values, by = "Sample") %>%
    mutate(Delta_Ct = Avg_Cq - Tert_Cq) %>% 
    select(Sample, Target, Delta_Ct)
  
  control_deltaCt <- summary_deltaCt %>% filter(Sample == positiveCtrl)
  colnames(control_deltaCt) <- c("Sample", "Target", "Control_Ct")
  control_deltaCt <- control_deltaCt %>% ungroup() %>% select(Target, Control_Ct)
  
  summary_deltadeltaCt <- summary_deltaCt %>% 
    left_join(control_deltaCt, by = "Target") %>%
    mutate(Delta_Delta_Ct = Delta_Ct - Control_Ct) %>%
    mutate(FC_norm = 2^(-Delta_Delta_Ct)) %>%
    select(Sample, Target, Delta_Delta_Ct, FC_norm) %>% 
    arrange(Sample, Target)
  
  pivot_summary <- summary_deltadeltaCt %>%
    select(Sample, Target, FC_norm) %>%
    pivot_wider(names_from = Target, values_from = FC_norm)
}

path = "qPCR_Files/240828_20mice"
pivot_summary <- deltaCt(path, "Tert", "336.2")

# Create the final summary tables----
get_samples <- function(df, conditions) {
  filter_expr <- purrr::map2(names(conditions), 
                             conditions, 
                             ~expr((!is.na(.data[[!!.x]]) == !!.y)))
  df %>% 
    filter(!!!filter_expr) %>%
    select(Sample)
}

conditions_list <- list(
  WT_Male = c(WT = TRUE, Neo = FALSE, Sry = TRUE),
  WT_Female = c(WT = TRUE, Neo = FALSE, Sry = FALSE),
  KO_Male = c(WT = TRUE, Neo = TRUE, Sry = TRUE),
  KO_Female = c(WT = TRUE, Neo = TRUE, Sry = FALSE))

samples_list <- map(conditions_list, ~get_samples(pivot_summary, .x)$Sample)

makePaddedDataFrame <- function(l) {
  maxlen <- max(map_int(l, length))
  data.frame(map(l, ~ c(.x, rep(NA, maxlen - length(.x)))))
}

final_summary <- makePaddedDataFrame(samples_list)

# Export the final summary tables----
xl_lst <- list("Genotype Table" = final_summary, 
               "Quantification Summary" = pivot_summary)

finalFileName <- paste0(path,"/", Sys.Date(), "_resultsAnalysis.xlsx")
write_xlsx(xl_lst, path = finalFileName)
