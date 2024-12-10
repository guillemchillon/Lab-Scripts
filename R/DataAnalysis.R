# Load the necessary libraries----
library(tidyverse)
library(readxl)
library(writexl)
library(str2str)

# Input variable arguments----
dataFolder <- "qPCR_Files"
versionFolder <- "240828_20mice"
PositiveControl <- "PosCtrl"

# Load data files-----

finalFolder <- file.path(dataFolder, versionFolder)

filesInFolder <- list.files(finalFolder, 
                            pattern = "^[^~]*Summary.*$",
                            all.files = FALSE,
                            full.names =TRUE)

print(filesInFolder)
summary_list <- lapply(filesInFolder, read_csv)
summary <- bind_rows(summary_list)

# Calculate the Delta Ct and Delta Delta Ct values----
summary_filtered <- summary %>% select(Sample, Target, Cq)
summary_average <- summary_filtered %>% 
  group_by(Sample, Target) %>% 
  summarise(Avg_Cq = mean(Cq, na.rm = TRUE))

TERT_values <- summary_average %>% filter(Target == "Tert") %>%
  select(Sample, Avg_Cq)
colnames(TERT_values) <- c("Sample", "TERT_Cq")

summary_deltaCt <- summary_average %>% 
  filter(Target %in% c("WT", "KO", "Neo", "Sry")) %>%
  left_join(TERT_values, by = "Sample") %>%
  mutate(Delta_Ct = Avg_Cq - TERT_Cq) %>% 
  select(Sample, Target, Delta_Ct)

control_deltaCt <- summary_deltaCt %>% filter(Sample == PositiveControl)
colnames(control_deltaCt) <- c("Sample", "Target", "Control_Ct")
control_deltaCt <- control_deltaCt %>% ungroup() %>% select(Target, Control_Ct)

summary_deltadeltaCt <- summary_deltaCt %>% 
  left_join(control_deltaCt, by = "Target") %>%
  mutate(Delta_Delta_Ct = Delta_Ct - Control_Ct) %>%
  mutate(FC_norm = 2^(-Delta_Delta_Ct)) %>%
  select(Sample, Target, Delta_Delta_Ct, FC_norm) %>% 
  arrange(Sample, Target)

# Create the final summary tables----
pivot_summary <- summary_deltadeltaCt %>%
  select(Sample, Target, FC_norm) %>%
  pivot_wider(names_from = Target, values_from = FC_norm)

WT_Male <- pivot_summary %>% 
  filter(!is.na(WT) & !is.na(Sry)) %>%
  filter(is.na(KO)) %>% 
  select(Sample)

WT_Female <- pivot_summary %>% 
  filter(!is.na(WT)) %>%
  filter(is.na(KO) & is.na(Sry)) %>% 
  select(Sample)

KO_Male <- pivot_summary %>% 
  filter(!is.na(WT) & !is.na(Sry) & !is.na(KO)) %>%
  select(Sample)

KO_Female <- pivot_summary %>% 
  filter(!is.na(WT) & !is.na(KO)) %>%
  filter(is.na(Sry)) %>% 
  select(Sample)

final_summary <- cbind_fill(WT_Male, WT_Female, KO_Male, KO_Female)
colnames(final_summary) <- c("Male WT", "Male KO", "Female WT", "Female KO")

# Export the final summary tables----
xl_lst <- list("Genotype Table" = final_summary, 
               "Quantification Summary" = pivot_summary)

finalFileName <- paste0(finalFolder,"/", Sys.Date(), "_resultsAnalysis.xlsx")
write_xlsx(xl_lst, path = finalFileName)
