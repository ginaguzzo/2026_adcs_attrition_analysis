## Merge tables for treatment status from different parts of analysis.

library(dplyr)
library(tidyr)
library(stringr)
library(readxl) 
library(openxlsx)

setwd("/mnt/c/Users/a1640999/Documents/research_work/2025_study_retention_analysis/Analysis/")



# Import data -------------------------------------------------------------

## Follow-up info for participants who were treated at private clinics.
## This table was generated from the financial analysis and only has those who went to private clinics, it is also outdated because some people
## finished their treatment after this table was made.
private_clinics <- read_excel("metadata/treatment_status_workflow/all_metadata_financial_2025.xlsx")


## Full ID list of participants.
## This list has the full study IDs list and a more comprehensive treatment status of all participants. It also includes whether participants were
## followed up or not.
full_id_list <- read.csv("metadata/treatment_status_workflow/id_list.csv",
                         row.names = NULL)


## Treatment status generated from qual analyses.
## This list was generated for qual analysis, includes updates on treatment statuses based on interview data.
updated_treatment_status <- read_excel("metadata/treatment_status_workflow/Box_referrals_and_treatment_8_oct_2025.xlsx")
updated_treatment_status$`ID (ascending)` <-  as.character(updated_treatment_status$`ID (ascending)`)


## Comprehensive Redcap data.
## These files were downloaded directly from Redcap in Jan 2026, they include all baseline and follow-up data on Redcap.

## Adult Redcap data
redcap_adult <- read.csv("metadata/treatment_status_workflow/redcap_adult_followup_data.csv",
                         row.names = NULL)
redcap_adult$participant_id <-  as.character(redcap_adult$participant_id)

## Child Redcap data.
redcap_child <- read.csv("metadata/treatment_status_workflow/redcap_child_followup_data.csv",
                         row.names = NULL)
redcap_child$participant_id <-  as.character(redcap_child$participant_id)
redcap_child$participant_id <- paste(redcap_child$participant_id, "C", sep = "") #Add C to end of child participant IDs.




# Merge tables ------------------------------------------------------------

## Join private clinics list to full IDs.
full_id_list <- full_id_list %>%
  left_join(
    private_clinics %>%
      rename(treatment_status_private = `treatment_status (complete, in progress, incomplete)`) %>%
      select(p_ID, treatment_status_private),
    by = "p_ID"
  )


## Join updated treatment status list from qual data to full IDs.
full_id_list <- full_id_list %>%
  left_join(
    updated_treatment_status %>%
      rename(p_ID = `ID (ascending)`, 
             treatment_type_qual = Treatment, 
             notes_qual = Notes) %>%
      select(p_ID, treatment_type_qual, notes_qual),
    by = "p_ID"
  )


## Join redcap data on adults to full IDs.
full_id_list <- full_id_list %>%
  left_join(
    redcap_adult %>%
      rename(p_ID = participant_id, 
             start_den_trt_adult = start_den_trt, 
             complete_den_trt_adult = complete_den_trt, 
             location_den_trt_adult = location_dent_trt) %>%
      select(p_ID, start_den_trt_adult, complete_den_trt_adult, location_den_trt_adult),
    by = "p_ID"
  )


## Join redcap data on children to full IDs.
full_id_list <- full_id_list %>%
  left_join(
    redcap_child %>%
      rename(p_ID = participant_id, 
             start_den_trt_child = start_dent_trt,
             start_den_trt_parent = start_dent_trt_v2,
             complete_den_trt_child = cmplt_dent_trt,
             complete_den_trt_parent = cmplt_dent_trt_v2, 
             location_den_trt_child = location_dnt_trt,
             location_den_trt_parent = location_dnt_trt_v2,) %>%
      select(p_ID, 
             start_den_trt_child,
             start_den_trt_parent,
             complete_den_trt_child,
             complete_den_trt_parent,
             location_den_trt_child,
             location_den_trt_parent),
    by = "p_ID"
  )



# Merge treatment status columns ------------------------------------------

## Treatment status
full_id_list <- full_id_list %>%
  mutate(
    treatment_status_redcap = case_when(
      # Completed treatment
      complete_den_trt_adult == 1 |
      complete_den_trt_child == 1 |
      complete_den_trt_parent == 1 ~ "complete",
      
      # Started but did not complete
      (start_den_trt_adult == 1 & (complete_den_trt_adult == 0 | is.na(complete_den_trt_adult))) |
      (start_den_trt_child == 1 & (complete_den_trt_child == 0 | is.na(complete_den_trt_child))) |
      (start_den_trt_parent == 1 & (complete_den_trt_parent == 0 | is.na(complete_den_trt_parent))) ~ "incomplete",
      
      # Did not start treatment
      start_den_trt_adult == 0 |
      start_den_trt_child == 0 |
      start_den_trt_parent == 0 ~ "none",
      
      # Not followed up
      TRUE ~ NA_character_
    )
  )


## Treatment location
full_id_list <- full_id_list %>%
  mutate(
    treatment_type_redcap = case_when(
      location_den_trt_adult == 4 |
      location_den_trt_child == 4 |
      location_den_trt_parent == 4 ~ "private and public",
      
      location_den_trt_adult %in% c(2, 3) |
      location_den_trt_child %in% c(2, 3) |
      location_den_trt_parent %in% c(2, 3) ~ "public",
      
      location_den_trt_adult == 1 |
      location_den_trt_child == 1 |
      location_den_trt_parent == 1 ~ "private",
      
      TRUE ~ NA_character_
    )
  )


## Make new table with essential columns
complete_treatment_status <- full_id_list %>%
  select(p_ID,
         adult_child,
         referral_type,
         followed_up,
         treatment_status_private,
         treatment_type_qual,
         notes_qual,
         treatment_status_redcap,
         treatment_type_redcap)


# Save table --------------------------------------------------------------

## Save table and go cross-ref Redcap and Box notes to complete missing data manually.
write.xlsx(complete_treatment_status, 
           file = "metadata/treatment_status_workflow/r_output_all_treatment_status_metadata.xlsx")
