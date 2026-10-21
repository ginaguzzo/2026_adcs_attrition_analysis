## Import all sub-tables into R for Study Retention Analysis.
## Format combined table for analysis in R.

library(dplyr)
library(tidyr)
library(stringr)
library(readxl) 
library(openxlsx)

setwd("/2025_study_retention_analysis/Analysis/")


# Import data tables -------------------------------------------------------------

baseline <- read.csv("metadata/merging_baseline_and_followup/baseline_ids.csv",
                     row.names = NULL)
followup <- read.csv("metadata/merging_baseline_and_followup/followup_ids.csv",
                     row.names = NULL)

baseline$id <-  as.character(baseline$id)
followup$id <-  as.character(followup$id)


# Merge data frames -------------------------------------------------------------
b_f_merged <- dplyr::left_join(baseline, followup, by = "id")


# Format merged metadata --------------------------------------------------
b_f_merged <- dplyr::select(b_f_merged, -followup_names)
b_f_merged$followed_up <- replace_na(b_f_merged$followed_up, "no")



# Save metadata files ------------------------------------------------------
write.xlsx(b_f_merged, 
           file = "metadata/merging_baseline_and_followup/r_merged_output.xlsx")



# Merge other metadata tables ---------------------------------------------
attrition <- read_excel("metadata/participant_attrition_reasons.xlsx")
orgs <- read_excel("metadata/participant_orgs_and_referral_types.xlsx")
all_metadata <- read_excel("metadata/all_metadata.xlsx")
treatment_status <- read_excel("metadata/treatment_status_workflow/treatment_status_final.xlsx")
dentist_referral <- read_excel("metadata/indigenous_dentist_coding_full.xlsx")

## Income was merged to so that 'Other' responses could be modified.
## Other income/financial support was coded for those who were supported by a parent, partner, scholarship, insurance payments.
## Government payment included those who selected 'Centrelink payment' on the survey or specified they had a disability pension.
## Retired/retirement savings was specified for those who wrote they were retired and/or on superannuation (i.e., retirement savings) funds.

## Join treatment status to rest of metadata
all_metadata <- all_metadata %>%
  left_join(
    treatment_status %>%
      select(p_ID, referral_type, treatment_status_final),
    by = "p_ID"
  )

## Join attrition df to the rest of the metadata
all_metadata <- all_metadata %>%
  left_join(
    attrition %>%
      rename(p_ID = id) %>%
      select(p_ID, followed_up, reason_for_attrition, reason_for_attrition_subgroup, additional_notes),
    by = "p_ID"
  )

## Join info on organisations and referral df to rest of the metadata
all_metadata <- all_metadata %>%
  left_join(
    orgs %>%
      rename(p_ID = id) %>%
      select(p_ID, adult_child, community_organisation),
    by = "p_ID"
  )


## Join info on Indigenous dentist referrals to the metadata
all_metadata <- all_metadata %>%
  left_join(
    dentist_referral %>%
      select(p_ID, indigenous_dentist, indigenous_dentist_duration),
    by = "p_ID"
  )



# Code study appointment location ---------------------------------------
loc_b <- read_excel("metadata/microbiome_mastersheet_baseline.xlsx")
loc_f <- read_excel("metadata/microbiome_mastersheet_followup.xlsx")


## Keep just one row per participant ID
loc_b <- loc_b %>%
  rename(p_ID = ID, 
         location_baseline = LOCATION,
         date_seen_baseline = `COLLECTION DATE`) %>%
  group_by(p_ID) %>%
  slice(1) %>%      # keeps the first row per participant
  ungroup()

loc_f <- loc_f %>%
  rename(p_ID = ID, 
         location_followup = LOCATION,
         date_seen_followup = `COLLECTION DATE`) %>%
  group_by(p_ID) %>%
  slice(1) %>%      # keeps the first row per participant
  ungroup()

## Merge location info with the rest of metadata
all_metadata <- all_metadata %>%
  left_join(
    loc_b %>%
      select(p_ID, location_baseline, date_seen_baseline),
    by = "p_ID"
  )

all_metadata <- all_metadata %>%
  left_join(
    loc_f %>%
      select(p_ID, location_followup, date_seen_followup),
    by = "p_ID"
  )



# Save all metadata -------------------------------------------------------
write.xlsx(all_metadata, 
           file = "metadata/r_all_metadata_output.xlsx")



