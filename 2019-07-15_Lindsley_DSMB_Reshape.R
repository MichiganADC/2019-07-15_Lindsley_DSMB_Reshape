# 2019-07-15_Lindsley_DSMB_Reshape.R

library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(lubridate)

source(
  paste("~",
        "Box Sync",
        "Documents",
        "I-CONECT",
        "Documents-USB",
        "ICONECT_Participant_Tracker",
        "get_data_helpers.R", sep = "/")
)
source("~/Box Sync/Documents/R_helpers/helpers.R")

df_raw <-
  read_excel("./Visit Date Chartv2.xlsx")


# Reshape data

df_raw_long <-
  df_raw %>% 
  gather(key = "DateType", value = "Date",
         -`Subject ID`,
         -`Event Name`,
         -`Repeat Instrument`,
         -`Repeat Instance`,
         -`Current Participant Status`,
         na.rm = TRUE) %>% 
  mutate(Date = as.Date(Date))

df_raw_long_unite <-
  df_raw_long %>% 
  unite(col = EventName_RepeatInstrument_RepeatInstance_DateType, 
        `Event Name`, `Repeat Instrument`, `Repeat Instance`, DateType) %>% 
  mutate(EventName_RepeatInstrument_RepeatInstance_DateType = case_when(
    str_detect(EventName_RepeatInstrument_RepeatInstance_DateType, "_NA") ~ 
      str_remove_all(EventName_RepeatInstrument_RepeatInstance_DateType, "_NA"),
    TRUE ~ EventName_RepeatInstrument_RepeatInstance_DateType
  ))

df_raw_wide <-
  df_raw_long_unite %>% 
  select(-`Current Participant Status`) %>% 
  spread(key = EventName_RepeatInstrument_RepeatInstance_DateType,
         value = Date)

df_raw_wide_ptstt <-
  df_raw_long_unite %>% 
  filter(!is.na(`Current Participant Status`)) %>% 
  select(-EventName_RepeatInstrument_RepeatInstance_DateType, 
         -Date)

df_raw_wide_ptstt <-
  left_join(df_raw_wide, df_raw_wide_ptstt, by = c("Subject ID")) %>% 
  select(`Subject ID`,
         `Current Participant Status`,
         everything())

readr::write_csv(df_raw_wide_ptstt, "df_raw_wide_ptstt.csv", na = "")


# Calculate date intervals

df_raw_wide_ptstt_drvd <-
  df_raw_wide_ptstt %>% 
  mutate(
    Screen_through_TechInstall = 
      case_when(
        !is.na(`Scrn V_Pre-Visit Stability Screening_1_Date Administered`) &
          !is.na(`Tech Install_Date of Installation`) ~
          time_length(
            interval(
              start = `Scrn V_Pre-Visit Stability Screening_1_Date Administered`,
              end = `Tech Install_Date of Installation`),
            unit = "week"
          ),
        TRUE ~ NA_real_
      )
  ) %>% 
  mutate(
    Screening_Time_Point = 
      case_when(
        !is.na(`Scrn V_Pre-Visit Stability Screening_3_Date Administered`) ~
          time_length(
            interval(
              start = `Scrn V_Pre-Visit Stability Screening_1_Date Administered`,
              end = `Scrn V_Pre-Visit Stability Screening_3_Date Administered`
            ), 
            unit = "week"
          ),
        !is.na(`Scrn V_Pre-Visit Stability Screening_2_Date Administered`) ~
          time_length(
            interval(
              start = `Scrn V_Pre-Visit Stability Screening_1_Date Administered`,
              end = `Scrn V_Pre-Visit Stability Screening_2_Date Administered`
            ), 
            unit = "week"
          ),
        !is.na(`Scrn V_Pre-Visit Stability Screening_1_Date Administered`) ~
          0.0,
        TRUE ~ NA_real_
      )
  ) %>% 
  mutate(
    Clinician_Diagnosis =
      case_when(
        !is.na(`Scrn V_Pre-Visit Stability Screening_3_Date Administered`) &
          !is.na(`BL CDx_Clinician Diagnosis (NACC D1)_1_Date of Clinician Diagnosis`) ~
          time_length(
            interval(
              start = `Scrn V_Pre-Visit Stability Screening_3_Date Administered`,
              end = `BL CDx_Clinician Diagnosis (NACC D1)_1_Date of Clinician Diagnosis`
            ), 
            unit = "week"
          ),
        !is.na(`Scrn V_Pre-Visit Stability Screening_2_Date Administered`) &
          !is.na(`BL CDx_Clinician Diagnosis (NACC D1)_1_Date of Clinician Diagnosis`) ~
          time_length(
            interval(
              start = `Scrn V_Pre-Visit Stability Screening_2_Date Administered`,
              end = `BL CDx_Clinician Diagnosis (NACC D1)_1_Date of Clinician Diagnosis`
            ),
            unit = "week"
          ),
        !is.na(`Scrn V_Pre-Visit Stability Screening_1_Date Administered`) &
          !is.na(`BL CDx_Clinician Diagnosis (NACC D1)_1_Date of Clinician Diagnosis`) ~
          time_length(
            interval(
              start = `Scrn V_Pre-Visit Stability Screening_1_Date Administered`,
              end = `BL CDx_Clinician Diagnosis (NACC D1)_1_Date of Clinician Diagnosis`
            ),
            unit = "week"
          ),
        TRUE ~ NA_real_
      )
  ) %>% 
  mutate(
    Baseline_Visit =
      case_when(
        !is.na(`BL CDx_Clinician Diagnosis (NACC D1)_1_Date of Clinician Diagnosis`) &
          !is.na(`BL V_Pre-Visit Stability Screening_1_Date Administered`) ~
          time_length(
            interval(
              start = `BL CDx_Clinician Diagnosis (NACC D1)_1_Date of Clinician Diagnosis`,
              end = `BL V_Pre-Visit Stability Screening_1_Date Administered`
            ),
            unit = "week"
          ),
        TRUE ~ NA_real_
      )
  ) %>% 
  rowwise() %>% 
  mutate(
    blv_mri_max = 
      max(
        `BL V_Pre-Visit Stability Screening_1_Date Administered`,
        `BL V_Pre-Visit Stability Screening_2_Date Administered`,
        `BL V_Pre-Visit Stability Screening_3_Date Administered`,
        `BL MRI_Scan Date`,
        na.rm = TRUE
      )
  ) %>% 
  ungroup() %>% 
  mutate(
    Intervention_Period =
      case_when(
        !is.na(blv_mri_max) &
          !is.na(`Admin_Activation Date`) ~
          time_length(
            interval(
              start = blv_mri_max,
              end = `Admin_Activation Date`
            ),
            unit = "week"
          ),
        TRUE ~ NA_real_
      )
  ) %>% 
  mutate(
    Month_06_Time_Point =
      case_when(
        !is.na(`06 V_Pre-Visit Stability Screening_1_Date Administered`) &
          !is.na(`06 V_Pre-Visit Stability Screening_2_Date Administered`) ~
          time_length(
            interval(
              start = `06 V_Pre-Visit Stability Screening_1_Date Administered`,
              end = `06 V_Pre-Visit Stability Screening_2_Date Administered`
            ),
            unit = "week"
          ),
        TRUE ~ NA_real_
      )
  )

readr::write_csv(df_raw_wide_ptstt_drvd, "df_raw_wide_ptstt_drvd.csv", na = "")


