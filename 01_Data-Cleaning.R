library(haven)
library(tidyr)
library(dplyr)
library(stringr)

df <- read_dta("data-raw/ED2018-stata.dta") %>%
  select(
    VMONTH,VDAYR,ARRTIME,ARREMS,WAITTIME,AGE,SEX,RACEUN,ETHUN,PAYTYPER,PAINSCALE,INJURY,INJURY72,INJURY_ENC,
    CATSCAN,CTAB,CTCHEST,CTHEAD,CTOTHER,CTUNK,ADMIT,MED,MED1:MED30,GPMED1:GPMED30
  ) %>% data.frame() 

df <- read_dta("data-raw/ED2017-stata.dta") %>%
  select(VMONTH,VDAYR,ARRTIME,ARREMS,WAITTIME,AGE,SEX,RACEUN,ETHUN,
         PAYTYPER,PAINSCALE,INJURY,INJURY72,INJURY_ENC,CATSCAN,CTAB,
         CTCHEST,CTHEAD,CTOTHER,CTUNK,ADMIT,MED,MED1:MED30,GPMED1:GPMED30 
  ) %>% data.frame() %>% zap_labels() %>%
  rbind(df)

df <- read_dta("data-raw/ED2016-stata.dta") %>%
  select(
    VMONTH, #Visit month (1-12)
    VDAYR, #Day of week of visit (1-7)
    ARRTIME, #Arrival time (military time)
    ARREMS, #Arrival by EMS
    WAITTIME, #Wait time to first provider contact, in minutes
    AGE, #Patient age in years
    SEX, #Patient sex
    RACEUN, #Race, not imputed 
    ETHUN, #Ethnicity, not imputed
    PAYTYPER, #Recoded primary payer
    PAINSCALE, #Pain scale, 0-10
    INJURY, #Is this visit related to injury/trauma/overdose poisoning, or adverse event of medical/surgical treatment?
    INJURY72, #Did the injury/trauma, overdose/poisoning, or adverse effect occur within 72 hours?
    INJURY_ENC, #Type of encounter for injury visits
    CATSCAN, #CT scan (any)
    CTAB, #CT scan - abdomen/pelvis
    CTCHEST, #CT scan - chest
    CTHEAD, #CT scan - head
    CTOTHER, # CT scan - other
    CTUNK, # CT scan - unknown location
    ADMIT, #Admission (and where?)
    #LOS, #Length of stay in hospital (days)
    MED, # Were medications or immunizations given at this visit or prescribed at ED discharge
    MED1:MED30, #Medications administered
    GPMED1:GPMED30 #Flag for medication in ED or prescription at discharge
  ) %>% data.frame() %>% zap_labels() %>% rbind(df)


df <- read_dta("data-raw/ED2015-stata.dta") %>%
  select(VMONTH,VDAYR,ARRTIME,ARREMS,WAITTIME,AGE,SEX,RACEUN,ETHUN,
         PAYTYPER,PAINSCALE,INJURY,INJURY72,INJURY_ENC,CATSCAN,CTAB,
         CTCHEST,CTHEAD,CTOTHER,CTUNK,ADMIT,MED,MED1:MED30,GPMED1:GPMED30 
  ) %>% data.frame() %>% zap_labels() %>%
  rbind(df)

# Recode variables
df <- df %>%
  mutate(WAITTIME = as.integer(WAITTIME)) %>%
  mutate(WAITTIME = as.integer(case_when(
    WAITTIME<0 ~ NA_integer_,
    WAITTIME>=0 ~ WAITTIME))) %>%
#  mutate(LOV = as.integer(LOV)) %>%
#   mutate(LOV = as.integer(case_when(
#     LOV<0 ~ NA_integer_,
#     LOV>=0 ~ LOV))) %>%
  mutate(ARREMS = factor(case_when(ARREMS==1 ~ "Yes",ARREMS==2 ~ "No", T ~ "Unknown"))) %>%
  mutate(SEX = factor(case_when(SEX==1 ~ "Female",SEX==0 ~ "Male", T ~ "Unknown"))) %>%
  mutate(RACE = factor(case_when(
    RACEUN==1 ~ "White",
    RACEUN==2 ~ "Black/African American",
    RACEUN==3 ~ "Asian",
    RACEUN==4 ~ "Native Hawaiian/Other Pacific Islander",
    RACEUN==5 ~ "American Indian/Alaska Nastive",
    RACEUN==6 ~ "More than one race reported",
    T ~ "Unknown"))) %>%
  mutate(ETHNICITY = factor(case_when(ETHUN==1 ~ "Hispanic or Latino",ETHUN==2 ~ "Not Hispanic or Latino",T ~ "Unknown"))) %>%
  mutate(PAYER = factor(case_when(
    PAYTYPER==1 ~ "Private insurance",
    PAYTYPER==2 ~ "Medicare",
    PAYTYPER==3 ~ "Medicaid or CHIP or other state-based",
    PAYTYPER==4 ~ "Worker's compensation",
    PAYTYPER==5 ~ "Self-pay",
    PAYTYPER==6 ~ "No charge/Charity",
    PAYTYPER==7 ~ "Other",
    T ~ "Unknown"))) %>%
  mutate(INJURY = factor(case_when(
    INJURY==0 ~ "Not injury-related",
    INJURY==1 ~ "Injury-related",
    T ~ "Unknown"))) %>%
  mutate(INJURY72 = factor(case_when(
    INJURY72==1 ~ "Yes, injury within 72 hours of visit",
    INJURY72==2 ~ "No, not within 72 hours of visit",
    T ~ "Unknown/not applicable"))) %>%
  mutate(INJURY_ENC = factor(case_when(
    INJURY_ENC==1 ~ "Initial encounter",
    INJURY_ENC>1 ~ "Subsequent encounter codes present",
    T ~ "Unknown/not applicable"))) %>%
  mutate(PAINSCALE = as.integer(PAINSCALE)) %>%
  mutate(PAINSCALE = as.integer(case_when(
    PAINSCALE<0 ~ NA_integer_,
    PAINSCALE>=0 ~ PAINSCALE))) %>%
  mutate(CATSCAN = factor(case_when(
    CATSCAN==0 ~ "No CT performed", CATSCAN==1 ~ "CT imaging performed"))) %>%
  mutate(ADMIT = factor(case_when(
    ADMIT==1 ~ "Critical care unit",ADMIT==2 ~ "Stepdown unit",ADMIT==3 ~ "Operating room",
    ADMIT==4 ~ "Mental health or detox unit",ADMIT==5 ~ "Cardiac catheterization lab",ADMIT==6 ~ "Other unit/bed",
    T ~ "Discharge/Not applicable"))) %>%
  # mutate(LOS = as.integer(LOS)) %>%
  # mutate(LOS = as.integer(case_when(
  #   LOS<0 ~ NA_integer_,
  #   LOS>=0 ~ LOS))) %>%
  select(-RACEUN,-ETHUN,-PAYTYPER) %>% zap_labels()

source("01a_Indicator-for-Pain-Meds.R")

saveRDS(df,"data-cleaned/df.rds")
