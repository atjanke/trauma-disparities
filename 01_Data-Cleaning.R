#### Load libraries, define variable lists     ####

source("Libraries.R")

# "NODISP"      No answer to item
# "NOFU"        No follow-up planned
# "RETRNED"     Return to ED
# "RETREFFU"    Return/Refer to physician/clinic for FU
# "LWBS"        Left without being seen (LWBS)
# "LBTC"        Left before treatment complete (LBTC)
# "LEFTAMA"     Left AMA (against medical advice)
# "DOA"         DOA (dead on arrival)
# "DIEDED"      Died in ED
# "TRANNH"      Return/transfer to nursing home
# "TRANPSYC"    Transfer to psychiatric hospital
# "TRANOTH"     Transfer to non-psychiatric hospital
# "ADMITHOS"    Admit to this hospital
# "OBSHOS"      Admit to observation unit, then hospitalized
# "OBSDIS"      Admit to observation unit, then discharged
# "OTHDISP"     Other visit disposition
# "ADMIT"       Categories for admissions

list.dispo.variables.16.19 <- c("NODISP","NOFU","RETRNED","RETREFFU","LWBS","LBTC",
                          "LEFTAMA","DOA","DIEDED","TRANNH","TRANPSYC","TRANOTH",
                          "ADMITHOS","OBSHOS","OBSDIS","OTHDISP")

list.dispo.variables.12.15 <- c("NODISP","NOFU","RETRNED","RETREFFU","LEFTBTRI","LEFTATRI",
                             "LEFTAMA","DOA","DIEDED","TRANNH","TRANPSYC","TRANOTH",
                             "ADMITHOS","OBSHOS","OBSDIS","OTHDISP")

list.dispo.variables.11    <- c("NODISP","NOFU","RETPRN","RETREFFU","LEFTBTRI","LEFTATRI",
                                "LEFTAMA","DOA","DIEDED","TRANNH","TRANPSYC","TRANOTH",
                                "ADMITHOS","OBSHOS","OBSDIS","OTHDISP")

#### Individually load multiple data sets      ####
df <- read_dta("data-raw/ED2019-stata.dta") %>%
  select(
    HOSPCODE,CPSUM,CSTRATM,PATWT,RFV1:RFV5,VMONTH,VDAYR,ARRTIME,ARREMS,WAITTIME,AGE,SEX,RACEUN,ETHUN,PAYTYPER,PAINSCALE,INJURY,INJURY72,INJURY_ENC,
    CATSCAN,CTAB,CTCHEST,CTHEAD,CTOTHER,CTUNK,ADMIT,TOXSCREN,MED,MED1:MED30,GPMED1:GPMED30,all_of(list.dispo.variables.16.19)
  ) %>% data.frame() %>% zap_labels() %>%
  mutate(DISPOSITION = case_when(
    ADMITHOS==1 | OBSHOS==1 | OBSDIS==1   | TRANOTH==1  | TRANPSYC==1    ~ "Admission",
    NOFU==1     | RETRNED==1| RETREFFU==1 | LWBS==1     | LBTC==1 | LEFTAMA==1  | TRANNH==1  ~"Discharge",
    DOA==1      | DIEDED==1          ~"Died",
    T     ~"Unknown")) %>%
  select(-all_of(list.dispo.variables.16.19)) %>%
  mutate(YEAR=2019)


df <- read_dta("data-raw/ED2018-stata.dta") %>%
  select(
    HOSPCODE,CPSUM,CSTRATM,PATWT,RFV1:RFV5,VMONTH,VDAYR,ARRTIME,ARREMS,WAITTIME,AGE,SEX,RACEUN,ETHUN,PAYTYPER,PAINSCALE,INJURY,INJURY72,INJURY_ENC,
    CATSCAN,CTAB,CTCHEST,CTHEAD,CTOTHER,CTUNK,ADMIT,TOXSCREN,MED,MED1:MED30,GPMED1:GPMED30,all_of(list.dispo.variables.16.19)
  ) %>% data.frame() %>% zap_labels() %>%
  mutate(DISPOSITION = case_when(
    ADMITHOS==1 | OBSHOS==1 | OBSDIS==1   | TRANOTH==1  | TRANPSYC==1    ~ "Admission",
    NOFU==1     | RETRNED==1| RETREFFU==1 | LWBS==1     | LBTC==1 | LEFTAMA==1  | TRANNH==1  ~"Discharge",
    DOA==1      | DIEDED==1      ~"Died",
    T     ~"Unknown")) %>%
  select(-all_of(list.dispo.variables.16.19)) %>%
  mutate(YEAR=2018) %>%
  rbind(df)

df <- read_dta("data-raw/ED2017-stata.dta") %>%
  select(HOSPCODE,CPSUM,CSTRATM,PATWT,RFV1:RFV5,VMONTH,VDAYR,ARRTIME,ARREMS,WAITTIME,AGE,SEX,RACEUN,ETHUN,
         PAYTYPER,PAINSCALE,INJURY,INJURY72,INJURY_ENC,CATSCAN,CTAB,
         CTCHEST,CTHEAD,CTOTHER,CTUNK,ADMIT,TOXSCREN,MED,MED1:MED30,GPMED1:GPMED30,all_of(list.dispo.variables.16.19)
  ) %>% data.frame() %>% zap_labels() %>%
  mutate(DISPOSITION = case_when(
    ADMITHOS==1 | OBSHOS==1 | OBSDIS==1   | TRANOTH==1  | TRANPSYC==1    ~ "Admission",
    NOFU==1     | RETRNED==1| RETREFFU==1 | LWBS==1     | LBTC==1 | LEFTAMA==1  | TRANNH==1  ~"Discharge",
    DOA==1      | DIEDED==1      ~"Died",
    T     ~"Unknown")) %>%
  select(-all_of(list.dispo.variables.16.19)) %>%
  mutate(YEAR=2017) %>%
  rbind(df)

df <- read_dta("data-raw/ED2016-stata.dta") %>%
  select(
    HOSPCODE,CPSUM,CSTRATM,PATWT,RFV1:RFV5,
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
    TOXSCREN, #Urine toxicology screen
    #LOS, #Length of stay in hospital (days)
    MED, # Were medications or immunizations given at this visit or prescribed at ED discharge
    MED1:MED30, #Medications administered
    GPMED1:GPMED30, #Flag for medication in ED or prescription at discharge
    all_of(list.dispo.variables.16.19)
  ) %>% data.frame() %>%
  mutate(DISPOSITION = case_when(
    ADMITHOS==1 | OBSHOS==1 | OBSDIS==1   | TRANOTH==1  | TRANPSYC==1    ~ "Admission",
    NOFU==1     | RETRNED==1| RETREFFU==1 | LWBS==1     | LBTC==1 | LEFTAMA==1  | TRANNH==1  ~"Discharge",
    DOA==1      | DIEDED==1      ~"Died",
    T     ~"Unknown")) %>%
  select(-all_of(list.dispo.variables.16.19)) %>%
  mutate(YEAR=2016) %>%
  zap_labels() %>% rbind(df)

df <- read_dta("data-raw/ED2015-stata.dta") %>%
  # We need to manually rename INJR1 --> INJURY_ENC
  rename(INJURY_ENC=INJR1) %>%
  select(HOSPCODE,CPSUM,CSTRATM,PATWT,RFV1:RFV5,VMONTH,VDAYR,ARRTIME,ARREMS,WAITTIME,AGE,SEX,RACEUN,ETHUN,
         PAYTYPER,PAINSCALE,INJURY,INJURY72,INJURY_ENC,CATSCAN,CTAB,
         CTCHEST,CTHEAD,CTOTHER,CTUNK,ADMIT,TOXSCREN,MED,MED1:MED30,GPMED1:GPMED30,
         all_of(list.dispo.variables.12.15)
  ) %>% data.frame() %>% zap_labels() %>%
  mutate(DISPOSITION = case_when(
    ADMITHOS==1 | OBSHOS==1 | OBSDIS==1   | TRANOTH==1  | TRANPSYC==1    ~ "Admission",
    NOFU==1     | RETRNED==1| RETREFFU==1 | LEFTAMA==1  | TRANNH==1   | LEFTBTRI==1 | LEFTATRI==1 ~"Discharge",
    DOA==1      | DIEDED==1      ~"Died",
    T     ~"Unknown")) %>%
  select(-all_of(list.dispo.variables.12.15)) %>%
  mutate(YEAR=2015) %>%
  rbind(df)

df <- read_dta("data-raw/ED2014-stata.dta") %>%
  # We need to manually rename INJR1 --> INJURY_ENC
  rename(INJURY_ENC=INJR1) %>%
  select(HOSPCODE,CPSUM,CSTRATM,PATWT,RFV1:RFV5,VMONTH,VDAYR,ARRTIME,ARREMS,WAITTIME,AGE,SEX,RACEUN,ETHUN,
         PAYTYPER,PAINSCALE,INJURY,INJURY72,INJURY_ENC,CATSCAN,CTAB,
         CTCHEST,CTHEAD,CTOTHER,CTUNK,ADMIT,TOXSCREN,MED,MED1:MED30,GPMED1:GPMED30,all_of(list.dispo.variables.12.15)
  ) %>% data.frame() %>% zap_labels() %>%
  mutate(DISPOSITION = case_when(
    ADMITHOS==1 | OBSHOS==1 | OBSDIS==1   | TRANOTH==1  | TRANPSYC==1    ~ "Admission",
    NOFU==1     | RETRNED==1| RETREFFU==1 | LEFTAMA==1  | TRANNH==1   | LEFTBTRI==1 | LEFTATRI==1 ~"Discharge",
    DOA==1      | DIEDED==1      ~"Died",
    T     ~"Unknown")) %>%
  select(-all_of(list.dispo.variables.12.15)) %>%
  mutate(YEAR=2014) %>%
  rbind(df)

df <- read_dta("data-raw/ED2013-stata.dta") %>%
  # We need to manually rename INJR1 --> INJURY_ENC
  rename(INJURY_ENC=INJR1) %>%
  select(HOSPCODE,CPSUM,CSTRATM,PATWT,RFV1:RFV3,VMONTH,VDAYR,ARRTIME,ARREMS,WAITTIME,AGE,SEX,RACEUN,ETHUN,
         PAYTYPER,PAINSCALE,INJURY,CATSCAN,CTAB,
         CTCHEST,CTHEAD,CTOTHER,CTUNK,ADMIT,TOXSCREN,MED,MED1:MED12,GPMED1:GPMED12,all_of(list.dispo.variables.12.15)
  ) %>% data.frame() %>% zap_labels() %>%
  mutate(YEAR=2013) %>%
  mutate(DISPOSITION = case_when(
    ADMITHOS==1 | OBSHOS==1 | OBSDIS==1   | TRANOTH==1  | TRANPSYC==1    ~ "Admission",
    NOFU==1     | RETRNED==1| RETREFFU==1 | LEFTAMA==1  | TRANNH==1   | LEFTBTRI==1 | LEFTATRI==1 ~"Discharge",
    DOA==1      | DIEDED==1      ~"Died",
    T     ~"Unknown")) %>%
  select(-all_of(list.dispo.variables.12.15)) %>%
  # INJURY72 and INJURY_ENC don't exist
  # MED and GPMED only go up to MED12 and GPMED12 starting in 2013
  # RFV only goes up to RFV3
  mutate(INJURY72="Unk") %>%
  mutate(INJURY_ENC="Unk") %>%
  rbind.all.columns(df)

df <- read_dta("data-raw/ED2012-stata.dta") %>%
  # We need to manually rename INJR1 --> INJURY_ENC
  rename(INJURY_ENC=INJR1) %>%
  select(HOSPCODE,CPSUM,CSTRATM,PATWT,RFV1:RFV3,VMONTH,VDAYR,ARRTIME,ARREMS,WAITTIME,AGE,SEX,RACEUN,ETHUN,
         PAYTYPER,PAINSCALE,INJURY,CATSCAN,CTAB,
         CTCHEST,CTHEAD,CTOTHER,CTUNK,ADMIT,TOXSCREN,MED,MED1:MED12,GPMED1:GPMED12,all_of(list.dispo.variables.12.15)
  ) %>% data.frame() %>% zap_labels() %>%
  mutate(YEAR=2012) %>%
  mutate(DISPOSITION = case_when(
    ADMITHOS==1 | OBSHOS==1 | OBSDIS==1   | TRANOTH==1  | TRANPSYC==1    ~ "Admission",
    NOFU==1     | RETRNED==1| RETREFFU==1 | LEFTAMA==1  | TRANNH==1   | LEFTBTRI==1 | LEFTATRI==1 ~"Discharge",
    DOA==1      | DIEDED==1      ~"Died",
    T     ~"Unknown")) %>%
  select(-all_of(list.dispo.variables.12.15)) %>%
  #INJURY72 and INJURY_ENC don't exist
  #MED and GPMED only go up to MED12 and GPMED12
  #RFV only goes up to RFV3
  mutate(INJURY72="Unk") %>%
  mutate(INJURY_ENC="Unk") %>%
  rbind.all.columns(df)

df <- read_dta("data-raw/ED2011-stata.dta") %>%
  # We need to manually rename INJR1 --> INJURY_ENC
  rename(INJURY_ENC=INJR1) %>%
  # CTNHEAD (not head) rename to CTOTHER
  rename(CTOTHER=CTNHEAD) %>%
  #CTAB and CTCHEST are incorporated into CTOTHER (all under CTNHEAD)
  select(HOSPCODE,CPSUM,CSTRATM,PATWT,RFV1:RFV3,VMONTH,VDAYR,ARRTIME,ARREMS,WAITTIME,AGE,SEX,RACEUN,ETHUN,
         PAYTYPER,PAINSCALE,INJURY,CATSCAN,
         CTHEAD,CTOTHER,CTUNK,ADMIT,TOXSCREN,MED,MED1:MED8,GPMED1:GPMED8,all_of(list.dispo.variables.11)
  ) %>% data.frame() %>% zap_labels() %>%
  mutate(YEAR=2011) %>%
  mutate(DISPOSITION = case_when(
    ADMITHOS==1 | OBSHOS==1   | OBSDIS==1   | TRANOTH==1  | TRANPSYC==1    ~ "Admission",
    NOFU==1     | RETREFFU==1 | LEFTAMA==1  | TRANNH==1   | LEFTBTRI==1 | LEFTATRI==1 ~"Discharge",
    DOA==1      | DIEDED==1      ~"Died",
    T     ~"Unknown")) %>%
  select(-all_of(list.dispo.variables.11)) %>%
  #INJURY72 and INJURY_ENC don't exist
  #MED and GPMED only go up to MED8 and GPMED8 starting in 2011
  #RFV only goes up to RFV3
  mutate(INJURY72="Unk") %>%
  mutate(INJURY_ENC="Unk") %>%
  mutate(CTAB="Unk") %>% 
  mutate(CTCHEST="Unk") %>%
  rbind.all.columns(df)

#### Recode variables                          ####

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
  mutate(SEX = factor(case_when(SEX==1 ~ "Female",SEX==2 ~ "Male", T ~ "Unknown"))) %>%
  mutate(RACE = factor(case_when(
    RACEUN==1 ~ "White",
    RACEUN==2 ~ "Black/African American",
    RACEUN==3 ~ "Asian",
    RACEUN==4 ~ "Native Hawaiian/Other Pacific Islander",
    RACEUN==5 ~ "American Indian/Alaska Native",
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


#### Convert NAs in Medication columns to 0    ####

# df <- df %>% mutate_at(vars(MED9:GPMED30),~replace(.,is.na(.),0))


#### Make indicator variables of interest      ####

# source("01a_Indicator-for-Pain-Meds.R")

# Build a chest pain chief complaint indicator
df <- df %>%
  mutate(Chest_Pain_1 = ifelse(
    (RFV1>=10500 & RFV1<=10503) | RFV1==12650 | RFV1==25150,1,0)) %>%
  mutate(Chest_Pain = case_when(
      (RFV1>=10500 & RFV1<=10503) | RFV1==12650 | RFV1==25150 |
      (RFV2>=10500 & RFV2<=10503) | RFV2==12650 | RFV2==25150 |
      (RFV3>=10500 & RFV3<=10503) | RFV3==12650 | RFV3==25150 |
      (RFV4>=10500 & RFV4<=10503) | RFV4==12650 | RFV4==25150 |
      (RFV5>=10500 & RFV5<=10503) | RFV5==12650 | RFV5==25150 
        ~ 1, T~0))

RFV.count <- df %>% select(RFV1,RFV2,RFV3,RFV4,RFV5) %>% 
  mutate(across(RFV1:RFV5,~ifelse(is.na(.),-9,.))) %>% 
  mutate(across(RFV1:RFV5,~ifelse(.==-9,0,1))) %>% 
  mutate(RFVs = RFV1+RFV2+RFV3+RFV4+RFV5) %>%
  select(-RFV1,-RFV2,-RFV3,-RFV4,-RFV5)

df <- cbind(df,RFV.count)

#### Save the data                             ####

saveRDS(df,"data-cleaned/df.rds")
