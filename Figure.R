library(ggplot2)
library(tidyr)
library(dplyr)
library(modelr)

source("Functions.R")

df <- readRDS("data-cleaned/df.rds")

pain_scale <- df %>%
  mutate(Meds_Given  = ifelse(
    rowSums(across(Acetaminophen:Oxycontin))>0,1,0)) %>%
  mutate(RACE = as.character(RACE)) %>%
  mutate(RACE = case_when(
    RACE=="White" ~ RACE,
    RACE=="Black/African American" ~ RACE,
    T ~ "Other")) %>%
  group_by(PAINSCALE,RACE) %>%
  summarise(
    Count = n(),
    Meds_Given = 
              sum(ifelse(Meds_Given==1,1,0))/n()) %>%
  mutate(
    Lower = Lower_Bound(Meds_Given*Count,Count),
    Upper = Upper_Bound(Meds_Given*Count,Count))
  
ggplot(
  filter(
  pain_scale
  ,RACE!="Other")
  ,aes(x=PAINSCALE,y=Meds_Given,color=RACE))+
  geom_point(size=4)+
  geom_errorbar(aes(ymin=Lower, ymax=Upper),width=0.1)+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(breaks=c(3:10),limits=c(3,10))+
  ylab("Proportion of Patients Receiving Pain Medication")+
  xlab("Pain Scale (1-10)")+
  theme_bw()


age_ct <-
  df %>%
  group_by(AGE) %>%
  summarise(
    CT_Rate = sum(ifelse(CATSCAN=="CT imaging performed",1,0))/n())

age_ct %>%
  mutate(Discontinuity = as.factor(ifelse(AGE<18,1,0))) %>%
  ggplot(aes(x=AGE,y=CT_Rate,color=Discontinuity)) +
  geom_point()+
  geom_smooth(method="loess")