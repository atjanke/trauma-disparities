library(ggplot2)
library(tidyr)
library(dplyr)
library(modelr)

source("Functions.R")

df <- readRDS("data-cleaned/combined.rds")

pain_scale <- df %>%
  filter(PAINSCALE > 6) %>%
  mutate(Meds_Given  = ifelse(
    rowSums(across(Acetaminophen:Oxycontin))>0,1,0)) %>%
  mutate(RACE = as.character(RACE)) %>%
  mutate(RACE = case_when(
    RACE=="White" ~ RACE,
    RACE=="Black/African American" ~ RACE,
    T ~ "Other")) %>%
  mutate(AGECATEGORY = cut(AGE, c(0,10,20,30,40,50,60,70,80,90,100))) %>%
  group_by(AGECATEGORY,RACE) %>%
  summarise(
    Count = n(),
    Meds_Given = 
              sum(ifelse(Meds_Given==1,1,0))/n()) %>%
  mutate(
    Lower = Lower_Bound(Meds_Given*Count,Count),
    Upper = Upper_Bound(Meds_Given*Count,Count))

  #cut(age, c(14,24,34,44,54,64,74,101),
   #   labels=c(“15–24”, “25–34”, “35–44”, “45–54”, “55–64”, “65–74”, “75+”))
  
  
ggplot(
  filter(
  pain_scale
  ,RACE!="Other")
  ,aes(x=AGECATEGORY,y=Meds_Given,color=RACE))+
  geom_point(size=4)+
  geom_errorbar(aes(ymin=Lower, ymax=Upper),width=0.1)+
  scale_y_continuous(labels=scales::percent)+
  #scale_x_continuous()+
  ylab("Proportion of Patients Receiving Pain Medication")+
  xlab("Age")+
  theme_bw()


age_ct <-
  df %>%
  group_by(AGE, VDAYR) %>%
  summarise(
    CT_Rate = sum(ifelse(CATSCAN=="CT imaging performed",1,0))/n())

age_ct %>%
  mutate(Discontinuity = as.factor(ifelse(AGE<16,1,0))) %>%
  ggplot(aes(x=AGE,y=CT_Rate,color=Discontinuity)) +
  geom_point()+
  geom_smooth(method="loess")+
  facet_wrap(~VDAYR)