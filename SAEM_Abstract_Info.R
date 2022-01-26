source("Libraries.R")
source("Functions.R")

# Load all years of data
# source("01_Data-Cleaning.R")

# Identify all adults visits, 'chest pain' as reason for visit
df <- readRDS("data-cleaned/df.rds") %>% 
  mutate(Total=1) %>%
  mutate(Adult = ifelse(AGE>17,1,0)) %>%
  mutate(Adult.Chest_Pain = ifelse(Adult==1 & Chest_Pain==1,1,0)) %>%
  mutate(SEX = relevel(SEX,ref="Female"),
         RACE = relevel(RACE,ref="White"),
         AGE = cut(AGE,c(17,45,65,120)))

# Set up complex survey design
library(survey)
library(scales)
cluster <- svydesign(
  id=~CPSUM,
  strata=~CSTRATM,
  weights=~PATWT,
  nest=TRUE,
  data=df,
  multicore=T)

print("Number of (weighted) visits for adults: ")
df <- df %>% filter(is.na(AGE)==FALSE) 
print(svytotal(~Adult, cluster,na.rm=T,se=TRUE,multicore=T))

print("Number of visits for adults with chest pain (any reason for visit): ")
df <- df %>% filter(Chest_Pain==1)
print(svytotal(~Adult.Chest_Pain, cluster,na.rm=T,se=TRUE,multicore=T))

# Report urine drug screen utilization overall
cluster_analysis_sample<-subset(cluster,Adult.Chest_Pain==1)

print("Proportion of these patients who had a UDS: ")
print(
  paste0(
  percent(svymean(~TOXSCREN,cluster_analysis_sample,multicore=T)[1],accuracy=0.1),
  " [",
  percent(confint(svymean(~TOXSCREN, cluster_analysis_sample))[1],accuracy=0.1),
  " to ",
  percent(confint(svymean(~TOXSCREN, cluster_analysis_sample))[2],accuracy=0.1),"]")
  )

# Report urine drug screen utilization across race/ethnicity and sex
print_result <- function(race,sex) {
  
  cluster_function<-subset(cluster_analysis_sample,SEX==sex & RACE==race)
  
  print(
    paste0(
      percent(svymean(~TOXSCREN,cluster_function,multicore=T)[1],accuracy=0.1),
      " [",
      percent(confint(svymean(~TOXSCREN, cluster_function))[1],accuracy=0.1),
      " to ",
      percent(confint(svymean(~TOXSCREN, cluster_function))[2],accuracy=0.1),"]"))
  
  print(svytotal(~Total,cluster_function,multicore=T)[1])
}
print("White females: ")
print_result("White","Female")
print("White males: ")
print_result("White","Male")
print("Black females: ")
print_result("Black/African American","Female")
print("Black males: ")
print_result("Black/African American","Male")

print("Unknown race, females: ")
print_result("Unknown","Female")
print("Unknown race, males: ")
print_result("Unknown","Male")



# Report odds ratios for binary logistic regression model for UDS
library(aod)

cluster_logit <- subset(cluster_analysis_sample,RACE %in% c("White","Black/African American"))

logit <- svyglm(TOXSCREN ~ SEX + RACE + AGE + YEAR, cluster_logit, family=quasibinomial)
summary(logit)

print(round(exp(cbind(OR = coef(logit), confint(logit))),digits=3))




#### 


