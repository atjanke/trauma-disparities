source("Libraries.R")
source("Functions.R")

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

#### Figure 1, trend in UDS utilization                   ####

df %>%
  group_by(YEAR) %>%
  summarise(
    Tox.Unweighted = mean(TOXSCREN),
    Tox.Weighted = sum(TOXSCREN*PATWT)/sum(PATWT),
    Tox.Chest.Pain.Unweighted = sum(TOXSCREN*Chest_Pain)/sum(Chest_Pain),
    Tox.Chest.Pain.Weighted   = sum(TOXSCREN*Chest_Pain*PATWT)/sum(Chest_Pain*PATWT)) %>%
  select(YEAR,Tox.Weighted,Tox.Chest.Pain.Weighted) %>%
  pivot_longer(cols=Tox.Weighted:Tox.Chest.Pain.Weighted) %>%
  mutate(name = case_when(
    name=="Tox.Chest.Pain.Weighted" ~ "Chest Pain Visits",
    T ~ "All Visits")) %>%
  rename(UDS=name) %>%
  ggplot(aes(x=YEAR,y=value,color=UDS,group=UDS))+
  geom_point(alpha=0.5)+
  geom_line()+
  scale_y_continuous(labels=scales::percent_format(),limits=c(0,NA))+
  theme_bw()+
  xlab("")+ylab("")

#### Make tables for UDS rate by sex/race ####
table.chest.pain <- data.frame()
table.chest.pain[1,1] <- "White\nFemale"
table.chest.pain[3,1] <- "Black\nFemale"
table.chest.pain[2,1] <- "White\nMale"
table.chest.pain[4,1] <- "Black\nMale"

cluster_analysis_sample <- subset(cluster,Chest_Pain==1)

fmean <- function(race,sex) {
  cluster_function<-subset(cluster_analysis_sample,SEX==sex & RACE==race)
  return(svymean(~TOXSCREN,cluster_function,multicore=T)[1])
}
flow <- function(race,sex) {
  cluster_function<-subset(cluster_analysis_sample,SEX==sex & RACE==race)
  return(confint(svymean(~TOXSCREN, cluster_function))[1])
}
fhigh <- function(race,sex) {
  cluster_function<-subset(cluster_analysis_sample,SEX==sex & RACE==race)
  return(confint(svymean(~TOXSCREN, cluster_function))[2])
}


table.chest.pain[1,2] <- fmean("White","Female")
table.chest.pain[1,3] <- flow("White","Female")
table.chest.pain[1,4] <- fhigh("White","Female")

table.chest.pain[2,2] <- fmean("White","Male")
table.chest.pain[2,3] <- flow("White" ,"Male")
table.chest.pain[2,4] <- fhigh("White","Male")

table.chest.pain[3,2] <- fmean("Black/African American","Female")
table.chest.pain[3,3] <- flow("Black/African American" ,"Female")
table.chest.pain[3,4] <- fhigh("Black/African American","Female")

table.chest.pain[4,2] <- fmean("Black/African American","Male")
table.chest.pain[4,3] <- flow("Black/African American" ,"Male")
table.chest.pain[4,4] <- fhigh("Black/African American","Male")

table.chest.pain <- table.chest.pain %>%
  setNames(c("group","mean","low.CI","high.CI"))

table <- data.frame()
table[1,1] <- "White\nFemale"
table[3,1] <- "Black\nFemale"
table[2,1] <- "White\nMale"
table[4,1] <- "Black\nMale"

fmean <- function(race,sex) {
  cluster_function<-subset(cluster,SEX==sex & RACE==race)
  return(svymean(~TOXSCREN,cluster_function,multicore=T)[1])
}
flow <- function(race,sex) {
  cluster_function<-subset(cluster,SEX==sex & RACE==race)
  return(confint(svymean(~TOXSCREN, cluster_function))[1])
}
fhigh <- function(race,sex) {
  cluster_function<-subset(cluster,SEX==sex & RACE==race)
  return(confint(svymean(~TOXSCREN, cluster_function))[2])
}


table[1,2] <- fmean("White","Female")
table[1,3] <- flow("White","Female")
table[1,4] <- fhigh("White","Female")

table[2,2] <- fmean("White","Male")
table[2,3] <- flow("White" ,"Male")
table[2,4] <- fhigh("White","Male")

table[3,2] <- fmean("Black/African American","Female")
table[3,3] <- flow("Black/African American" ,"Female")
table[3,4] <- fhigh("Black/African American","Female")

table[4,2] <- fmean("Black/African American","Male")
table[4,3] <- flow("Black/African American" ,"Male")
table[4,4] <- fhigh("Black/African American","Male")

table <- table %>%
  setNames(c("group","mean","low.CI","high.CI"))




#### Bar plots for UDS rate by sex/race   ####

table %>%
  mutate(category = "All ED Visits") %>%
  rbind.all.columns(table.chest.pain) %>%
  mutate(category = ifelse(is.na(category),"Chest Pain Visits",category)) %>%
  ggplot(aes(x=category,y=mean,fill=group))+
  geom_bar(stat="identity",position=position_dodge(width=0.9))+
  geom_errorbar(aes(ymin=low.CI,ymax=high.CI,x=category),
                position=position_dodge(width=0.9),
                width=0.1,
                alpha=0.5)+
  geom_text(aes(label=group,y=0.003,hjust=0),position=position_dodge(width=0.9),
            check_overlap=TRUE,
            angle=90,
            fontface="bold")+
  scale_y_continuous(labels=scales::percent_format())+
  scale_fill_brewer()+
  theme_bw()+
  xlab("")+ylab("")+
  theme(legend.position="none")

