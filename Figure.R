
library(ggplot2)
library(modelr)

age_ct %>%
  mutate(Discontinuity = as.factor(ifelse(AGE<18,1,0))) %>%
  ggplot(aes(x=AGE,y=CT_Rate,color=Discontinuity)) +
  geom_point()+
  geom_smooth(method="loess")