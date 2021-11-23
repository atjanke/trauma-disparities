library(data.table)
library(tidyverse)
library(tidytable)


neds19 <- readRDS("../../../Box/EMF-Work/Project-Folder/NEDS-visit-complexity/__data-cleaned/neds19-subsample.rds")


neds19 %>%
  slice(1:10000) %>%
  select(CPT1:CPT15) %>%
  pivot_longer(CPT1:CPT15) %>%
  filter(value=="80305"|value=="80306"|value=="80307")