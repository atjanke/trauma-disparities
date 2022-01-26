library(data.table)
library(tidyverse)
library(tidytable)

neds19 <- readRDS("data-cleaned/neds19-subsample.rds")

df <- slice(neds19,1:100000)

# Identifies visits with a UDS
list_of_uds_visits <- df %>%
  mutate(row_name = row_number()) %>%
  select(row_name, CPT1:CPT15) %>%
  pivot_longer(CPT1:CPT15) %>%
  filter(value=="80305" | value=="80306" | value=="80307") %>%
  select(row_name) %>%
  unique() %>%
  mutate(UDS=1)

df <- df %>%
  mutate(row_name = row_number()) %>%
  left_join(list_of_uds_visits,by="row_name") %>%
  select(-row_name)

df$UDS[is.na(df$UDS)] <- 0

# Identifies visits with a Troponin performed

# Limit to ED visits not resulting in admission
df <- df %>% filter(DISP_ED != 9)

# Create an identifier for X,Y,Z clinical presentations
