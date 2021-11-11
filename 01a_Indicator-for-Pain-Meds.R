#### Make a blank dataframe to populate medications given ED ####

# First, we need to remove medications that were 
# *prescribed* during the ED visit, rather than 
# given during the encounter.
table <- data.frame(N = 1:nrow(df))

for (i in 1:30) {
  build <- df %>% select(MED1:GPMED30)
  build <- build[,c(i,30+i)]
  colnames(build) <- c("MED","GPMED")
  build[build$GPMED!=1,]$MED <- -10
  build <- select(build,MED)
  table <- cbind(table,build)
  rm(build)
}
colnames(table) <- c("N",paste0("Med_",rep(1:30)))
# Remove MED1:GPMED30, these columns are no longer needed
df <- df %>%
  cbind(table) %>%
  select(-(MED1:GPMED30)) %>% select(-N)
rm(table)



#### Identify individual medications ####

long <- df %>%
  mutate(N = row_number()) %>%
  select(N, Med_1:Med_30) %>%
  pivot_longer(Med_1:Med_30,values_to="Med") %>%
  filter(Med>=0) %>%
  mutate(Med=as.character(Med)) %>%
  mutate(
    Acetaminophen = ifelse(Med==260,1,0),
    Advil                  = ifelse(Med==597,1,0),
    Tylenol_Codeine        = ifelse(Med==280 | Med==32920,1,0),   
    Tylenol_Oxycodone      = ifelse(Med==283,1,0),
    Tylenol_No_2           = ifelse(Med==32915,1,0),
    Tylenol_No_3           = ifelse(Med==32920,1,0),
    Tylenol_No_4           = ifelse(Med==32930,1,0),
    Tylenol_Extra_Strength = ifelse(Med==2036,1,0),
    Tylenol_Infant         = ifelse(Med==4541,1,0),
    Tylenol_Childrens      = ifelse(Med==6374,1,0),
    Mobic                  = ifelse(Med==00048,1,0),
    Celecoxib              = ifelse(Med==00208 ,1,0),  
    Hydromorphone          = ifelse(Med==15005,1,0),
    Ketorolac              = ifelse(Med==00169 | Med==93220 ,1,0),  
    Tylenol       = ifelse(Med==32905 | Med==99028,1,0),
    Naprosyn      = ifelse(Med==20285,1,0),
    Naproxyn      = ifelse(Med==20290,1,0),
    Indomethacin  = ifelse(Med==15600,1,0),
    Ibuprofen     = ifelse(Med==15395,1,0),
    Motrin        = ifelse(Med==19675,1,0),
    Toradol       = ifelse(Med==92161,1,0),
    Fentanyl      = ifelse(Med==94188,1,0),
    Dilaudid      = ifelse(Med==9600 ,1,0),
    Morphine      = ifelse(Med==19650 | Med==99123,1,0),
    Oxycodone     = ifelse(Med==12028,1,0),
    Oxycontin     = ifelse(Med==96109,1,0)
  ) %>%
  group_by(N) %>%
  summarise(
    Acetaminophen         = ifelse(sum(Acetaminophen)         >=1,1,0),
    Advil                 = ifelse(sum(Advil)                 >=1,1,0),
    Tylenol_Codeine       = ifelse(sum(Tylenol_Codeine)       >=1,1,0),
    Tylenol_Oxycodone     = ifelse(sum(Tylenol_Oxycodone)     >=1,1,0),
    Tylenol_No_2          = ifelse(sum(Tylenol_No_2)          >=1,1,0),
    Tylenol_No_3          = ifelse(sum(Tylenol_No_3)          >=1,1,0),
    Tylenol_No_4          = ifelse(sum(Tylenol_No_4)          >=1,1,0),
    Tylenol_Extra_Strength= ifelse(sum(Tylenol_Extra_Strength)>=1,1,0),
    Tylenol_Infant        = ifelse(sum(Tylenol_Infant)        >=1,1,0),
    Tylenol_Childrens     = ifelse(sum(Tylenol_Childrens)     >=1,1,0),
    Mobic                 = ifelse(sum(Mobic)                 >=1,1,0),
    Celecoxib             = ifelse(sum(Celecoxib)             >=1,1,0),
    Hydromorphone         = ifelse(sum(Hydromorphone)         >=1,1,0),
    Ketorolac             = ifelse(sum(Ketorolac)             >=1,1,0),
    Tylenol               = ifelse(sum(Tylenol)               >=1,1,0),
    Naprosyn              = ifelse(sum(Naprosyn)              >=1,1,0),
    Naproxyn              = ifelse(sum(Naproxyn)              >=1,1,0),
    Indomethacin          = ifelse(sum(Indomethacin)          >=1,1,0),
    Ibuprofen             = ifelse(sum(Ibuprofen)             >=1,1,0),
    Motrin                = ifelse(sum(Motrin)                >=1,1,0),
    Toradol               = ifelse(sum(Toradol)               >=1,1,0),
    Fentanyl              = ifelse(sum(Fentanyl)              >=1,1,0),
    Dilaudid              = ifelse(sum(Dilaudid)              >=1,1,0),
    Morphine              = ifelse(sum(Morphine)              >=1,1,0),
    Oxycodone             = ifelse(sum(Oxycodone)             >=1,1,0),
    Oxycontin             = ifelse(sum(Oxycontin)             >=1,1,0))


df <- df %>%
  mutate(N = row_number()) %>%
  left_join(long,by="N") %>%
  select(-c(Med_1:Med_30))

rm(long)

df[is.na(df)] <- 0

          