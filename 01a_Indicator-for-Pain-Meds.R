
# First, we need to remove medications that were 
# *prescribed* during the ED visit, rather than 
# given during the encounter.

table <- data.frame(N = 1:56467)

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

long <- df %>%
  mutate(N = row_number()) %>%
  select(N, Med_1:Med_30) %>%
  pivot_longer(Med_1:Med_30,values_to="Med") %>%
  filter(Med>=0) %>%
  mutate(Med=as.character(Med)) %>%
  mutate(
    Acetaminophen = ifelse(Med==260,1,0),
    Tylenol       = ifelse(Med==32905,1,0),
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
    Acetaminophen = ifelse(sum(Acetaminophen)>=1,1,0),
    Tylenol       = ifelse(sum(Tylenol)      >=1,1,0),
    Naprosyn      = ifelse(sum(Naprosyn)     >=1,1,0),
    Naproxyn      = ifelse(sum(Naproxyn)     >=1,1,0),
    Indomethacin  = ifelse(sum(Indomethacin) >=1,1,0),
    Ibuprofen     = ifelse(sum(Ibuprofen)    >=1,1,0),
    Motrin        = ifelse(sum(Motrin)       >=1,1,0),
    Toradol       = ifelse(sum(Toradol)      >=1,1,0),
    Fentanyl      = ifelse(sum(Fentanyl)     >=1,1,0),
    Dilaudid      = ifelse(sum(Dilaudid)     >=1,1,0),
    Morphine      = ifelse(sum(Morphine)     >=1,1,0),
    Oxycodone     = ifelse(sum(Oxycodone)    >=1,1,0),
    Oxycontin     = ifelse(sum(Oxycontin)    >=1,1,0))


df <- df %>%
  mutate(N = row_number()) %>%
  left_join(long,by="N") %>%
  select(-c(Med_1:Med_30))

rm(long)

df[is.na(df)] <- 0

          