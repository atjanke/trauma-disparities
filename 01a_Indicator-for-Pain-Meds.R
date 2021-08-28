

for (i in 1:30) {
  VAR = paste0("MED",i)
  LABEL = paste0("GPMED",i)
  df[df$LABEL==1]$VAR <- 00000
}


build <- df %>%
  select(MED1:MED30) %>%
  pivot_longer(MED1:MED30) %>%
  mutate(N = rep(1:20291,times=1,each=30)) %>%
  mutate(Drug = case_when(

    value==15395 ~ "Ibuprofen",
    value==19675 ~ "Motrin",
    
    value==15600 ~ "Indomethacin",
    value==20285 ~ "Naprosyn",
    value==20290 ~ "Naproxen",
    
    value==00260 ~ "Acetaminophen",
    value==32905 ~ "Tylenol",
    
    value==12028 ~ "Oxycodone",
    value==96109 ~ "Oxycontin")) %>%
  filter(value!=-9)



          