df <- df %>%
  mutate(Pain_Medications_Given = factor(case_when(
    (MED1 == 12033 & GPMED1==1) |
    (MED2 == 12033 & GPMED2==1) |
    (MED3 == 12033 & GPMED3==1) |
    (MED4 == 12033 & GPMED4==1) |
    (MED5 == 12033 & GPMED5==1) |
    (MED6 == 12033 & GPMED6==1) |
    (MED7 == 12033 & GPMED7==1) |
    (MED8 == 12033 & GPMED8==1) |
    (MED9 == 12033 & GPMED9==1) |
    (MED10 == 12033 & GPMED10==1) |
    (MED11 == 12033 & GPMED11==1) |
    (MED12 == 12033 & GPMED12==1) |
    (MED13 == 12033 & GPMED13==1) |
    (MED14 == 12033 & GPMED14==1) |
    (MED15 == 12033 & GPMED15==1) |
    (MED16 == 12033 & GPMED16==1) |
    (MED17 == 12033 & GPMED17==1) |
    (MED18 == 12033 & GPMED18==1) |
    (MED19 == 12033 & GPMED19==1) |
    (MED20 == 12033 & GPMED20==1) |
    (MED21 == 12033 & GPMED21==1) |
    (MED22 == 12033 & GPMED22==1) |
    (MED23 == 12033 & GPMED23==1) |
    (MED24 == 12033 & GPMED24==1) |
    (MED25 == 12033 & GPMED25==1) |
    (MED26 == 12033 & GPMED26==1) |
    (MED27 == 12033 & GPMED27==1) |
    (MED28 == 12033 & GPMED28==1) |
    (MED29 == 12033 & GPMED29==1) |
    (MED30 == 12033 & GPMED30==1)  ~ "Yes",
    T ~ "No"
  ))) %>%
    select(-(MED1:GPMED30))