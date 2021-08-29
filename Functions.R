Lower_Bound <- function(Meds_Given, Count) {
  p = Meds_Given/Count
  Lower_Bound = p - 1.96*(sqrt(p*(1-p)/Count))
  return(Lower_Bound)
}

Upper_Bound <- function(Meds_Given, Count) {
  p = Meds_Given/Count
  Upper_Bound = p + 1.96*(sqrt(p*(1-p)/Count))
  return(Upper_Bound)
}