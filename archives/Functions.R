Lower_Bound <- function(Items, Count) {
  p = Items/Count
  Lower_Bound = p - 1.96*(sqrt(p*(1-p)/Count))
  return(Lower_Bound)
}

Upper_Bound <- function(Items, Count) {
  p = Items/Count
  Upper_Bound = p + 1.96*(sqrt(p*(1-p)/Count))
  return(Upper_Bound)
}