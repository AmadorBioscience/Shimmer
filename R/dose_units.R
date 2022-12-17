## extract the dose (mass) units from the current concentration units
dose_units <- function(C_units) {
  ## set dose units
  if (C_units == "ug/L") {
    mass_units <- "ug"
  }
  if (C_units == "mg/L") {
    mass_units <- "mg"
  }
  if (C_units == "nM") {
    mass_units <- "nmol"
  }
  return(mass_units)
}