# create graph y-axis labels that reflect the measure/axis being used

y_label_create <- function (x, unit = "bw") {

  if (grepl("^[a-zA-Z]+RFD$", x)) {
    yAxisLabel <- expression('Force (N·s' ^ -1 ~ ')')
  }

  else if (grepl("^[a-zA-Z]+Peak$", x)) {
    switch(unit,
           "n" = {yAxisLabel <- "Force (N)"},
           "bw" = {yAxisLabel <- "Force (BW)"}
    )
  }

  else if (grepl("^[a-zA-Z]+Angle$", x)) {
    yAxisLabel <- "Angle (°)"
  }

  else (yAxisLabel = "")

  yAxisLabel

}
