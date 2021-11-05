cousineau_error_bars <- function(data, selection = "single", unit) {

  moreyCorrection <- sqrt((4*2)/(4*2-1))

  if (selection == "single") {

    dfSubAvg <- data %>%
      group_by(partID) %>%
      summarise(subAvg = mean({{ unit }})) %>%
      ungroup()

    grandAvg <- mean(dfSubAvg$subAvg)

    data %>%
      left_join(dfSubAvg) %>%
      mutate(normalisedValue = {{ unit }} - subAvg + grandAvg) %>%
      select(partID, movement, leg, normalisedValue) %>%
      group_by(movement, leg) %>%
      summarise(n = n(),
                result = mean(normalisedValue),
                normSD = sd(normalisedValue),
                normSE = sd(normalisedValue)/sqrt(n)*moreyCorrection,
                normCI = normSE * 1.96) %>%
      ungroup()
  }

  else if (selection == "all") {

    dfSubAvg <- data %>%
      group_by(partID) %>%
      summarise(subAvg = mean({{ unit }})) %>%
      ungroup()

    grandAvg <- mean(dfSubAvg$subAvg)

    data %>%
      left_join(dfSubAvg) %>%
      mutate(normalisedValue = {{ unit }} - subAvg + grandAvg) %>%
      select(partID, movement, leg, measure, normalisedValue) %>%
      group_by(movement, leg, measure) %>%
      summarise(n = n(),
                result = mean(normalisedValue),
                normSD = sd(normalisedValue),
                normSE = sd(normalisedValue)/sqrt(n)*moreyCorrection,
                normCI = normSE * 1.96) %>%
      ungroup()

  }

  else {stop("Type unknown")}

}
