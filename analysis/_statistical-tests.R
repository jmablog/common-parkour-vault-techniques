# This file contains code for the other statistical tests run as part
# of the data analysis performed for this project, but that are not
# directly included as part of the main report text. Code is run
# with the same library and data loading step as setup in index.Rmd.

## ---- normality

# takes data from a summarise by group pipe and returns
# either the p-value or the W statistic as requested
multiShap <- function(data, return = "p") {

  y <- shapiro.test(data)

  if (return == "w") {
    y$statistic
  }

  else {y$p.value}
}

normalityTests <-
  pkvs %>%
  group_by(measure, movement, leg) %>%
  summarise(p = round(multi_shap(result_bw), 2)) %>%
  ungroup %>%
  spread(movement, p) %>%
  rename(Measure = measure,
         `Landing Style` = leg) %>%
  mutate(Measure = as.character(Measure)) %>%
  mutate(Measure = str_replace_all(Measure, "Vert", "Vertical"),
         Measure = str_replace_all(Measure, "Horz", "Horizontal"),
         Measure = str_replace_all(Measure, "Peak", " Peak")) %>%
  mutate(dropsig = case_when(Drop <= .05 ~ "*"),
         stepsig = case_when(Step <= .05 ~ "*"),
         dashsig = case_when(Dash <= .05 ~ "*"),
         kongsig = case_when(Kong <= .05 ~ "*")) %>%
  replace_na(list(dropsig = "",
                  stepsig = "",
                  dashsig = "",
                  kongsig = "")) %>%
  unite(Drop, Drop, dropsig, sep = "") %>%
  unite(Step, Step, stepsig, sep = "") %>%
  unite(Dash, Dash, dashsig, sep = "") %>%
  unite(Kong, Kong, kongsig, sep = "")

normalityTests_formatted <- normalityTests
normalityTests_formatted[c(2, 4), 1] <- " "

normalityTests_formatted

## ---- standardised-force-curve-plots

# creates plots of the force curves for each movement,
# standardised to be from 0-100% of the movement  on the x-axis
# rather than absolute number of seconds

standardTime <- function(data,
                         weight,
                         landingStyle,
                         contactThreshold = NULL) {

  ifelse(is.null(contactThreshold),
         threshold <- (weight/100) * 2,
         threshold <- contactThreshold)

  start_time <- data %>%
    slice(which.max(Fz > threshold):n()) %>%
    pluck("time", 1)

  peak_time <- data %>%
    filter(Fz == max(Fz)) %>%
    pluck("time", 1)

  switch(as.character(landingStyle),
         "Double" = {
           end_time <- data %>%
             filter(time > peak_time) %>%
             slice(0:which.max(Fz < (weight - threshold))) %>%
             pluck("time", nrow(.))
         },
         "Single" = {
           end_time <- data %>%
             filter(time > peak_time) %>%
             slice(0:which.max(Fz < threshold)) %>%
             pluck("time", nrow(.))
         })

  data_trimmed <- data %>%
    filter(between(time, start_time, end_time))

  data_newtime <- data_trimmed %>%
    mutate(time = seq(0, nrow(data_trimmed) - 1, by = 1),
           percent = round(time / max(time), 3)) %>%
    select(time, percent, everything())

  return(data_newtime)
  }

peaksPerMovement <- function(slice,
                             keys) {

  maxes <- slice %>% mutate(maxFz = max(Fz),
                            maxFy = max(Fy))

  maxes %>%
    select(weight,
           time,
           percent, Fy, Fz,
           rMag, rAng,
           maxFz, maxFy)
  }

standardForceCurves <- forceCurves %>%
  mutate(adjForceCurves = pmap(list(forceCurves, weight, leg),
                               ~standardTime(..1, ..2, ..3, 20))) %>%
  unnest(c(adjForceCurves)) %>%
  mutate(Fz = Fz/weight,
         Fy = Fy/weight) %>%
  group_by(partID, movement, leg, rep) %>%
  group_modify(peaksPerMovement) %>%
  ungroup() %>%
  select(-weight, -time, -rMag, -rAng) %>%
  gather(key = "axis",
         value = "result_bw",
         Fy, Fz) %>%
  mutate(axis = factor(axis,
                       levels = c("Fz", "Fy"),
                       labels = c("Vertical", "Horizontal")))

partIDs <- as.character(unique(standardForceCurves$partID))

forceCurvesPlots <- function(id) {
  standardForceCurves %>%
    mutate(leg = fct_recode(leg, Precision = "Double", Running = "Single")) %>%
    filter(partID == id) %>%
    ggplot(aes(percent)) +
    # geom_line(aes(y = Fz, linetype = rep), colour = "#515151") +
    # geom_line(aes(y = Fy, linetype = rep), colour = "#909090") +
    #geom_hline(yintercept = 1, linetype = "dashed", alpha = .2) +
    geom_line(aes(y = result_bw, colour = axis, linetype = rep)) +
    geom_hline(aes(yintercept = maxFz, linetype = rep, colour = axis), alpha = .2) +
    geom_hline(aes(yintercept = maxFy, linetype = rep, colour = axis), alpha = .2) +
    # geom_hline(yintercept = 0, alpha = .2) +
    scale_x_continuous(labels = scales::percent_format()) +
    facet_grid(leg~movement, scales = "free_y") +
    labs(x = "% of Movement",
         y = "Force (BW)",
         # title = glue("Force Curves for Participant {id}"),
         # subtitle = "Horizontal lines highlight peak values per rep, line colour denotes axes",
         linetype = "Rep:",
         colour = "Axis:") +
    theme(strip.text = element_text(size = 11, color = "black"),
          strip.text.x = element_text(hjust = -.02),
          axis.line = element_line(color = "black"),
          axis.ticks = element_line(color = "black"),
          axis.title = element_text(color = "black"),
          axis.text = element_text(color = "black"),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
          legend.title = element_text(color = "black"),
          legend.text = element_text(color = "black"))
}

standardForceCurvePlots <- map(partIDs, forceCurvesPlots)

## ---- resultant-forces-plot

# Although not used, this plot showing the direction and magnitude
# of the resultant forces for each movement was a useful exercise
# in plotting and understanding the movements executed

medianResultant <- function(slice, keys){

  slice %>%
    select(-weight, -time, -ends_with("_n")) %>%
    arrange(resultantForce_bw) %>%
    slice(2)

}

resForcesPlot <-
  resultantForces %>%
  group_by(partID, movement, leg) %>%
  group_modify(medianResultant) %>%
  ungroup %>%
  group_by(movement, leg) %>%
  summarise(resultantPeak = mean(resultantForce_bw),
            resultantAngle = mean(resultantAngle),
            vertical = mean(vertical_bw),
            horizontal = mean(horizontal_bw)) %>%
  ungroup %>%
  ggplot(aes(horizontal, vertical)) +
  geom_segment(aes(xend = 0, yend = 0), position = "jitter",
               arrow = arrow(ends = "first", length = unit(0.03, "npc"),
                             type = "closed")) +
  geom_text(aes(label = paste(round(resultantPeak, 2), " x BW", "\n ",
                              round(resultantAngle, 1), "°", sep = " ")),
            size = 3, hjust = -.2) +
  coord_fixed() +
  expand_limits(x = c(-1, 4), y = c(0, 5)) +
  facet_grid(leg~movement) +
  labs(x = "Horizontal Force (BW)",
       y = "Vertical Force (BW)") +
  theme(#axis.text.x = element_text(angle = 270, hjust = 0, vjust = .5),
    panel.border = element_rect(colour = "black", fill = "NA"),
    axis.line = element_line(colour = "NA"))

resForcesPlot
