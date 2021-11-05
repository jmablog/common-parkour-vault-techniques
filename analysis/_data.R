# This file contains code to load all raw data from the force platform
# in data/forcePlatformOutput and process it into the data formats used
# throughout the remaining report. This does not need to be run if just
# building the book, as index.Rmd will just load the .Rdata files already
# created, but can be used to create SPSS, CSV, or Excel versions of the
# data if required.

# Library Setup
library(tidyr)
library(dplyr)
library(readr)
library(purrr)
library(rlang)
library(here)
library(openxlsx)
library(haven)
library(beepr)

# create list of all .txt files in directory
filePathList <- list.files(path=here("data", "forcePlatformOutput"),
                           pattern = "\\.txt$", full.names = TRUE)

# create empty frame for raw data import to append to
rawForceCurves <- data.frame()

# loop over files and import each one into dataframe
for (f in filePathList) {

  import <- read_delim(file = f,
                       delim = "\t",
                       skip = 11,
                       col_names = TRUE) %>%
    mutate(Fy = as.numeric(Fy),
           Fz = as.numeric(Fz),
           rMag = sqrt(Fy^2 + Fz^2),
           rAng = (atan2(Fz, Fy) * 180 / pi)) %>%
    rename(time = `abs time (s)`) %>%
    filter(!is.na(Fz))

  tmp <- cbind(fileName = basename(f) %>% tools::file_path_sans_ext(),
               import)

  rawForceCurves <- rbind(rawForceCurves, tmp)

  rm(f, tmp, import)

}; beep(sound = 2)

# ===

# data cleaning & formatting

measures <- c("VP", "HP", "VR", "HR", "RM", "RA")

# import characteristics table
characteristics <- read.xlsx(here("data", "excel", "characteristics.xlsx")) %>%
  mutate(weight = round(weight, 0), weightKg = round(weight / 9.81, 1),
         boxRatio = round(96/height, 2)) %>%
  select(partID, age, gender, height, boxRatio,
         weight, weightKg, yearsTraining, preferredFoot) %>%
  mutate(partID = factor(partID),
         gender = factor(gender, levels = c("m", "f"),
                         labels = c("Male", "Female")),
         preferredFoot = factor(preferredFoot, levels = c("l", "r"),
                                labels = c("Left", "Right")))

# nest force curves into one row per file and remove participant name for anonymisation
forceCurves <- rawForceCurves %>%
  group_by(fileName) %>%
  nest(forceCurves = c(time, Fy, Fz, rMag, rAng))  %>%
  separate(fileName, into = c("partID", "moveLeg", "rep"), sep = "_") %>%
  separate(moveLeg, into = c("movement", "leg"), sep = 4) %>%
  mutate(partID = factor(partID),
         movement = factor(movement, levels = c("drop", "step", "dash", "kong"),
                  labels = c("Drop", "Step", "Dash", "Kong")),
         leg = factor(leg, levels = c("D", "S"),
             labels = c("Double", "Single")),
         rep = factor(rep)) %>%
  full_join((characteristics  %>% select(partID, weight)), by = "partID")

# import noise file
noise <- read_delim(file = here("data", "forcePlatformOutput", "noise", "noise.txt"),
                    delim = "\t",
                    skip = 11,
                    col_names = TRUE) %>%
  mutate(Fy = as.numeric(Fy),
         Fz = as.numeric(Fz)) %>%
  rename(time = `abs time (s)`) %>%
  filter(!is.na(Fz))

# find contact thresholds for each axes - mean + 3 standard deviations
FzNoiseThreshold <- mean(noise$Fz)+(3*sd(noise$Fz))
FyNoiseThreshold <- mean(noise$Fy)+(3*sd(noise$Fy))

# find peak force function
findPeak <- function(data, axis) {
  tmp <- data %>% filter(get(axis) == max(get(axis)))
  tmp[[1, axis]]
}

# find RFD function
findRFD <- function(data, axis, contactThreshold = 15) {
  peak <- data %>% filter(get(axis) == max(get(axis)))
  peakTime <- round(peak[[1, "time"]], 3)
  peakForce <- peak[[1, axis]]

  start <- data %>% slice(which.max(get(axis) > contactThreshold) : n())
  startTime <- round(start[[1, "time"]], 3)
  startForce <- start[[1, axis]]

  rfd <- (peakForce - startForce) / (peakTime - startTime)
  round(rfd, 0)
}

# find resultant force & angle function
findResultant <- function(data, axis, full = "no") {
  if (full == "yes"){
    tmp <- data %>% filter(get(axis) == max(get(axis)))
    tmp[1, ]
  }
  else {
    tmp <- data %>% filter(get(axis) == max(get(axis)))
    tmp[1, 4:5]
  }
}

# resultant forces
resultantForces <- forceCurves %>%
  mutate(resultant = map(forceCurves, ~findResultant(.x, "rMag", full = "yes"))) %>%
  select(-forceCurves) %>%
  unnest(resultant) %>%
  rename(resultantForce_n = rMag,
         resultantAngle = rAng,
         vertical_n = Fz,
         horizontal_n = Fy) %>%
  mutate(resultantForce_bw = resultantForce_n / weight,
         vertical_bw = vertical_n / weight,
         horizontal_bw = horizontal_n / weight)

# long format, all reps
pkvsFull <- forceCurves %>%
  mutate(VP = map(forceCurves, ~findPeak(.x, "Fz")),
         HP = map(forceCurves, ~findPeak(.x, "Fy")),
         VR = map(forceCurves, ~findRFD(.x, "Fz")),
         HR = map(forceCurves, ~findRFD(.x, "Fy")),
         resultant = map(forceCurves, ~findResultant(.x, "rMag", full = "no"))) %>%
  unnest(c(VP, HP, VR, HR, resultant)) %>%
  rename(RM = rMag,
         RA = rAng) %>%
  select(-forceCurves) %>%
  gather(key = "measure", value = "result_n", one_of(measures)) %>%
  mutate(measure = factor(measure, levels = c("VP", "VR", "HP", "HR", "RM", "RA"),
                          labels = c("VertPeak", "VertRFD", "HorzPeak", "HorzRFD",
                                     "ResultantPeak", "ResultantAngle"))) %>%
  mutate(result_bw = case_when(measure != "ResultantAngle" ~ result_n / weight,
                               measure == "ResultantAngle" ~ result_n)) %>%
  select(partID, measure, movement, leg, rep, result_n, result_bw) %>%
  arrange(partID, measure, movement, leg, rep)

pkvsFullChars <- pkvsFull %>%
  full_join(characteristics, by = "partID")

# with 3 reps averaged

pkvsAvg <- pkvsFull %>%
  group_by(partID, movement, leg, measure) %>%
  summarise(result_n = mean(result_n),
            result_bw = mean(result_bw)) %>%
  ungroup() %>%
  select(partID, measure, movement, leg, result_n, result_bw) %>%
  arrange(partID, measure, movement, leg)

pkvsCharsAvg <- pkvsFullChars %>%
  group_by(partID, movement, leg, measure) %>%
  summarise(result = mean(result_n),
            result_bw = mean(result_bw),
            age = unique(age),
            gender = unique(gender),
            height = unique(height),
            boxRatio = unique(boxRatio),
            weight = unique(weight),
            weightKg = unique(weightKg),
            yearsTraining = unique(yearsTraining),
            preferredFoot = unique(preferredFoot)) %>%
  ungroup %>%
  arrange(partID, measure, movement, leg) %>%
  select(partID, measure, movement, leg, result, result_bw,
         age, gender, height, boxRatio, weight, weightKg, yearsTraining, preferredFoot)

# with median of 3 reps instead of average

pkvs <- pkvsFull %>%
  group_by(partID, movement, leg, measure) %>%
  summarise(result_n = median(result_n),
            result_bw = median(result_bw)) %>%
  ungroup() %>%
  select(partID, measure, movement, leg, result_n, result_bw) %>%
  arrange(partID, measure, movement, leg)

pkvsChars <- pkvsFullChars %>%
  group_by(partID, movement, leg, measure) %>%
  summarise(result_n = median(result_n),
            result_bw = median(result_bw),
            age = unique(age),
            gender = unique(gender),
            height = unique(height),
            boxRatio = unique(boxRatio),
            weight = unique(weight),
            weightKg = unique(weightKg),
            yearsTraining = unique(yearsTraining),
            preferredFoot = unique(preferredFoot)) %>%
  ungroup %>%
  arrange(partID, measure, movement, leg) %>%
  select(partID, measure, movement, leg, result_n, result_bw,
         age, gender, height, boxRatio, weight, weightKg, yearsTraining, preferredFoot)

# in wide format, all reps
pkvsFullWide <- forceCurves %>%
  mutate(VP = map(forceCurves, ~findPeak(.x, "Fz")),
         HP = map(forceCurves, ~findPeak(.x, "Fy")),
         VR = map(forceCurves, ~findRFD(.x, "Fz")),
         HR = map(forceCurves, ~findRFD(.x, "Fy")),
         resultant = map(forceCurves, ~findResultant(.x, "rMag", full = "no"))) %>%
  unnest(VP, HP, VR, HR, resultant) %>%
  rename(RM = rMag,
         RA = rAng) %>%
  select(-forceCurves, -weight) %>%
  gather(key = "measure", value = "result", one_of(measures)) %>%
  unite(col = "movement", movement, leg, measure, rep, sep = "") %>%
  spread(movement, result) %>%
  full_join(characteristics, by = "partID") %>%
  select(partID, age, gender, height, boxRatio, weight, weightKg,
         yearsTraining, preferredFoot, everything())

# wide with 3 reps averaged
pkvsAvgWide <- forceCurves %>%
  mutate(VP = map(forceCurves, ~findPeak(.x, "Fz")),
         HP = map(forceCurves, ~findPeak(.x, "Fy")),
         VR = map(forceCurves, ~findRFD(.x, "Fz")),
         HR = map(forceCurves, ~findRFD(.x, "Fy")),
         resultant = map(forceCurves, ~findResultant(.x, "rMag", full = "no"))) %>%
  unnest(c(VP, HP, VR, HR, resultant)) %>%
  rename(RM = rMag,
         RA = rAng) %>%
  select(-forceCurves, -weight) %>%
  gather(key = "measure", value = "result", one_of(measures)) %>%
  group_by(partID, movement, leg, measure) %>%
  summarise(result = mean(result)) %>%
  ungroup %>%
  unite(col = "movement", movement, leg, measure, sep = "") %>%
  spread(movement, result) %>%
  full_join(characteristics, by = "partID") %>%
  select(partID, age, gender, height, boxRatio, weight, weightKg,
         yearsTraining, preferredFoot, everything())

# wide with median of 3 reps
pkvsWide <- forceCurves %>%
  mutate(VP = map(forceCurves, ~findPeak(.x, "Fz")),
         HP = map(forceCurves, ~findPeak(.x, "Fy")),
         VR = map(forceCurves, ~findRFD(.x, "Fz")),
         HR = map(forceCurves, ~findRFD(.x, "Fy")),
         resultant = map(forceCurves, ~findResultant(.x, "rMag", full = "no"))) %>%
  unnest(c(VP, HP, VR, HR, resultant)) %>%
  rename(RM = rMag,
         RA = rAng) %>%
  select(-forceCurves, -weight) %>%
  gather(key = "measure", value = "result", one_of(measures)) %>%
  group_by(partID, movement, leg, measure) %>%
  summarise(result = median(result)) %>%
  ungroup %>%
  unite(col = "movement", movement, leg, measure, sep = "") %>%
  spread(movement, result) %>%
  full_join(characteristics, by = "partID") %>%
  select(partID, age, gender, height, boxRatio, weight, weightKg,
         yearsTraining, preferredFoot, everything())

# save to RData file
save(pkvs, pkvsFull, characteristics, resultantForces,
     file = here("data", "pkvs.RData"))
save(pkvsChars, pkvsFullChars, pkvsAvg, pkvsCharsAvg,
     pkvsFullWide, pkvsAvgWide, pkvsWide,
     file = here("data", "pkvsExtra.RData"))
save(forceCurves, file = here("data", "forceCurves.RData"))

# export to Excel format
xlExport <- createWorkbook()
addWorksheet(xlExport, "Wide")
addWorksheet(xlExport, "PKVS")
addWorksheet(xlExport, "Characteristics")
writeData(xlExport, "Wide", x = pkvsWide, colNames = TRUE)
writeData(xlExport, "PKVS", x = pkvs, colNames = TRUE)
writeData(xlExport, "Characteristics", x = characteristics, colNames = TRUE)
saveWorkbook(xlExport, here("data", "excel", "pkvs.xlsx"), overwrite = TRUE)

# export to SPSS format
for (m in measures) {
  saveData <- pkvsWide %>%
    select(partID, age, gender, height, boxRatio, weight, weightKg,
           yearsTraining, preferredFoot, contains(m, ignore.case = FALSE))

  write_sav(saveData, path = paste0(here("data", "spss", "pkvs"), m, ".sav"))

  rm(saveData)
}
