# this file will be read in to the main book file as part of index.Rmd,
# and called from various points in the main text using chunk names.
# See index.Rmd for libraries and data loading.

## ---- participant-chars-tab

part_tab <- characteristics %>%
  select(age, height, weightKg, yearsTraining) %>%
  rename(`Age (yrs)` = age,
         `Height (cm)` = height,
         `Weight (kg)` = weightKg,
         `Years Training (yrs)` = yearsTraining) %>%
  gather(key = "characteristic", value = "value") %>%
  group_by(characteristic) %>%
  summarise(Mean = round(mean(value), 2),
            SD = round(sd(value), 2)) %>%
  ungroup() %>%
  # unite(`Mean (SD)`, Mean, SD, sep = " (") %>%
  # mutate(bracket = ")") %>%
  unite(`Mean ± SD`, Mean, SD, sep = " ± ") %>%
  rename(` ` = characteristic)

part_tab_cap <- "Participant characteristics (n = 10)."
part_tab_scap <- "Participant characteristics"

if(pdf | git) {
  part_tab %>%
    kableExtra::kbl(caption = part_tab_cap,
        caption.short = part_tab_scap,
        booktabs = TRUE,
        escape = TRUE,
        align = c("l", "r")) %>%
    kableExtra::kable_styling(full_width = TRUE)
} else if(word) {
  part_tab %>%
    knitr::kable(caption = part_tab_cap,
          caption.short = part_tab_scap,
          booktabs = TRUE)
} else {
  part_tab
}

# ========================================================================
# ========================================================================

## ---- participant-cat-tab

part_cat_tab <- characteristics %>%
  select(partID, gender, preferredFoot) %>%
  count(gender, preferredFoot) %>%
  spread(preferredFoot, n) %>%
  replace_na(list(Left = 0, Right = 0)) %>%
  rename(` ` = gender,
         `Left Foot` = Left,
         `Right Foot` = Right)

part_cat_tab_cap <- "Participant categorical characteristics (n = 10)."
part_cat_tab_scap <- "Participant categorical characteristics"

if(pdf | git) {
  part_cat_tab %>%
    kableExtra::kbl(caption = part_cat_tab_cap,
        caption.short = part_cat_tab_scap,
        booktabs = TRUE,
        escape = TRUE) %>%
    kableExtra::kable_styling(full_width = TRUE)
} else if(word) {
  part_cat_tab %>%
    knitr::kable(caption = part_cat_tab_cap,
          caption.short = part_cat_tab_scap,
          booktabs = TRUE)
} else {
  part_cat_tab
}

# ========================================================================
# ========================================================================

## ---- anova-tab

# calculate partial eta sq. separately from ANOVA results as ez does not give partial, only gen

fullANOVA <- pkvs %>%
  split(.$measure) %>%
  map(~ezANOVA(data = ., dv = result_bw, wid = partID, within = .(movement, leg),
               type = 2, return_aov = T, detailed= T))

partEta <- map(fullANOVA, "aov") %>%
  map(~DescTools::EtaSq(.x, type = 1) %>% as.data.frame) %>%
  enframe %>% unnest(cols = everything()) %>% rename(Measure = name) %>%
  mutate(Effect = rep(c("movement", "leg", "movement:leg"), times = 2)) %>%
  select(Measure, Effect, eta.sq.part) %>%
  rename(`Part. η2` = eta.sq.part) %>%
  pull(3)

# compile and format results into presentation table
anovaResults <- pkvs %>%
  group_by(measure) %>%
  group_modify(aov_by_measure) %>%
  ungroup() %>%
  select(-W, -GGe, -HFe, -pHF, -`pSpher<.05`, -`pHF<.05`) %>%
  mutate_if(is.numeric, ~round(.x, 3)) %>%
  replace_na(list(pSpher = 1, pGG = 1, `pGG<.05` = "")) %>%
  mutate(p = case_when(pSpher <= .05 ~ pGG,
                       pSpher > .05 ~ pVal)) %>%
  select(measure, Effect, DFn, DFd, `F`, p) %>%
  mutate(psig = case_when(p <= .05 ~ "*",
                          p > .05 ~ "")) %>%
  mutate(p = case_when(p < 0.001 ~ "< 0.001",
                       p >= 0.001 ~ as.character(round(p, 3)))) %>%
  unite(p, p, psig, sep = "") %>%
  rename(Measure = measure) %>%
  mutate(Measure = str_replace_all(Measure, "Vert", "Vertical"),
         Measure = str_replace_all(Measure, "Horz", "Horizontal"),
         Measure = str_replace_all(Measure, "Peak", " Peak"),
         Measure = str_replace_all(Measure, "Angle", " Angle"),
         Effect = str_replace_all(Effect, "movement", "Movement"),
         Effect = str_replace_all(Effect, "leg", "Landing Style")) %>%
  mutate(`ηp2` = round(partEta, 2))

anovaResults[-c(1, 4), 1] <- " "

anova_cap <- "Two-way repeated-measures ANOVA results. * indicates significance (p < 0.05)."
anova_scap <- "Two-way repeated-measures ANOVA results"

if(pdf) {
    anovaResults %>%
      select(-Measure) %>%
      rename("$η_p^2$" = ηp2,
             "\\textit{p}" = p) %>%
      kableExtra::kbl(caption = anova_cap,
          caption.short = anova_scap,
          booktabs = TRUE,
          escape = FALSE,
          align = c("l", "r", "r", "r", "r", "r")) %>%
      kableExtra::kable_styling(full_width = TRUE) %>%
      pack_rows(index=c("Peak vGRF" = 3, "Peak bGRF" = 3)) %>%
      column_spec(1, width = "5cm")
  } else if(git) {
    anovaResults %>%
      rename(`n~p~^2^` = ηp2) %>%
      kableExtra::kbl(caption = anova_cap,
                      caption.short = anova_scap,
                      booktabs = TRUE,
                      escape = FALSE,
                      align = c("l", "r", "r", "r", "r", "r")) %>%
      kableExtra::kable_styling(full_width = TRUE) %>%
      pack_rows(index=c("Peak vGRF" = 3, "Peak bGRF" = 3))
  } else if(word) {
    anovaResults %>%
      knitr::kable(caption = anova_cap,
            caption.short = anova_scap,
            booktabs = TRUE)
  } else {
    anovaResults
}

# ========================================================================
# ========================================================================

## ---- landing-style-tab

simpleEffects_landingStyle <- list()

for (m in c("VertPeak", "HorzPeak")) {

  tmp <- pkvs %>%
      filter(measure == m) %>%
      split(.$movement)

  # legs <- c("Single", "Double")

  for (con in movements) {

  simpleEffects_landingStyle[[m]][[con]] <- tmp[[con]] %$%
    t.test(result_bw ~ leg, paired = T)
    # pairwise.wilcox.test(result_bw, leg, paired = T, p.adjust.method = pAdjustChoice)
    # both the above give the same result, only difference is getting the conf. intervals

  }
  rm(tmp, m, con)
}

simpleEffects_landingStyle_df <- map_dfr(simpleEffects_landingStyle,
                                   ~map_dfr(.x, tidy, .id = "Movement"),
                                   .id = "Measure") %>%
  # mutate(Contrast = "Precision / Running") %>%
  mutate(p.value = p.adjust(p.value, method = "holm")) %>%
  mutate(psig = case_when(p.value <= .05 ~ "*",
                          p.value > .05 ~ "")) %>%
  mutate(p = case_when(p.value < 0.001 ~ "< 0.001",
                       p.value >= 0.001 ~ as.character(round(p.value, 3)))) %>%
  unite(p, p, psig, sep = "") %>%
  mutate(`Diff. (BW)` = round(estimate, 2)) %>%
  rename(t = statistic) %>%
  mutate(t = round(t, 2)) %>%
  select(Measure, Movement, `Diff. (BW)`, t, p) %>%
  mutate(Measure = str_replace_all(Measure, "Vert", "Vertical"),
         Measure = str_replace_all(Measure, "Horz", "Horizontal"),
         Measure = str_replace_all(Measure, "Peak", " Peak"))

g_landingStyle <- list()

for (m in c("VertPeak", "HorzPeak")) {

  tmp <- pkvs %>%
      filter(measure == m) %>%
      split(.$movement)

  # legs <- c("Single", "Double")

  for (con in movements) {

  g_landingStyle[[m]][[con]] <-
    effectsize::hedges_g(result_bw ~ leg, data = tmp[[con]], paired = T)#, correction = T)
    # pairwise.wilcox.test(result_bw, leg, paired = T, p.adjust.method = pAdjustChoice)
    # both the above give the same result, only difference is getting the conf. intervals

  }
  rm(tmp, m, con)
}

g_landingStyle_df <- g_landingStyle %>%
  as.data.frame() %>%
  pivot_longer(cols = everything(),
               names_to = "name",
               values_to = "value") %>%
  separate(name, sep = "\\.",
           into = c("Measure", "Movement", "type")) %>%
  pivot_wider(names_from = type,
              values_from = value) %>%
  mutate(g = round(Hedges_g, 2)) %>%
  select(Measure, Movement, g) %>%
  mutate(Measure = str_replace_all(Measure, "Vert", "Vertical"),
         Measure = str_replace_all(Measure, "Horz", "Horizontal"),
         Measure = str_replace_all(Measure, "Peak", " Peak"))

landingstyle_tab <- simpleEffects_landingStyle_df %>%
  left_join(g_landingStyle_df)

landingstyle_tab[c(-1, -5), 1] <- ""

landingeffects_cap <- "Pairwise comparisons for the effect of landing style (precision vs. running) on each movement. Δ shows mean change in force (BW) when switching from a running to precision landing style. * indicates significant difference between landing styles (p < 0.05)."
landingeffects_scap <- "Pairwise comparisons for the effect of landing style"

if(pdf) {
  landingstyle_tab %>%
    select(-Measure) %>%
    rename(`\\textit{t}` = t,
           `\\textit{p}` = p,
           `\\textit{g}` = g) %>%
    kableExtra::kbl(caption = landingeffects_cap,
                    caption.short = landingeffects_scap,
                    booktabs = TRUE,
                    escape = FALSE,
                    align = c("l", "r", "r", "r", "r")) %>%
    kableExtra::kable_styling(full_width = TRUE) %>%
    pack_rows(index=c("Peak vGRF" = 4, "Peak bGRF" = 4))
} else if(git) {
  landingstyle_tab %>%
    select(-Measure) %>%
    kableExtra::kbl(caption = landingeffects_cap,
                    caption.short = landingeffects_scap,
                    booktabs = TRUE,
                    escape = FALSE,
                    align = c("l", "r", "r", "r", "r")) %>%
    kableExtra::kable_styling(full_width = TRUE) %>%
    pack_rows(index=c("Peak vGRF" = 4, "Peak bGRF" = 4))
} else if(word) {
  landingstyle_tab %>%
    knitr::kable(caption = landingeffects_cap,
                 caption.short = landingeffects_scap,
                 booktabs = TRUE)
} else {
  landingstyle_tab
}

# ========================================================================
# ========================================================================

## ---- movement-choice-tab

# create a dataframe of all the pairwise combinations to be tested
pairwise_contrasts <- tibble(
  Measure = rep(c("VertPeak", "HorzPeak"), each = 12),
  Landing = rep(c("Precision", "Running"), each = 6, times = 2),
  Movement = rep(c(rep("Drop", times = 3), rep("Step", times = 2), "Dash"), times = 4),
  Contrast = rep(c("Step", "Dash", "Kong", "Dash", "Kong", "Kong"), times = 4)
)

# calculate pairwise comparisons and effect sizes with manual_t function
movementchoice <- pmap_dfr(pairwise_contrasts, manual_t) %>%
  mutate(p.adj = unlist(map(create_chunks(p, 6), ~p.adjust(.x, "holm")), use.names = F)) %>%
  select(-p, -df) %>%
  mutate(p.adj = round(p.adj, 3)) %>%
  mutate(p = case_when(p.adj == 1 ~ " = 1.000",
                       p.adj < 0.001 ~ " < 0.001*",
                       p.adj < 0.05 ~ as.character(glue(" = {p.adj}*")),
                       TRUE ~ as.character(glue(" = {p.adj}")))) %>%
  mutate(across(where(is.double), ~ as.character(.x))) %>%
  mutate(`Δ` = case_when(diff == "-0.6" ~ "-0.60",
                         diff == "0.1" ~ "0.10",
                         TRUE ~ diff)) %>%
  mutate(t = case_when(t == "1.4" ~ "1.40",
                       TRUE ~ t)) %>%
  mutate(g = case_when(g == "0.5" ~ "0.50",
                       g == "0.4" ~ "0.40",
                       TRUE ~ g)) %>%
  select(Measure, `Landing Style`, Movement, Contrast, `Δ`, t, p, g)

# create vertical peak table
movementchoice_vgrf <-
  movementchoice %>%
  filter(Measure == "VertPeak") %>%
  pivot_longer(5:8, names_to = "stat") %>%
  pivot_wider(names_from = Contrast, values_from = value) %>%
  mutate(Step = case_when(!is.na(Step) & stat == "p" ~ glue("{stat}{Step}"),
                          !is.na(Step) & stat != "p" ~ glue("{stat} = {Step}"),
                          is.na(Step) ~ glue("")),
         Dash = case_when(!is.na(Dash) & stat == "p" ~ glue("{stat}{Dash}"),
                          !is.na(Dash) & stat != "p" ~ glue("{stat} = {Dash}"),
                          is.na(Dash) ~ glue("")),
         Kong = case_when(!is.na(Kong) & stat == "p" ~ glue("{stat}{Kong}"),
                          !is.na(Kong) & stat != "p" ~ glue("{stat} = {Kong}"),
                          is.na(Kong) ~ glue(""))) %>%
  pivot_wider(names_from = `Landing Style`, values_from = c(Step, Dash, Kong),
              names_sep = "_") %>%
  select(Measure, Movement, Step_Precision, Dash_Precision, Kong_Precision, Step_Running, Dash_Running, Kong_Running) %>%
  rename(Step = Step_Precision,
         Dash = Dash_Precision,
         Kong = Kong_Precision,
         ` Step` = Step_Running,
         ` Dash` = Dash_Running,
         ` Kong` = Kong_Running)

# create horizontal peak table

movementchoice_bgrf <-
  movementchoice %>%
  filter(Measure == "HorzPeak") %>%
  pivot_longer(5:8, names_to = "stat") %>%
  pivot_wider(names_from = Contrast, values_from = value) %>%
  mutate(Step = case_when(!is.na(Step) & stat == "p" ~ glue("{stat}{Step}"),
                          !is.na(Step) & stat != "p" ~ glue("{stat} = {Step}"),
                          is.na(Step) ~ glue("")),
         Dash = case_when(!is.na(Dash) & stat == "p" ~ glue("{stat}{Dash}"),
                          !is.na(Dash) & stat != "p" ~ glue("{stat} = {Dash}"),
                          is.na(Dash) ~ glue("")),
         Kong = case_when(!is.na(Kong) & stat == "p" ~ glue("{stat}{Kong}"),
                          !is.na(Kong) & stat != "p" ~ glue("{stat} = {Kong}"),
                          is.na(Kong) ~ glue(""))) %>%
  pivot_wider(names_from = `Landing Style`, values_from = c(Step, Dash, Kong)) %>%
  select(Measure, Movement, Step_Precision, Dash_Precision, Kong_Precision, Step_Running, Dash_Running, Kong_Running) %>%
  rename(Step = Step_Precision,
         Dash = Dash_Precision,
         Kong = Kong_Precision,
         ` Step` = Step_Running,
         ` Dash` = Dash_Running,
         ` Kong` = Kong_Running)

# combine the two
movementchoice_tab <- rbind(movementchoice_vgrf, movementchoice_bgrf)
movementchoice_tab[c(-1, -13), 1] <- ""
movementchoice_tab[c(-1, -5, -9, -13, -17, -21), 2] <- ""

# print with Kable

movementchoice_cap <- "Pairwise comparisons for the effect of movement choice within each landing style. Δ shows mean change in force (BW) when switching from column movement to row movement. * denotes significant difference (p < 0.05)."
movementchoice_scap <- "Pairwise comparisons for the effect of movement choice"

if(pdf | git) {
  movementchoice_tab %>%
    select(-Measure) %>%
    rename(" " = Movement) %>%
    kableExtra::kbl(caption = movementchoice_cap,
                    caption.short = movementchoice_scap,
                    booktabs = TRUE,
                    longtable = TRUE,
                    escape = FALSE,
                    align = c("l", "r", "r", "r", "r", "r", "r")) %>%
    kableExtra::kable_styling(latex_options = c("repeat_header"),
                              #latex_options = c("striped", "repeat_header"),
                              #stripe_index = c(5:8, 17:20),
                              full_width = TRUE,
                              repeat_header_method = "replace") %>%
    column_spec(1, width = "3cm") %>%
    add_header_above(c(" " = 1, "Precision" = 3, "Running" = 3)) %>%
    row_spec(row = c(4, 8, 16, 20), hline_after = TRUE) %>%
    pack_rows(index=c("Peak vGRF" = 12, "Peak bGRF" = 12)) %>%
    landscape()
} else if(word) {
  movementchoice_tab %>%
    knitr::kable(caption = movementchoice_cap,
                 caption.short = movementchoice_scap,
                 booktabs = TRUE)
} else {
  movementchoice_tab
}

# ========================================================================
# ========================================================================

## ---- means-sds-tab

means_sds <- pkvs %>%
  select(-result_n) %>%
  group_by(measure, movement, leg) %>%
  summarise(mean = round(mean(result_bw), 2),
            sd = round(sd(result_bw), 2)) %>%
  ungroup %>%
  unite(col = "meanSD", sep = " ± ", mean, sd) %>%
  spread(movement, meanSD) %>%
  rename(Measure = measure,
         `Landing Style` = leg) %>%
  group_by(Measure) %>%
  group_modify(marginal_means) %>%
  ungroup()

means_sds_tab <- means_sds %>%
  mutate(Measure = as.character(Measure)) %>%
  mutate(Measure = str_replace_all(Measure, "Vert", "Vertical"),
         Measure = str_replace_all(Measure, "Horz", "Horizontal"),
         Measure = str_replace_all(Measure, "Peak", " Peak (BW)")) %>%
  rename(` ` = Measure,
         `  ` = `Landing Style`) %>%
  replace_na(list(`*Landing Style Avg.*` = " "))

means_sds_tab[c(-1, -4), 1] <- " "

means_sds_cap <- "Means ± standard deviations for all dependent variable, movement, and landing style combinations."
means_sds_scap <- "Means and standard deviations for all groups"

if(pdf) {
  means_sds_tab %>%
    select(-1) %>%
    rename(`\\textit{Row Mean ± SD}` = `*Landing Style Avg.*`) %>%
    mutate(`  ` = str_replace_all(`  `, "\\*Movement Avg.\\*", "\\\\textit{Column Mean ± SD}")) %>%
    kableExtra::kbl(caption = means_sds_cap,
                    caption.short = means_sds_scap,
                    booktabs = TRUE,
                    escape = FALSE,
                    align = c("l", "r", "r", "r", "r", "r")) %>%
    kableExtra::kable_styling(full_width = TRUE) %>%
    pack_rows(index=c("Peak vGRF" = 3, "Peak bGRF" = 3)) %>%
    column_spec(1, width = "5cm") %>%
    landscape()
} else if(git) {
  means_sds_tab %>%
    select(-1) %>%
    rename(`*Row Mean ± SD*` = `*Landing Style Avg.*`) %>%
    mutate(`  ` = str_replace_all(`  `, "\\*Movement Avg.\\*", "*Column Mean ± SD*")) %>%
    kableExtra::kbl(caption = means_sds_cap,
                    caption.short = means_sds_scap,
                    booktabs = TRUE,
                    escape = FALSE,
                    align = c("l", "r", "r", "r", "r", "r")) %>%
    kableExtra::kable_styling(full_width = TRUE) %>%
    pack_rows(index=c("Peak vGRF" = 3, "Peak bGRF" = 3)) %>%
    column_spec(1, width = "5cm")
} else if(word) {
  means_sds_tab %>%
    knitr::kable(caption = means_sds_cap,
                 caption.short = means_sds_scap,
                 booktabs = TRUE)
} else {
  means_sds_tab
}
