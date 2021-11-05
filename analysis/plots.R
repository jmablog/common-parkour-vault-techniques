# this file will be read in to the main book file as part of index.Rmd,
# and called from various points in the main text using chunk names.
# See index.Rmd for libraries and data loading.

## ---- icc-plot

# ICC creation for all movements/legs/measures, using 3 reps as 3 raters

ICCs <- list()

for (m in 1:length(allMeasuresMovements$movement)) {

  x <- allMeasuresMovements[m, 2]
  y <- allMeasuresMovements[m, 3]
  z <- allMeasuresMovements[m, 4]

  ICCs[paste(x, y, z, sep = "-")] <- list(
    pkvsFull %>%
      filter(measure == z, leg == y, movement == x) %>%
      select(-measure, -movement, -leg, -result_n) %>%
      spread(key = rep, value = result_bw) %>%
      column_to_rownames("partID") %>%
      irr::icc(model = "twoway", type = "agreement", unit = "single")
  )
  rm(m, x, y, z)
}

# extracting ICC data from list and compiling in dataframe

iccDf <- data.frame(matrix(data = NA, nrow = length(ICCs), ncol = 15))
colnames(iccDf) <- c("ICC", "pVal", "ConfLo", "ConfHi", "ConfLevel",
                     "r0", "F", "df1", "df2",
                     "iccName", "model", "type", "unit", "subjects", "raters")
rownames(iccDf) <- names(ICCs)

for (i in allMeasuresMovements$combined) {

  iccDf[i,1] <- round(get(i, ICCs)$value, 2)
  iccDf[i,2] <- round(get(i, ICCs)$p.value, 3)
  iccDf[i,3] <- round(get(i, ICCs)$lbound, 2)
  iccDf[i,4] <- round(get(i, ICCs)$ubound, 2)
  iccDf[i,5] <- get(i, ICCs)$conf.level
  iccDf[i,6] <- round(get(i, ICCs)$r0, 2)
  iccDf[i,7] <- round(get(i, ICCs)$Fvalue, 2)
  iccDf[i,8] <- round(get(i, ICCs)$df1, 2)
  iccDf[i,9] <- round(get(i, ICCs)$df2, 2)
  iccDf[i,10] <- get(i, ICCs)$icc.name
  iccDf[i,11] <- get(i, ICCs)$model
  iccDf[i,12] <- get(i, ICCs)$type
  iccDf[i,13] <- get(i, ICCs)$unit
  iccDf[i,14] <- get(i, ICCs)$subjects
  iccDf[i,15] <- get(i, ICCs)$raters

  rm(i)
}

iccDf <- iccDf %>%
  rownames_to_column() %>%
  separate(rowname, into = c("movement", "leg", "measure"), sep = "-") %>%
  select(measure, movement, leg, everything()) %>%
  mutate_if(is.character, factor) %>%
  arrange(measure, movement, leg) %>%
  mutate(measure = factor(measure,
                          levels = c("VertPeak",
                                     "HorzPeak",
                                     "ResultantPeak",
                                     "ResultantAngle"),
                          labels = c("A. Peak vGRF",
                                     "B. Peak bGRF",
                                     "Resultant Peak",
                                     "Resultant Angle")),
         movement = factor(movement,
                           levels = c("Drop", "Step", "Dash", "Kong"),
                           labels = c("Drop", "Step", "Dash", "Kong")))

# ICC plot

iccDf %>%
  ggplot(aes(x = movement, y = ICC, colour = leg)) +
  geom_errorbar(aes(ymin = ConfLo, ymax = ConfHi), width = 0.15, alpha = 0.8,
                position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5), size = 1.5) +
  geom_hline(yintercept = 0.5, linetype = "dotted", alpha = 0.3) +
  geom_hline(yintercept = 0.75, linetype = "dotted", alpha = 0.3) +
  geom_hline(yintercept = 0.9, linetype = "dotted", alpha = 0.3) +
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.2),
                     labels = seq(from = -1, to = 1, by = 0.2)) +
  scale_color_grey(start = 0.2, end = 0.6) +
  expand_limits(y = c(-0.2, 1)) +
  facet_wrap(~measure) +
  labs(x = "Movement",
       y = "ICC",
       colour = "Landing Style") +
  theme(strip.text.x = element_text(hjust = -.02, size = 11, color = "black"),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        legend.title = element_text(color = "black"),
        legend.text = element_text(color = "black"))

# ==============================================================================
# ==============================================================================

## ---- interaction-plot

intPlots <- list()

for (m in measures) {

  if (m == "VertPeak") {
    limits <- 1
  }
  else {
    limits <- 0.8
  }

  jitter_amount <- 0.5

  intPlots[m] <- list(
    pkvs %>%
      filter(measure == m) %>%
      mutate(movement = factor(movement, levels = c("Kong", "Drop", "Step", "Dash"))) %>%
      cousineau_error_bars(unit = result_bw) %>%
      ggplot(aes(y = result, x = leg, colour = movement, shape = movement)) +
      geom_line(aes(group = movement, linetype = movement),
                position = position_dodge(width = jitter_amount),
                alpha = 0.5) +
      geom_errorbar(aes(ymin = result - normCI, ymax = result + normCI),
                    width = 0.2, alpha = 0.4, color = "black",
                    position = position_dodge(width = jitter_amount)) +
      geom_point(position = position_dodge(width = jitter_amount), size = 3) +
      scale_colour_grey(start = 0.2, end = 0.7) +
      scale_shape_manual(values=c(15, 16, 17, 18))+
      expand_limits(y = limits) +
      labs(colour = "Movement", linetype = "Movement", shape = "Movement",
        x = "Landing Style",
        y = y_label_create(m)) +
      theme(plot.subtitle = element_text(color = "black"),
            axis.line = element_line(color = "black"),
            axis.ticks = element_line(color = "black"),
            axis.title = element_text(color = "black"),
            axis.text = element_text(color = "black"),
            legend.title = element_text(color = "black"),
            legend.text = element_text(color = "black"))
  )
  rm(m)
}

int1 <- intPlots$VertPeak + labs(subtitle = "A. Peak vGRF")
int2 <- intPlots$HorzPeak + labs(subtitle = "B. Peak bGRF")

int1 + int2 + plot_layout(guides = 'collect') &
  theme(axis.title.x = element_blank())

# ==============================================================================
# ==============================================================================

## ---- intensity-plot

# calculate and pull means for drop-pre vGRF
# to use as baseline
vertDropPre <- pkvs %>%
  filter(measure == "VertPeak") %>%
  group_by(measure, movement, leg) %>%
  summarise(result_bw = mean(result_bw)) %>%
  ungroup() %>%
  filter(movement == "Drop" & leg == "Precision") %>%
  pull(result_bw)

# calculate relative intensity of all other movements
# compared to drop-pre and plot as bar plot
intensityP1 <- pkvs %>%
  filter(measure == "VertPeak") %>%
  group_by(measure, movement, leg) %>%
  summarise(result_bw = mean(result_bw)) %>%
  ungroup() %>%
  mutate(intensity = round(case_when(movement == "Drop" & leg == "Precision" ~ 1,
                                     TRUE ~ result_bw / vertDropPre), 2)) %>%
  unite(movement, movement, leg, sep = "\n", remove = F) %>%
  mutate(movement = factor(movement),
         movement = fct_reorder(movement, intensity)) %>%
  ggplot(aes(movement, intensity, fill = leg)) +
  geom_col(position = position_dodge2()) +
  geom_hline(aes(yintercept = 1), linetype = "dashed", alpha = .5) +
  labs(x = "Movement",
       y = "Intensity",
       fill = "Landing Style",
       subtitle = "A. Peak vGRF") +
  scale_x_discrete(labels = c("Dash", "Drop", "Step", "Kong",
                              "Dash", "Step", "Drop", "Kong")) +
  theme(plot.subtitle = element_text(color = "black"),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 0.8),
        legend.title = element_text(color = "black"),
        legend.text = element_text(color = "black"))

# calculate and pull means for drop-pre bGRF
# to use as baseline
horzDropPre <- pkvs %>%
  filter(measure == "HorzPeak") %>%
  group_by(measure, movement, leg) %>%
  summarise(result_bw = mean(result_bw)) %>%
  ungroup() %>%
  filter(movement == "Drop" & leg == "Precision") %>%
  pull(result_bw)

# calculate relative intensity of all other movements
# compared to drop-pre and plot as bar plot
intensityP2 <- pkvs %>%
  filter(measure == "HorzPeak") %>%
  group_by(measure, movement, leg) %>%
  summarise(result_bw = mean(result_bw)) %>%
  ungroup() %>%
  mutate(intensity = round(case_when(movement == "Drop" & leg == "Precision" ~ 1,
                                     TRUE ~ result_bw / horzDropPre), 2)) %>%
  unite(movement, movement, leg, sep = "\n", remove = F) %>%
  mutate(movement = factor(movement),
         movement = fct_reorder(movement, intensity)) %>%
  ggplot(aes(movement, intensity, fill = leg)) +
  geom_col(position = position_dodge2()) +
  geom_hline(aes(yintercept = 1), linetype = "dashed", alpha = .5) +
  labs(x = "Movement",
       y = "Intensity",
       fill = "Landing Style",
       subtitle = "B. Peak bGRF") +
  scale_x_discrete(labels = c("Dash", "Step", "Dash", "Drop",
                              "Step", "Drop", "Kong", "Kong")) +
  theme(plot.subtitle = element_text(color = "black"),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 0.8),
        legend.title = element_text(color = "black"),
        legend.text = element_text(color = "black"))

# combine vert and horz plots, with shared axis labels added
intensityLegend <- get_legend(intensityP1)

intensityPlots <- plot_grid(intensityP1 + theme(legend.position = "none",
                                                axis.title.x = element_blank()),
                            NULL,
                            intensityP2 + theme(legend.position = "none",
                                                axis.title.y = element_blank(),
                                                axis.title.x = element_blank()),
                            NULL,
                            intensityLegend,
                            nrow = 1, ncol = 5, rel_widths = c(1, .05, 1, .05, .4))

intensityPlots_sharedLabel <- ggdraw(add_sub(intensityPlots, "Movement", size = 12,
                                             vpadding=grid::unit(0,"lines"),y=5, x=0.44, vjust=4.5))

intensityPlots_sharedLabel

# ==============================================================================
# ==============================================================================
