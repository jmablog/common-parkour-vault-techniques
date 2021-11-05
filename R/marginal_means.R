marginal_means <- function(slice, key) {

  key <- key[[1,1]]

rowMeans <- pkvs %>%
  filter(measure == key) %>%
  select(-result_n) %>%
  group_by(leg) %>%
  summarise(mean = round(mean(result_bw), 2),
            sd = round(sd(result_bw), 2)) %>%
  ungroup() %>%
  unite(`*Landing Style Avg.*`, mean, sd, sep = " ± ")

colMeans <- pkvs %>%
  filter(measure == key) %>%
  select(-result_n) %>%
  group_by(movement) %>%
  summarise(mean = round(mean(result_bw), 2),
            sd = round(sd(result_bw), 2)) %>%
  ungroup() %>%
  unite(`Landing Style Means`, mean, sd, sep = " ± ") %>%
  spread(movement, 2) %>%
  mutate(`Landing Style` = "*Movement Avg.*",
         `*Landing Style Avg.*` = as.character(NA)) %>%
  select(`Landing Style`, Drop, Step, Dash, Kong, `*Landing Style Avg.*`)

slice <- cbind(slice, rowMeans[2])
slice <- rbind(slice, colMeans)

return(slice)

}
