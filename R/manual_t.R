# function to be applied across the pairwise dataframe above that will extract each contrast
# combination and perform a t-test and effect size test
manual_t <- function(Measure, Landing, Movement, Contrast) {

  x <- pkvs %>%
    filter(measure == Measure,
           leg == Landing,
           movement == Movement) %>%
    pull(result_bw)

  y <- pkvs %>%
    filter(measure == Measure,
           leg == Landing,
           movement == Contrast) %>%
    pull(result_bw)

  g <- hedges_g(x, y, paired = TRUE)
  g <- round(g$Hedges_g, 2)

  t.test(x, y, paired = T) %>%
    tidy() %>%
    mutate(Movement = Movement, Contrast = Contrast,
           Measure = Measure, `Landing Style` = Landing,
           g = g) %>%
    rename(diff = estimate, t = statistic, p = p.value, df = parameter) %>%
    mutate(diff = round(diff, 2),
           t = round(t, 2)) %>%
    select(Measure, `Landing Style`, Movement, Contrast, p, g, diff, t, df)

}
