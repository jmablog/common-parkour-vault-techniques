aov_by_measure <- function(slice, keys) {
  aov <- ezANOVA(data = slice, dv = result_bw, wid = partID, within = .(movement, leg),
              type = 2, return_aov = F, detailed= F)
  full_join(aov$ANOVA, aov$`Mauchly's Test for Sphericity`, by = "Effect") %>%
  full_join(aov$`Sphericity Corrections`) %>%
  rename(pVal = p.x,
         pSpher = p.y,
         `p<.05` = `p<.05.x`,
         `pSpher<.05` = `p<.05.y`,
         pGG = `p[GG]`,
         pHF = `p[HF]`,
         `pGG<.05` = `p[GG]<.05`,
         `pHF<.05` = `p[HF]<.05`,)

}
