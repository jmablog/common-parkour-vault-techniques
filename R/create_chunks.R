# function to split the resulting p values back into 6 for the pairwise adjustments
create_chunks <- function(x, elements.per.chunk){
  # plain R version
  # split(x, rep(seq_along(x), each = elements.per.chunk)[seq_along(x)])
  # magrittr version - because that's what people use now
  x %>% seq_along %>% rep(., each = elements.per.chunk) %>% magrittr::extract(seq_along(x)) %>% split(x, .) 
}