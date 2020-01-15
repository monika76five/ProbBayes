compute_individual_regressions <- function(d1){
  library(tidyverse)
  library(broom)
  regressions <- d1 %>%
    split(pull(., nameLast)) %>%
    map(~ glm(cbind(OB, PA - OB) ~ AgeD +
                I(AgeD ^ 2),
              family = binomial, data = .)) %>%
    map_df(tidy, .id = "Name") %>%
    as_tibble() %>% select(Name, term, estimate)
  
  # compute predicted probabilities for all seasons and players
  
  regressions %>% spread(term, estimate) -> regs
  names(regs) <- c("Name", "B0", "B1", "B2")
  
  inner_join(d1, regs, by=c("nameLast"="Name")) -> d1
  
  d1 %>%
    mutate(LP = B0 + B1 * AgeD +
             B2 * AgeD ^ 2,
           P = exp(LP) / (1 + exp(LP))) -> d1
  d1
}
