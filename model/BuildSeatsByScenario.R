rm(list = ls())

library(tidyverse)

assembly.all <- read_csv("model/AssemblyDistricts_with_ModelledVote.csv")

swings <- seq(-20, 20, by = 0.1)

seats_by_swing <- function(margin_swing, base_margin){
  assembly.all %>%
    select(plan, district, {{base_margin}}) %>%
    mutate(adj_margin = {{base_margin}} + margin_swing,
           winner = case_when(
             adj_margin > 0 ~ "dem",
             adj_margin == 0 ~ "tie",
             adj_margin < 0 ~ "rep"
           )) %>%
    group_by(plan, winner) %>%
    summarise(districts = n(),
              swing = margin_swing,
              baseline = rlang::as_string(rlang::ensym(base_margin)),
              .groups = "drop")
}


applied.swings.modeled <- map_df(swings, seats_by_swing, base_margin = final_wsa, .progress = T)
applied.swings.evers <- map_df(swings, seats_by_swing, base_margin = GOV, .progress = T)
applied.swings.johnson <- map_df(swings, seats_by_swing, base_margin = USS, .progress = T)

all.applied.swings <- bind_rows(applied.swings.modeled, applied.swings.evers, applied.swings.johnson) %>%
  pivot_wider(names_from = winner, values_from = districts)
write_csv(all.applied.swings, "model/AssemblySeats_by_Scenario.csv")

