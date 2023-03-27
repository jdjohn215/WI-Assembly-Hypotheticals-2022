rm(list = ls())

library(tidyverse)

# This script builds a model to predict Assembly vote

orig <- read_csv("WI_2022_Votes_in_Assembly_Districts.csv")

# margins for all races in 2022 Assembly Districts
margins.2022 <- orig %>%
  filter(plan == "2022") %>%
  select(office, district, margin) %>%
  pivot_wider(names_from = office, values_from = margin)

# two-party vote total in 2022 Assembly Districts
vote.total.2022 <- orig %>%
  filter(plan == "2022") %>%
  mutate(party2_tot = DEM + REP) %>%
  select(office, district, party2_tot) %>%
  pivot_wider(names_from = office, values_from = party2_tot)

# assembly seats contested by both parties in 2022
contested.assembly.22 <- orig %>%
  filter(plan == "2022",
         office == "WSA",
         DEM > 0,
         REP > 0)

# just the margins for contested assembly districts
margins.contested <- margins.2022 %>%
  filter(district %in% contested.assembly.22$district)

# just the vote totals for contested assembly districts
vote.totals.contested <- vote.total.2022 %>%
  filter(district %in% contested.assembly.22$district)


# Two-step modelling process
#   Use the "v1" model to predict Assembly winner
#   Use the "v2" model to predict Assembly margin using interaction term with predicted winner from "v1"
lm.margin.v1 <- lm(WSA ~ GOV + USS + WAG + WST, data = margins.contested)

margins.contested.v2 <- margins.contested %>%
  mutate(predicted_margin_v1 = predict.lm(lm.margin.v1, newdata = .[]),
         predicted_dem_win = if_else(predicted_margin_v1 > 0, 1, 0))

lm.margin.v2 <- lm(WSA ~ predicted_dem_win + GOV*predicted_dem_win +
                     USS*predicted_dem_win + WAG*predicted_dem_win +
                     WST*predicted_dem_win, data = margins.contested.v2)

summary(lm.margin.v1)
summary(lm.margin.v2)

predicted.margin <- orig %>%
  select(plan, office, district, margin) %>%
  pivot_wider(names_from = office, values_from = margin) %>%
  mutate(predict_v1 = predict.lm(lm.margin.v1, newdata = .[]),
         predicted_dem_win = if_else(predict_v1 > 0, 1, 0)) %>%
  mutate(predict_v2 = predict.lm(lm.margin.v2, newdata = .[])) %>%
  mutate(predict_avg = (GOV + USS + WAG + WST)/4,
         final_wsa = case_when(
           plan != "2022" ~ predict_v2,
           district %in% contested.assembly.22$district ~ WSA,
           TRUE ~ predict_v2
         ))

compare.residuals <- predicted.margin %>%
  filter(plan == "2022",
         district %in% contested.assembly.22$district) %>%
  mutate(residuals_v1 = WSA - predict_v1,
         residuals_v2 = WSA - predict_v2,
         residuals_avg = WSA - predict_avg)

summary(compare.residuals$residuals_v1)
sd(compare.residuals$residuals_v1)
summary(compare.residuals$residuals_v1)
sd(compare.residuals$residuals_v1)
summary(compare.residuals$residuals_avg)
sd(compare.residuals$residuals_avg)

# compare the residuals from the various models
compare.residuals %>%
  select(district, WSA, starts_with("residuals")) %>%
  pivot_longer(cols = starts_with("residuals"), names_to = "model", values_to = "residuals") %>%
  ggplot(aes(WSA, residuals)) +
  geom_point() +
  ggrepel::geom_text_repel(data = function(x){filter(x, abs(residuals) > 5)},
                           aes(label = district), min.segment.length = 0.01) +
  facet_wrap(facets = vars(model))


####################################################################
# model vote totals
lm.votes.v1 <- lm(WSA ~ GOV + USS + WAG + WST, data = vote.totals.contested)
vote.totals.contested %>%
  mutate(predicted = predict(lm.votes.v1),
         residuals = residuals(lm.votes.v1)) %>%
  arrange(residuals)

predicted.votes <- orig %>%
  mutate(party2_tot = DEM + REP) %>%
  select(plan, office, district, party2_tot) %>%
  pivot_wider(names_from = office, values_from = party2_tot) %>%
  mutate(predict_party2tot = predict.lm(lm.votes.v1, newdata = .[])) %>%
  select(plan, district, predict_party2tot)

predicted.wsa <- inner_join(predicted.margin, predicted.votes) %>%
  mutate(contested_2022 = case_when(
    plan != "2022" ~ NA_character_,
    district %in% contested.assembly.22$district ~ "contested",
    TRUE ~ "uncontested"
  ))


write_csv(predicted.wsa, "model/AssemblyDistricts_with_ModelledVote.csv")

