rm(list = ls())

library(tidyverse)

# This script builds a model to predict Assembly vote

orig <- read_csv("WI_2022_Votes_in_Assembly_Districts.csv")

# margins for all races in 2022 Assembly Districts
margins.2022 <- orig %>%
  filter(plan == "2022") %>%
  select(office, district, margin) %>%
  pivot_wider(names_from = office, values_from = margin)

# assembly seats contested by both parties in 2022
contested.assembly.22 <- orig %>%
  filter(plan == "2022",
         office == "WSA",
         DEM > 0,
         REP > 0)

# just the margins for contested assembly districts
margins.contested <- margins.2022 %>%
  filter(district %in% contested.assembly.22$district)


# Two-step modelling process
#   Use the "v1" model to predict Assembly winner
#   Use the "v2" model to predict Assembly margin using interaction term with predicted winner from "v1"
lm.wsa.v1 <- lm(WSA ~ GOV + USS + WAG + WST, data = margins.contested)

margins.contested.v2 <- margins.contested %>%
  mutate(predicted_margin_v1 = predict.lm(lm.wsa.v1, newdata = .[]),
         predicted_dem_win = if_else(predicted_margin_v1 > 0, 1, 0))

lm.wsa.v2 <- lm(WSA ~ predicted_dem_win + GOV*predicted_dem_win +
                  USS*predicted_dem_win + WAG*predicted_dem_win +
                  WST*predicted_dem_win, data = margins.contested.v2)

summary(lm.wsa.v1)
summary(lm.wsa.v2)

predicted.wsa <- orig %>%
  select(plan, office, district, margin) %>%
  pivot_wider(names_from = office, values_from = margin) %>%
  mutate(predict_v1 = predict.lm(lm.wsa.v1, newdata = .[]),
         predicted_dem_win = if_else(predict_v1 > 0, 1, 0)) %>%
  mutate(predict_v2 = predict.lm(lm.wsa.v2, newdata = .[])) %>%
  mutate(predict_avg = (GOV + USS + WAG + WST)/4,
         final_wsa = case_when(
           plan != "2022" ~ predict_v2,
           district %in% contested.assembly.22$district ~ WSA,
           TRUE ~ predict_v2
         ))

compare.residuals <- predicted.wsa %>%
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

# compare congruence of win totals
predicted.wsa %>%
  filter(plan == "2022") %>%
  select(district, WSA, starts_with("predict_")) %>%
  pivot_longer(cols = starts_with("predict"), names_to = "method", values_to = "prediction") %>%
  mutate(winner_comparison = case_when(
    WSA < 0 & prediction < 0 ~ "agree, rep won",
    WSA > 0 & prediction > 0 ~ "agree, dem won",
    WSA < 0 & prediction > 0 ~ "disagree, rep won",
    WSA > 0 & prediction < 0 ~ "disagree, dem won"
  )) %>%
  group_by(method, winner_comparison) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = method, values_from = count, values_fill = 0)

write_csv(predicted.wsa, "model/AssemblyDistricts_with_ModelledVote.csv")
