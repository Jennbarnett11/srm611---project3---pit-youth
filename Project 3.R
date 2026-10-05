# ######################################################

# SRM 611 - Project 3: Count Models
# Unsheltered unaccompanied youth across states, 2025 PIT
# Author: Jenn Barnett
# Data: HUD, 2007-2025 Point-in-Time Estimates by State (2025 sheet),
#       HUD User AHAR page
# To run: open "Project 3.Rproj" in RStudio, then source this script

# #####################################################

# SETUP ####

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)   
library(ggrepel)
library(AER)

# LOAD DATA ####

# Source: HUD, "2007- 2025 Point-in-Time Estimates by State" (.xlsb).
# downloaded from the HUD User AHAR page.
# one-time manual step: opened the .xlsb in Excel (macros disabled)
# and saved the "2025" sheet as CSV. Only that sheet is used here.

pit_raw <- read_csv("Data/2025-PIT-Counts-by-State.csv")

# CLEAN DATA & SAMPLE ####

  # The raw file has ~ 1,000 columns; keep only these five.
  # "us" = unsheltered, "tot" = total (sheltered + unsheltered)
  #     youth_us = all unaccompanied youth under 25 counted unsheltered (response)
  #     youth_tot = all unaccompanied youth under 25 counted
  #                  (Exposure, enters model as offset)
  #     all_us = everyone counted unsheltered (used to build the predictor)
  #     all_tot = everyone counted (used to build the predictor)

pit <- pit_raw |>
  transmute(
    state     = State,
    youth_us  = `Unsheltered Homeless Unaccompanied Youth (Under 25)`,
    youth_tot = `Overall Homeless Unaccompanied Youth (Under 25)`,
    all_us    = `Unsheltered Homeless`,
    all_tot   = `Overall Homeless`
  )
str(pit)

# Rows to review for exclusion ####
nrow(pit)                        
pit |> filter(state == "Total")   
pit |> filter(state %in% c("AS", "GU", "MP", "PR", "VI", "DC"))
pit |> filter(youth_tot == 0 | is.na(youth_tot))

  # Rows removed and why (each documented for the data section):
  # "Total": national sum of all rows, not a state; including it
  # would double-count every youth and create an extreme outlier
  # Blank rows (state = NA): empty rows created when the .xlsb
  # sheet was saved as CSV; not data
  # AS, MP: no PIT data reported (all values missing)
  # GU, PR, VI: excluded; the research question concerns states,
  # and territories differ in geography, housing context, and count
  # administration; youth counts are also very small (5, 45, 6)
  # DC: retained; HUD reports DC as a state-level jurisdiction,
  # though it is entirely urban (noted as a limitation)

# Exclusions 
  pit_model <- pit |>
  filter(
    !is.na(state),                        # drops blank export rows
    state != "Total",                     # drops the national total
    !is.na(youth_tot),                    # drops AS and MP (no data)
    youth_tot > 0,                        # safeguard: log(0) undefined for offset
    !state %in% c("GU", "PR", "VI")       # drops remaining territories
  )

nrow(pit_model)   # should be 51: 50 states + DC

# BUILD VARIABLES ####

# pct_us_other: % of the REST of the homeless population that is
#   unsheltered (everyone except unaccompanied youth). Youth are
#   removed so the outcome is not built into its own predictor.
#   Entered in its original units ( 1 percentage point).
# youth_share: share of counted youth who are unsheltered.
#   DESCRIPTIVE ONLY: used for exploration and plots, NOT the model
#   outcome (the model uses the count youth_us with youth_tot as offset)

pit_model <- pit_model |>
  mutate(
    pct_us_other = 100 * (all_us - youth_us) / (all_tot - youth_tot),
    youth_share  = 100 * youth_us / youth_tot
  )

# EXPLORATION ####

desc <- pit_model |>
  summarise(across(c(youth_us, youth_tot, pct_us_other, youth_share),
                   list(mean = mean, sd = sd, median = median,
                        min = min, max = max))) |>
  tidyr::pivot_longer(everything(),
                      names_to = c("variable", ".value"),
                      names_pattern = "(.*)_(mean|sd|median|min|max)")
desc |> mutate(across(where(is.numeric), \(x) round(x, 1)))

  # Results: n=51
  #   youth_us: mean 227, sd 664, median 63, range 4-4,583
  #   youth_tot: mean688, sd 1,386, median 256, range 51-8086
  #   pct_us_other: mean 30.0, sd16.6, medain 26.4, range 4.0-63.8
  #   youth_share:  mean 27.4, SD 16.4, median 24.6, range 4.7-68.6
  #   Both counts strongly right-skewed (mean >> median, SD > mean).
  #   A few larges states hold most youth, so counts need an offset.

# Raw mean vs. variance of the response

#     rough first look only: the real overdispersion check comes
#     after fitting the model, because the offset matters)

mean(pit_model$youth_us)  # 227
var(pit_model$youth_us)   # ~440,900

# Correlation, youth share vs predictor

cor(pit_model$pct_us_other, pit_model$youth_share)                      # Pearson
cor(pit_model$pct_us_other, pit_model$youth_share, method = "spearman") # rank-based

  # Results:
  #   Correlation, youth share vs predictor: Pearson r = .86,
  #   Spearman rho = .88. Similar values: relationship not driven by
  #   a few extreme states. Unweighted (each state counts equally). 

# States at the extremes

pit_model |> arrange(desc(youth_tot)) |> select(state, youth_tot, youth_share) |> head(5)
pit_model |> arrange(desc(pct_us_other)) |> select(state, pct_us_other, youth_share) |> head(5)

  # Notes:
  #   CA, OR high or predictor and share; NY large but very low share (4.9%) 

# Three groups: youth share above, near, or below the rest-of-population
# share. "Near" = difference within +/- 1.96 standard errors of the
# youth share, given that state's number of counted youth (binomial SE).
# Rest-of-population % treated as fixed (descriptive grouping only).
pit_model <- pit_model |>
  mutate(
    p_youth  = youth_share / 100,
    se_share = 100 * sqrt(p_youth * (1 - p_youth) / youth_tot),
    diff     = youth_share - pct_us_other,
    youth_vs_rest = case_when(
      diff >  1.96 * se_share ~ "Youth higher",
      diff < -1.96 * se_share ~ "Youth lower",
      TRUE                    ~ "Similar"
    ),
    youth_vs_rest = factor(youth_vs_rest,
                           levels = c("Youth higher", "Similar", "Youth lower"))
  )
table(pit_model$youth_vs_rest)   # report these counts in the text

ggplot(pit_model, aes(x = pct_us_other, y = youth_share)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(aes(size = youth_tot, fill = youth_vs_rest), shape = 21,
             color = "gray30", alpha = 0.85, stroke = 0.4) +
  geom_text_repel(aes(label = state), size = 2.5, max.overlaps = 20) +
  scale_size_area(max_size = 10, name = "Youth counted") +
  scale_fill_manual(values = c("Youth higher" = "#00441b",
                               "Similar"      = "#d9d9d9",
                               "Youth lower"  = "#74c476"),
                    name = "Youth share vs.\nrest of population") +
  labs(x = "% of rest of homeless population unsheltered",
       y = "% of unaccompanied youth unsheltered") +
  theme_minimal()

# BASELINE POISSON MODEL ####

# Response: youth_us (count of unsheltered unaccompanied youth)
# Predictor: pct_us_other (Per 1 percentage point)
# Offset: log(youth_tot): coefficient fixed at 1, so the model
#         describes the RATE of unsheltered youth per counted youth
m_pois <- glm(youth_us ~ pct_us_other + offset(log(youth_tot)),
              family = poisson, data = pit_model)
summary(m_pois)

deviance(m_pois) / df.residual(m_pois)  # deviance ratio; ~1 if Poisson fits
exp(coef(m_pois))                       # rate ratios
range(pit_model$pct_us_other)           # intercept (0%) is outside data range

  # Results:
  #   b = 0.033 (SE 0.0005), RR = 1.033.
  #   Residual deviance 668.44 on 49 df (ration ~13.6. AIC  = 984.26
  #   Not interpreted: Poisson fixes dispersion at 1, so SEs may be understated.
  #   Intercept corresponds to 0%, outside observed range.

# BASELINE DIAGNOSTICS ####

# Offset check: does the data support fixing the coefficient at 1?
#     Fit log(youth_tot) as a regular predictor instead. If its
#     estimate is close to 1, the offset is supported.
m_offcheck <- glm(youth_us ~ pct_us_other + log(youth_tot),
                  family = poisson, data = pit_model)
coef(m_offcheck)["log(youth_tot)"]
confint(m_offcheck)["log(youth_tot)", ]

# Overdispersion: Pearson chi-square / residual df
#     Poisson assumes this ratio is about 1.
#     Much larger than 1 = states vary more than Poisson allows,
#     so its standard errors are too small.
sum(residuals(m_pois, type = "pearson")^2) / df.residual(m_pois)

# Formal test of overdispersion
#     H0: variance = mean (Poisson holds)
dispersiontest(m_pois)

#### Did not add (Cooks or residual plot) to the report for the sake of saving 
# space and redundancy

# Influence: which states pull hardest on the estimates?
#     Common rule of thumb: Cook's D > 4/n flags a state for a look
pit_model$cooks <- cooks.distance(m_pois)
pit_model |>
  arrange(desc(cooks)) |>
  select(state, youth_tot, youth_share, pct_us_other, cooks) |>
  head(6)
4 / nrow(pit_model)   # the cutoff

# Residual plot: Pearson residuals vs. fitted values
plot(fitted(m_pois), residuals(m_pois, type = "pearson"),
     xlab = "Fitted values", ylab = "Pearson residuals",
     main = "Poisson model: Pearson residuals vs. fitted")
abline(h = 0, lty = 2)

# NEGATIVE BINOMIAL MODEL ####

m_nb <- MASS::glm.nb(youth_us ~ pct_us_other + offset(log(youth_tot)),
                     data = pit_model)
summary(m_nb)
  #   Model comparison: AIC (lower)
AIC(m_pois, m_nb)
  # Results:
  # M_pois 984.26
  # m_nb    481.40
