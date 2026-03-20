# regressions

# load in packages

library(pacman)

p_load(tidyverse, stringr, viridis, reshape2, jtools, tmap, RColorBrewer,
       data.table, dtplyr, lubridate, plm, estimatr,
       fixest, huxtable, stargazer)

# Bring in data ----------------------------------------------------

scanner <- fread("Data\\scanner_with_hur.csv")


scanner <- scanner |> mutate(
  fips = str_pad(fips, 5, "left", "0"),
  year = str_sub(week_end, end = 4),
  month = str_sub(week_end, start = 6, end = 7),
  Hur_Landfall = ifelse(Landfall == 1 & Wind >= 64, 1, 0),
  next_wind = lag(Wind, -1L),
  Hur_Threat = ifelse(Threat == 1 & (Wind >= 64 | next_wind >= 64), 1, 0)
)

scanner <- scanner[order(scanner$fips, scanner$week_end), ]

scanner <- subset(scanner, fips_state_code != "51")

# run regressions for hurricane effect ---------------------------------------

# base regression
current_lm <- feols(data = scanner,
                    log(total_rev_per_cap) ~ Threat + Landfall +
                      temp_mean |
                      year + month + fips,
                    cluster = c("fips", "year"),
                    mem.clean = TRUE)

summary(current_lm)

# base regression, but only looking at hurricanes
current_hur_lm <- feols(data = scanner,
                        log(total_rev_per_cap) ~ Hur_Threat + Hur_Landfall +
                          temp_mean |
                          year + month + fips,
                        cluster = c("fips", "year"),
                        mem.clean = TRUE)

summary(current_hur_lm)


esttex(current_lm, current_hur_lm,
       title = "Base Results",
       fitstat = ~n + r2)

# historical count regression ------------------------------------------------

# get landfall count for each hurricane before 2008
sub_df <- scanner |>
  group_by(fips, year) |>
  summarise(
    total_landfall = sum(Landfall),
    total_hist_landfall = head(total_hist_landfall, 1L)
  ) |>
  ungroup()

old_county <- sub_df$fips[200]

# update variable at the end of each year to include new hurricanes
for (i in seq_along(sub_df$fips)) {
  new_county <- sub_df$fips[i]
  if (old_county == new_county) {
    last_yr = sub_df$total_landfall[i - 1]
    sub_df$total_hist_landfall[i] = last_yr + sub_df$total_hist_landfall[i - 1]
    old_county = new_county
  }else {
    sub_df$total_hist_landfall[i] = sub_df$total_hist_landfall[i]
    old_county = new_county
  }
}


scanner <- scanner |> rename("past_hist_landfall" = "total_hist_landfall")

scanner <- left_join(scanner, sub_df, by = c("fips", "year"))


# regressions with historical landfall count, but only looking at hurricanes
hist_hur_lm1 <- feols(data = scanner,
                      log(total_rev_per_cap) ~ Hur_Threat + Hur_Landfall +
                        temp_mean + total_hist_landfall |
                        year + month + fips,
                      cluster = c("fips", "year"),
                      mem.clean = TRUE)

summary(hist_hur_lm1)


#regression with historical landfall count and interaction,
# but only looking at hurricanes
hist_hur_lm2 <- feols(data = scanner,
                      log(total_rev_per_cap) ~ Hur_Threat + Hur_Landfall +
                        temp_mean + total_hist_landfall +
                        Hur_Threat:total_hist_landfall +
                        Hur_Landfall:total_hist_landfall |
                        year + month + fips,
                      cluster = c("fips", "year"),
                      mem.clean = TRUE)

summary(hist_hur_lm2)


esttex(current_hur_lm, hist_hur_lm1 ,hist_hur_lm2,
       title = "Historical Exposure Results",
       fitstat = ~n + r2)

# Discounting effect -----------------------------------------------------------

# regressions with years since last landfall
disc_lm1 <- feols(data = scanner,
                  log(total_rev_per_cap) ~ Hur_Threat + Hur_Landfall +
                    temp_mean + total_hist_landfall +
                    years_since_landfall_hur |
                    year + month + fips,
                  cluster = c("fips", "year"),
                  mem.clean = TRUE)

summary(disc_lm1)

# regression with years since last landfall and interaction
disc_lm2 <- feols(data = scanner,
                  log(total_rev_per_cap) ~ Hur_Threat + Hur_Landfall +
                    temp_mean + total_hist_landfall +
                    years_since_landfall_hur +
                    Hur_Threat:years_since_landfall_hur +
                    Hur_Landfall:years_since_landfall_hur |
                    year + month + fips,
                  cluster = c("fips", "year"),
                  mem.clean = TRUE)

summary(disc_lm2)

# current hurricane squared

# create squared variable for years since last landfall
scanner <- scanner |> mutate(
  yrs_since_hur_sq = years_since_landfall_hur^2
)

# other functional forms
disc_lm3 <- feols(data = scanner,
                      log(total_rev_per_cap) ~  Hur_Threat+
                        Hur_Landfall +
                        temp_mean + total_hist_landfall +
                        years_since_landfall_hur +
                        yrs_since_hur_sq |
                        year + month + fips,
                      cluster = c("fips", "year"),
                      mem.clean = TRUE)

summary(disc_lm3)

disc_lm4 <- feols(data = scanner,
                  log(total_rev_per_cap) ~  Hur_Threat+
                    Hur_Landfall +
                    temp_mean + total_hist_landfall +
                    years_since_landfall_hur +
                    yrs_since_hur_sq +
                    Hur_Landfall:years_since_landfall_hur +
                    Hur_Landfall:yrs_since_hur_sq|
                    year + month + fips,
                  cluster = c("fips", "year"),
                  mem.clean = TRUE)

summary(disc_lm4)


disc_lm5 <- feols(data = scanner,
                  log(total_rev_per_cap) ~  Hur_Threat+
                    Hur_Landfall +
                    temp_mean + total_hist_landfall +
                    years_since_landfall_hur +
                    yrs_since_hur_sq +
                    Hur_Threat:years_since_landfall_hur +
                    Hur_Threat:yrs_since_hur_sq|
                    year + month + fips,
                  cluster = c("fips", "year"),
                  mem.clean = TRUE)

summary(disc_lm5)


disc_lm6 <- feols(data = scanner,
                  log(total_rev_per_cap) ~  Hur_Threat+
                    Hur_Landfall +
                    temp_mean + total_hist_landfall +
                    years_since_landfall_hur +
                    yrs_since_hur_sq +
                    Hur_Threat:years_since_landfall_hur +
                    Hur_Threat:yrs_since_hur_sq +
                    Hur_Landfall:years_since_landfall_hur +
                    Hur_Landfall:yrs_since_hur_sq|
                    year + month + fips,
                  cluster = c("fips", "year"),
                  mem.clean = TRUE)



summary(disc_lm6)


disc_lm7 <- feols(data = scanner,
                  log(total_rev_per_cap) ~ Hur_Threat + Hur_Landfall +
                    temp_mean + total_hist_landfall +
                    years_since_landfall_hur +
                    yrs_since_hur_sq +
                    Hur_Threat:years_since_landfall_hur +
                    Hur_Landfall:years_since_landfall_hur |
                    year + month + fips,
                  cluster = c("fips", "year"),
                  mem.clean = TRUE)

summary(disc_lm7)


esttex(current_hur_lm, disc_lm1 ,disc_lm2,disc_lm7, disc_lm5, disc_lm6,
       title = "Recent Exposure Results",
       fitstat = ~n + r2)


esttex(current_hur_lm, disc_lm1 ,disc_lm2,
       title = "Recent Exposure Results",
       fitstat = ~n + r2)