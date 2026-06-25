library(data.table)
library(lubridate)
library(tidyverse)

# -------------------------------------------------
# 1. LOAD SCANNER DATA
# -------------------------------------------------
scanner <- fread("Data/water_scanner.csv")

scanner[, fips := ifelse(fips == 12025, 12086, fips)]
scanner[, fips := sprintf("%05d", as.integer(fips))]

scanner[, week_end := as.Date(
  gsub("(\\d{4})(\\d{2})(\\d{2})$", "\\1-\\2-\\3", week_end)
)]

setkey(scanner, fips, week_end)

scanner[, year := as.integer(format(week_end, "%Y"))]


# -------------------------------------------------
# 2. CURRENT HURRICANES
# -------------------------------------------------
hur <- fread("Data/weekly_level_frcst.csv")

hur[, `:=`(
  fips = sprintf("%05d", as.integer(fips))
)]


hur[, event := as.integer(inside_cone == 1 | landfall == 1)]


setkey(hur, fips, week_end)

scanner[hur, `:=`(
  landfall = i.landfall,
  wind = i.max_sust_wind,
  threat = i.inside_cone,
  event = i.event
)]

scanner[is.na(landfall), `:=`(
  landfall = 0,
  wind = 0,
  threat = 0,
  event = 0
)]


# -------------------------------------------------
# 3. LOAD HISTORICAL PANEL (FROM SCRIPT 1)
# -------------------------------------------------

hist_rec <- fread("Data/Past_Hurricanes.csv")

hist_rec[, fips := sprintf("%05d", as.integer(fips))]
setnames(hist_rec, "Year", "year")

# Split into yearly panel + recency info
hist_panel <- hist_rec[, .(fips, year, all, h1_5, h3_5)]

hist_recency <- unique(
  hist_rec[, .(fips, recent_all, recent_h1_5, recent_h3_5,
               Last_Wind_all, Last_Wind_h1_5, Last_Wind_h3_5)]
)

# Merge recency variables (by fips only)
setkey(scanner, fips)
setkey(hist_recency, fips)

scanner[hist_recency, `:=`(
  recent_all = i.recent_all,
  recent_h1_5 = i.recent_h1_5,
  recent_h3_5 = i.recent_h3_5,
  Last_Wind_all = i.Last_Wind_all,
  Last_Wind_h1_5 = i.Last_Wind_h1_5,
  Last_Wind_h3_5 = i.Last_Wind_h3_5
)]


# -------------------------------------------------
# 3.5 TRUE ROLLING 25-YEAR EXPOSURE
# -------------------------------------------------

# Current yearly hurricane counts
hur <- hur[, `:=`(
  year = year(week_end)
)]

current_panel <- hur[, .(
  all = sum(landfall),
  h1_5 = sum(max_sust_wind >= 64),
  h3_5 = sum(max_sust_wind >= 96)
), by = .(fips, year)]


# Combine pre-2008 + post-2008
full_panel <- rbindlist(list(hist_panel, current_panel), fill = TRUE)

full_panel[is.na(all), `:=`(all = 0, h1_5 = 0, h3_5 = 0)]

setorder(full_panel, fips, year)

# Rolling 25-year window (exclude current year)
full_panel[, `:=`(
  past_25_all  = shift(frollsum(all, 25, align = "right"), 1),
  past_25_h1_5 = shift(frollsum(h1_5, 25, align = "right"), 1),
  past_25_h3_5 = shift(frollsum(h3_5, 25, align = "right"), 1)
), by = fips]

# Merge into scanner
setkey(scanner, fips, year)
setkey(full_panel, fips, year)

scanner[full_panel, `:=`(
  past_25_all  = i.past_25_all,
  past_25_h1_5 = i.past_25_h1_5,
  past_25_h3_5 = i.past_25_h3_5
)]

scanner[, `:=`(
  base_25_all  = head(past_25_all, 1L),
  base_25_h1_5 = head(past_25_h1_5, 1L),
  base_25_h3_5 = head(past_25_h3_5, 1L)
), by = .(fips)]


# -------------------------------------------------
# 4. RECENCY 
# -------------------------------------------------
setorder(scanner, fips, week_end)

# specifically builds years since last hurricane. Not just any storm

scanner[, last_landfall_date :=
          nafill(
            shift(fifelse(landfall == 1 & wind >= 64, week_end, NA_Date_), 1),
            "locf"
          ),
        by = fips
]

scanner[, last_wind :=
          nafill(
            shift(fifelse(landfall == 1 & wind >= 64, wind, NA_real_), 1),
            "locf"
          ),
        by = fips
]

scanner[is.na(last_wind), last_wind := Last_Wind_h1_5]
scanner[is.na(last_landfall_date), last_landfall_date := recent_h1_5]

scanner[, days_since_last :=
          as.numeric(week_end - last_landfall_date)
]

scanner[, `:=`(
  weeks_since_landfall = days_since_last / 7,
  years_since_landfall = days_since_last / 365
)]


# -------------------------------------------------
# 5. DEMOGRAPHICS
# -------------------------------------------------

# WEATHER
weather <- fread("Data/weekly_weather.csv")

weather[, fips := sprintf("%05d", as.integer(fips))]
weather[, week_end := as.Date(week_end)]

weather <- weather[, .SD, .SDcols = c(1,2,6)]

setkey(weather, fips, week_end)

weather_cols <- setdiff(names(weather), c("fips","week_end"))

scanner[weather,
        (weather_cols) := mget(paste0("i.", weather_cols)),
        on = .(fips, week_end)]

# INCOME
inc <- fread("Data/rep_income.csv")
inc[, fips := sprintf("%05d", as.integer(fips))]
setkey(inc, fips)

inc_cols <- setdiff(names(inc), "fips")
scanner[inc, (inc_cols) := mget(paste0("i.", inc_cols)), on = "fips"]

# RACE
race <- fread("Data/rep_race.csv")
race[, fips := sprintf("%05d", as.integer(fips))]
setkey(race, fips)

race_cols <- setdiff(names(race), "fips")
scanner[race, (race_cols) := mget(paste0("i.", race_cols)), on = "fips"]

# POPULATION
pop10 <- fread("Data/co-est2010.csv")
pop20 <- fread("Data/co-est2020.csv")

pop10[, state := sprintf("%02d", as.integer(STATE))]
pop10[, county := sprintf("%03d", as.integer(COUNTY))]
pop10[, fips := paste0(state, county)]
pop10[, fips := sprintf("%05d", as.integer(fips))]


pop10 <- melt(
  pop10, 
  measure.vars =  patterns(population = "^POP"),
  value.name = "population",
  variable.name = "year"
)

pop10[, year := as.integer(substr(year, 12, 15))]

pop10<-pop10[, c("fips", "year", "population")]


pop20[, state := sprintf("%02d", as.integer(STATE))]
pop20[, county := sprintf("%03d", as.integer(COUNTY))]
pop20[, fips := paste0(state, county)]
pop20[, fips := sprintf("%05d", as.integer(fips))]


pop20 <- melt(
  pop20, 
  measure.vars =  patterns(population = "^POP"),
  value.name = "population",
  variable.name = "year"
)

pop20[, year := as.integer(substr(year, 12, 15))]
pop20<-pop20[, c("fips", "year", "population")]

pop <- rbind(pop10, pop20)


setkey(pop, fips, year)

pop_cols <- setdiff(names(pop), c("fips", "year"))
scanner[pop, (pop_cols) := mget(paste0("i.", pop_cols)), on = c("fips","year")]


# -------------------------------------------------
# 6. FINAL VARIABLES
# -------------------------------------------------
scanner[, total_rev_per_cap :=
          total_rev / population * 100000]

scanner[, total_vol_per_cap :=
          total_vol /population * 100000]

scanner[, major_hur := as.integer(wind >= 96)]


# -------------------------------------------------
# 7. SAVE
# -------------------------------------------------


fwrite(scanner, "Data/combined_scanner.csv")

