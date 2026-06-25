library(data.table)
library(sf)
library(tigris)

# 1. LOAD + CLEAN HURDAT -------------------------------------------------------
HURDAT <- fread("Data//HURDAT2.csv")

# filter for 30 years before data starts 
# filter for landfalls
HURDAT <- HURDAT[
  Year >= 1978 & Year < 2008 & Record_identifier == "L"
]

# format date correctly
HURDAT[, date := as.Date(sprintf("%d-%02d-%02d", Year, Month, Day))]

# Indicate classification of storms. 
HURDAT[, `:=`(
  all = 1,
  h1_5 = as.integer(Maximum_sustained_wind_in_knots >= 64), # Cat 1 to 5 
  h3_5 = as.integer(Maximum_sustained_wind_in_knots >= 96) # Cat 3 to 5
)]


# 2. PULL IN COUNTIES ----------------------------------------------------------
us_counties <- counties(
  state = c('01','12','13','22','28','37','45','48'),
  year = 2010,
  class = "sf"
)

# fix fips code to match hurricane data
us_counties$fips <- paste0(us_counties$STATEFP10, us_counties$COUNTYFP10)


# 3. CONVERT TO SF + BUFFER ----------------------------------------------------
HURDAT_sf <- st_as_sf(HURDAT, coords = c("Longitude", "Latitude"), crs = 4269)

# Project for correct buffering
HURDAT_sf <- st_transform(HURDAT_sf, 5070)
us_counties <- st_transform(us_counties, 5070)

# Add 100 nautical mile buffer
HURDAT_buf <- st_buffer(HURDAT_sf, dist = 185200)

# 4. GET INTERSECTIONS OF BUFFERS AND COUNTIES ---------------------------------
intersections <- st_intersects(HURDAT_buf, us_counties)

# converst list of intersections into data table
dt_pairs <- data.table(
  storm_id = rep(seq_along(intersections), lengths(intersections)),
  county_id = unlist(intersections)
)


# add in information about hurricanes and counties to intersection table
dt_pairs[, `:=`(
  fips = us_counties$fips[county_id],
  Date = HURDAT$date[storm_id],
  Year = HURDAT$Year[storm_id],
  all = HURDAT$all[storm_id],
  h1_5 = HURDAT$h1_5[storm_id],
  h3_5 = HURDAT$h3_5[storm_id]
)]

# Remove duplicates (same county-date storms)
dt_pairs <- unique(dt_pairs, by = c("fips", "Date"))


# 5. COLLAPSE TO YEARLY --------------------------------------------------------
years <- 1983:2007 # Note:: First hurricane is 1983
panel <- CJ(fips = us_counties$fips, Year = years)

dt_year <- dt_pairs[, .(
  all = sum(all),
  h1_5 = sum(h1_5),
  h3_5 = sum(h3_5)
), by = .(fips, Year)]

panel <- dt_year[panel, on = .(fips, Year)] # convert to panel

panel[is.na(all), `:=`(all = 0, h1_5 = 0, h3_5 = 0)] # replace missing values with 0

setorder(panel, fips, Year) # order panel correctly


# 6. RECENCY VARIABLES ---------------------------------------------------------

# Step 1: Recency 
recency_dt <- dt_pairs[, .(
  recent_all  = max(Date),
  recent_h1_5 = if (any(h1_5 == 1)) max(Date[h1_5 == 1]) else as.Date(NA),
  recent_h3_5 = if (any(h3_5 == 1)) max(Date[h3_5 == 1]) else as.Date(NA)
), by = fips]

# Step 2: UNIQUE wind lookup
wind_lookup <- HURDAT[, .(
  max_wind = max(Maximum_sustained_wind_in_knots, na.rm = TRUE)
), by = date]

setkey(wind_lookup, date)


# Initialize columns
recency_dt[, `:=`(
  Last_Wind_all = NA_real_,
  Last_Wind_h1_5 = NA_real_,
  Last_Wind_h3_5 = NA_real_
)]

# Update joins
recency_dt[wind_lookup, Last_Wind_all := i.max_wind, on = .(recent_all = date)]
recency_dt[wind_lookup, Last_Wind_h1_5 := i.max_wind, on = .(recent_h1_5 = date)]
recency_dt[wind_lookup, Last_Wind_h3_5 := i.max_wind, on = .(recent_h3_5 = date)]


# Merge all data into one file
final_dt <- merge(panel, recency_dt, by = "fips", all = TRUE)

# save
fwrite(final_dt, "Data/Past_Hurricanes.csv")
