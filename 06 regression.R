# Run Regressions

library(data.table)
library(fixest)
library(lubridate)

# load in data
scanner <- fread("Data/combined_scanner.csv")

setDT(scanner)

scanner[, fips := sprintf("%05d", as.integer(fips))]
scanner[, `:=`(
  year = year(week_end),
  month = month(week_end)
)]

setorder(scanner, fips, week_end)

# Drop Virginia
scanner <- scanner[substr(fips, 1, 2) != "51"]


# Get variables for hurricane threats and landfalls
scanner[, `:=`(
  hur_landfall = as.integer(landfall == 1 & wind >= 64),
  hur_threat   = as.integer(threat == 1 & wind >= 64)
)]

scanner[, `:=`(
  event = as.integer(hur_threat == 1 | hur_landfall == 1)
)]


# build regression function
run_feols <- function(rhs) {
  feols(
    as.formula(paste0(
      "log(total_rev_per_cap) ~ ", rhs,
      " | fips + year + month"
    )),
    data = scanner,
    cluster = ~ fips + year,
    mem.clean = TRUE
  )
}



# Run base/replication model
base_models <- list(
  baseline   = run_feols("threat + landfall + temp_mean"),
  hur_only   = run_feols("hur_threat + hur_landfall + temp_mean"),
  event_only = run_feols("event + temp_mean")
)

lapply(base_models, summary)

esttex(base_models$baseline,
       base_models$hur_only,
       base_models$event_only,
       title = "Base Results",
       fitstat = ~n + r2)


# Test historical exposure
hist_models <- list(
  
  event_no_interaction = run_feols(
    "event + temp_mean + past_25_h1_5"
  ),
  
  event_with_interaction = run_feols(
    "event + temp_mean + past_25_h1_5 +
     event:past_25_h1_5"
  )
)

lapply(hist_models, summary)

esttex(base_models$event_only,
       hist_models$event_no_interaction,
       hist_models$event_with_interaction,
       title = "Historical Exposure Results",
       fitstat = ~n + r2)


# Test recency

scanner[, yrs_since_hur_sq := years_since_landfall^2]

disc_models <- list(
  
  base = run_feols(
    "event + temp_mean +
     past_25_h1_5 + years_since_landfall"
  ),
  
  interaction = run_feols(
    "event + temp_mean +
     past_25_h1_5 + years_since_landfall +
     event:years_since_landfall"
  ),
  
  quadratic = run_feols(
    "event + temp_mean +
     past_25_h1_5 + years_since_landfall + yrs_since_hur_sq"
  ),
  
  full = run_feols(
    "event + temp_mean +
     past_25_h1_5 + years_since_landfall + yrs_since_hur_sq +
     event:years_since_landfall +
     event:yrs_since_hur_sq"
  )
)

lapply(disc_models, summary)

esttex(base_models$event_only,
       disc_models$base,
       disc_models$interaction,
       disc_models$full,
       title = "Recent Exposure Results",
       fitstat = ~n + r2)



event_disc_models <- list(
  
  base = run_feols(
    "event + temp_mean + past_25_h1_5 +
     years_since_landfall"
  ),
  
  interaction = run_feols(
    "event + temp_mean + past_25_h1_5 +
     years_since_landfall +
     event:years_since_landfall"
  ),
  
  full = run_feols(
    "event + temp_mean + past_25_h1_5 +
     years_since_landfall +
     event:years_since_landfall +
     event:past_25_h1_5 +
     past_25_h1_5:years_since_landfall +
     event:past_25_h1_5:years_since_landfall"
  )
)

lapply(event_disc_models, summary)

esttex(base_models$event_only,
       event_disc_models$base,
       event_disc_models$interaction,
       event_disc_models$full,
       title = "Recent Exposure Results",
       fitstat = ~n + r2)
