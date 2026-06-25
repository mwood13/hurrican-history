# Functions used in Event studies

library(data.table)


# FILTER windOW AROUND EVENT

filter_data <- function(data, event_row) {
  
  fips_now <- event_row$fips
  event_week <- event_row$week_end
  
  county_data <- data[fips == fips_now]
  
  result <- county_data[
    week_end >= (event_week - 70) & week_end <= (event_week + 70)
  ]
  
  result[, ref_num :=
           as.integer(round((week_end - event_week) / 7, 0))]
  
  return(result)
}


# BUILD EVENT DATA

get_event_data <- function(data, var_int, bound, wind_range) {
  
  data <- as.data.table(data)
  
  
  # 1. IDENTIFY VALID EVENT ROWS
  
  
  if (var_int == "none") {
    
    events <- data[
     landfall == 1 &
        wind >= wind_range[1] &
        wind < wind_range[2]
    ]
    
  } else if (var_int == "total") {
    
    events <- data[
     landfall == 1 &
        base_25_h1_5 > bound[1] &
        base_25_h1_5 <= bound[2] &
        wind >= wind_range[1] &
        wind < wind_range[2]
    ]
    
  } else if (var_int == "years") {
    
    events <- data[
     landfall == 1 &
        years_since_landfall > bound[1] &
        years_since_landfall <= bound[2] &
        wind >= wind_range[1] &
        wind < wind_range[2]
    ]
  }
  
  
  # 2. PREPARE FOR JOIN
  
  data[, week_num := as.integer(week_end)]
  events[, event_week := week_end]
  events[, event_week_num := as.integer(event_week)]
  
  setkey(data, fips)
  setkey(events, fips)
  
  
  # 3. SELF-JOIN WITH windOW
  
  event_df <- data[events,
                   on = .(fips),
                   allow.cartesian = TRUE
  ][
    week_num >= event_week_num - 70 &
      week_num <= event_week_num + 70
  ]
  
  
  # 4. CREATE RELATIVE TIME
  
  event_df[, ref_num :=
             as.integer(round((week_num - event_week_num) / 7))]
  
  
  # 5. CLEANUP + FACTOR
  
  event_df[, ref_num := factor(ref_num)]
  event_df[, ref_num := relevel(ref_num, ref = "-2")]
  
  # Optional cleanup
  event_df[, c("week_num", "event_week", "event_week_num") := NULL]
  
  return(event_df)
}



# EVENT STUDY TABLE

make_ES_table <- function(model, id) {
  
  # Extract coefficients
  coefs <- coef(model)
  
  # Extract confidence intervals
  ci <- confint(model)
  
  # Convert to data.table
  dt <- data.table(
    term = names(coefs),
    estimate = as.numeric(coefs),
    lci = ci[, 1],
    uci = ci[, 2],
    id = as.character(id)
  )
  
  # Extract event-time terms only (ref_num)
  dt <- dt[grepl("^ref_num", term)]
  
  # Extract time from term name
  dt[, time := as.integer(sub("ref_num", "", term))]
  
  # Add reference point explicitly
  ref_point <- data.table(
    term = "ref_num-2",
    estimate = 0,
    lci = 0,
    uci = 0,
    time = -2,
    id = as.character(id)
  )
  
  # Combine safely
  dt <- rbindlist(list(dt, ref_point), use.names = TRUE, fill = TRUE)
  
  return(dt)
}