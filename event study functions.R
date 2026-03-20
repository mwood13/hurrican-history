# make event study data creation, and regession results tables

# filter the data fror the event study
filter_data <- function(data, i){
  df <- data[i,]
  week <- (as.Date(df$week_end))
  fip_now <- df$fips[1]
  county <- data %>% filter(data$fips == fip_now)
  result <- county %>% filter(week_end >= (week-70) & week_end <= (week+70))
  result <- result %>% mutate(
    ref_num = round((as.Date(week_end) - week)/7,0)
  )
  
  return(result)
}


# function to create event study data frame based on the variable of interest and the bound for the variable of interest
get_event_data <- function(data, var_int, bound, wind_range){
  # initializae interation length
  L <- nrow(data)
  
  # initialize output data frame
  event_df <- data[0, ]
  
  if(var_int == "none"){
    
    for(i in 1:L){
      if(data$Landfall[i] == 1 & 
        data$Wind[i] >= wind_range[1] & 
        data$Wind[i] < wind_range[2]){
        result <- filter_data(data, i)
      }else{next}
      
    event_df <- rbind(event_df, result)
  }}
  # fill in data frame
  
  if(var_int == "total"){
    
    for(i in 1:L){
      if(data$Landfall[i] == 1 &
        (data$total_hist_landfall[i]> bound[1] & 
        data$total_hist_landfall[i]<= bound[2]) & 
        (data$Wind[i] >= wind_range[1] & 
        data$Wind[i] < wind_range[2])){
        result <- filter_data(data, i)
      }else{next}
      
    event_df <- rbind(event_df, result)
  }}
  
  if(var_int == "years"){
    
    for(i in 1:L){
      
      if(data$Landfall[i] == 1 &
         (data$years_since_landfall_hur[i] > bound[1] &
          data$years_since_landfall_hur[i]<= bound[2]) &
         (data$Wind[i] >= wind_range[1] &
          data$Wind[i] < wind_range[2])){
        result <- filter_data(data, i)
      }else{next}
      
      event_df <- rbind(event_df, result, fill=TRUE)
    }}
  
 
  
  # add in reference number
  event_df$ref_num = relevel(as.factor(event_df$ref_num), ref = "-2")
  
  return(event_df)
}


# compile event study results into a table
make_ES_table <- function(model, id){
  results= tidy(model)   
  test <- confint(model, level =0.95)
  
  #get 95% confidence intervals
  ES_results = results %>% mutate(
    lci = confint(model, level = 0.95)$'2.5 %',
    uci = confint(model, level = 0.95)$'97.5 %',
    time = as.integer(str_sub(term, start = 8)),
    id = as.character(id)
  )
  
  # add reference point to the data
  ref_point <- results [1,]
  ref_point <- ref_point %>% mutate(
    term = "ref_num-2",
    estimate = 0,
    std.error=0,
    time = -2,
    lci = 0,
    uci = 0,
    id = as.character(id)
  )
  
  ES_results <- rbind(ES_results, ref_point)
}