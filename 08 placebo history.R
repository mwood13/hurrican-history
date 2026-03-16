# robustness, random landfall date for hur count event study and regression. 

# load in packages

library(pacman)

p_load(tidyverse, stringr, viridis, reshape2, jtools,tmap,RColorBrewer,
       data.table, dtplyr, lubridate, plm, estimatr, fixest)

# load in data ----------------------------------------------------

# load data
scanner <- read.csv('Data\\scanner_with_hur.csv')

# adjust date and county formats
scanner <- scanner %>% mutate(
  fips = str_pad(fips, 5,'left', '0'),
  year = str_sub(week_end, end = 4),
  month = str_sub(week_end, start =6, end = 7)
) 

scanner<- scanner[order(scanner$fips, scanner$week_end),]

scanner <- subset(scanner, fips_state_code != '51')

# subset data by county and year, then calculate total landfalls and cumulative landfalls for each county-year
sub_df <- scanner %>% group_by(fips, year) %>% summarise(
  total_landfall = sum(Landfall),
  total_hist_landfall = head(total_hist_landfall, 1L)
)%>%ungroup()

old_county = sub_df$fips[200]


# fill in data so that total landfall updates at the end of each year
for (i in 1:length(sub_df$fips)) {
  new_county = sub_df$fips[i]
  
  if(old_county == new_county){
    last_yr = sub_df$total_landfall[i-1]
    sub_df$total_hist_landfall[i] = last_yr+sub_df$total_hist_landfall[i-1]
    old_county = new_county
    
  }else{
    sub_df$total_hist_landfall[i] = sub_df$total_hist_landfall[i]
    old_county=new_county
  }
  
}

# rename variables for clarity
scanner <- scanner %>% rename("past_hist_landfall" = "total_hist_landfall")

# merge updated county with historical landfall data back into main dataset
scanner <- left_join(scanner, sub_df, by = c("fips", "year"))

# remove counties with no historical landfalls
scanner <- subset(scanner, total_hist_landfall>0)

# get quartiles
quantile(scanner$total_hist_landfall, prob = c(0,0.25, 0.50, 0.75, 1), na.rm=T)
# breaks = 1, 7, 12,16,26


# make event study function to wrap over. ---------------------------------------

event_study <- function(data, quartile){
  data <- na.omit(data)
  
  lb <- ifelse(quartile == 1, 1, 
               ifelse(quartile == 2, 8,
                      ifelse(quartile == 3, 13, 17)))
  
  ub <- ifelse(quartile == 1, 7,
               ifelse(quartile ==2, 12,
                      ifelse(quartile == 3, 16, 26)))
  
  L = length(data$fips)
  event_df <- data [0,]
  
  for(i in 1:L){
    
    if(data$Landfall[i] == 1 & (
      data$total_hist_landfall[i]> lb & data$total_hist_landfall[i]<= ub) & 
       data$Wind[i] >= 64){
      df <- data[i,]
      week <- (as.Date(df$week_end) + 365)
      fip_now <- df$fips[1]
      county <- data %>% filter(data$fips == fip_now)
      result <- county %>% filter(week_end >= (week-75) & week_end <= (week+70))
      result <- result %>% mutate(
        ref_num = round((as.Date(week_end) - week)/7,0)
      )
    }else{next}
    
    event_df <- rbind(event_df, result)
  }
  
  event_df$ref_num = relevel(as.factor(event_df$ref_num), ref = "-2")
  
  # run event study
  ES = feols(data = event_df, total_rev_per_cap ~ ref_num + temp_mean| fips + year + month, cluster = event_df$fips)
  results= tidy(ES)     
  
  test <- confint(ES, level =0.95)
  
  #get 95% confidence intervals
  ES_results = results %>% mutate(
    lci = confint(ES, level = 0.95)$'2.5 %',
    uci = confint(ES, level = 0.95)$'97.5 %',
    time = as.integer(str_sub(term, start = 8))
  )
  
  ref_point <- results [1,]
  ref_point <- ref_point %>% mutate(
    term = "ref_num-2",
    estimate = 0,
    std.error=0,
    time = -2,
    lci = 0,
    uci = 0
  )
  
  ES_results <- rbind(ES_results, ref_point)
  
  ES_results <- ES_results %>% mutate(
    group = ifelse(quartile == 1, "1-7",
                   ifelse(quartile == 2, "8-12",
                          ifelse(quartile == 3, "13-16", "17-26")))
  )
  
  return(ES_results)
}


# run event study for quartiles and combine results ----------------------------

esq1 <- event_study(scanner, 1)
esq2 <- event_study(scanner, 2)
esq3 <- event_study(scanner, 3)
esq4 <- event_study(scanner, 4)

es_results <- rbind(esq1, esq2, esq3, esq4)


# plot results


#subset for only weeks surrounding landfall
graph_df <- subset(es_results, time == -1 | time == 0 | time == 1)


# plot of quartile results
ggplot(data = graph_df, aes(y = estimate, x = seq(1,12, by = 1))) + 
  geom_point(aes(y = estimate, color = group), size = 3)+
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin = lci, ymax = uci, color = group), linewidth = 0.9)+
  theme_minimal()+
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                     labels=c("-1", "0", "1", "-1", "0", "1", "-1", "0", "1", "-1", "0", "1"))+
  theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20), 
        legend.text = element_text(size = 15), legend.title = element_text(size = 20), 
        plot.title = element_text(size = 30))+
  labs(title = "", x = "Weeks", 
       y = "Total Revenue per 100K Residents", color="Landfalls since 1980")



# plot all results on seperate graphs

ggplot(data = esq1, aes(x = time, y = estimate))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0)+
  geom_point(aes(x = -2, y = 0), fill = "white", shape = 21, size = 2)+
  theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20))+
  labs(title = " ", x = "Weeks", y = "Total Revenue per 100K Residents")


ggplot(data = esq2, aes(x = time, y = estimate))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0)+
  geom_point(aes(x = -2, y = 0), fill = "white", shape = 21, size = 2)+
  theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20))+
  labs(title = " ", x = "Weeks", y = "Total Revenue per 100K Residents")



ggplot(data = esq3, aes(x = time, y = estimate))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0)+
  geom_point(aes(x = -2, y = 0), fill = "white", shape = 21, size = 2)+
  theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20))+
  labs(title = " ", x = "Weeks", y = "Total Revenue per 100K Residents")




ggplot(data = esq4, aes(x = time, y = estimate))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0)+
  geom_point(aes(x = -2, y = 0), fill = "white", shape = 21, size = 2)+
  theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20))+
  labs(title = " ", x = "Weeks", y = "Total Revenue per 100K Residents")



# regressions of the same thing ------------------------------------------------


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

scanner <- scanner |> 
  group_by(fips) |>
  mutate(
    plac_threat = dplyr::lag(Hur_Threat, n=52L),
    plac_land = dplyr::lag(Hur_Landfall, n=52L)
  ) |>
  ungroup()


hist_hur_lm1 <- feols(data = scanner,
                      log(total_rev_per_cap) ~ plac_threat + plac_land +
                        temp_mean + total_hist_landfall |
                        year + month + fips,
                      cluster = c("fips", "year"),
                      mem.clean = TRUE)

summary(hist_hur_lm1)


#regression with historical landfall count and interaction,
# but only looking at hurricanes
hist_hur_lm2 <- feols(data = scanner,
                      log(total_rev_per_cap) ~ plac_threat + plac_land +
                        temp_mean + total_hist_landfall +
                        plac_threat:total_hist_landfall +
                        plac_land:total_hist_landfall |
                        year + month + fips,
                      cluster = c("fips", "year"),
                      mem.clean = TRUE)

summary(hist_hur_lm2)


esttex(hist_hur_lm1, hist_hur_lm2,
       title = "Historical Exposure Robustness Test",
       fitstat = ~n + r2)
