# robustness test for timing results

# Recency Event Studies

# load in packages

library(pacman)

p_load(tidyverse, stringr, viridis, reshape2, jtools,tmap,RColorBrewer,
       data.table, dtplyr, lubridate, plm, estimatr, fixest)

# load in data ----------------------------------------------------

# load in full dataset
scanner <- fread('Data\\scanner_with_hur.csv')

# format data
scanner <- scanner %>% mutate(
  fips = str_pad(fips, 5,'left', '0'),
  year = str_sub(week_end, end = 4),
  month = str_sub(week_end, start =6, end = 7)
) 

scanner<- scanner[order(scanner$fips, scanner$week_end),]

scanner <- subset(scanner, fips_state_code != '51')

# subset for all hurricane level landfalls
sub_df <- subset(scanner, Landfall == 1)
sub_df <- subset(sub_df, Wind >= 64)

scanner <- subset(scanner, last_wind_hur>= 64)

sub_df <- subset(scanner, Landfall == 1 & Wind >= 64)

quantile(sub_df$years_since_landfall_hur, prob = c(0,0.25, 0.50, 0.75, 1), na.rm=T)

# breaks 0,1,3,14

# Event study for 1st quartile (same season events) -----------------------------


event_study <- function(data, quartile){
  data <- na.omit(data)
  
  
  lb <- ifelse(quartile == 1, 0, 
               ifelse(quartile == 2, 2.86,
                      ifelse(quartile == 3, 3.99,6.87)))
  
  ub <- ifelse(quartile == 1, 2.86,
               ifelse(quartile ==2, 3.99,
                      ifelse(quartile == 3, 6.87, 17)))
  
  L = length(data$fips)
  event_df <- data[0,]
  
  for(i in 1:L){
    
    if(data$Landfall[i] == 1 & 
       (data$years_since_landfall[i] > lb & data$years_since_landfall[i]<= ub) & 
       data$Wind[i] >=64){
      df <- data[i,]
      week <- (as.Date(df$week_end) - 365)
      fip_now <- df$fips[1]
      county <- data %>% filter(data$fips == fip_now)
      result <- county %>% filter(week_end >= (week-75) & week_end <= (week+70))
      result <- result %>% mutate(
        ref_num = round((as.Date(week_end) - week)/7,0)
      )
    }else{next}
    
    event_df <- rbind(event_df, result, fill=TRUE)
  }
  
  event_df$ref_num = relevel(as.factor(event_df$ref_num), ref = "-2")
  
  # run event study
  ES = feols(data = event_df, total_rev_per_cap ~ ref_num + temp_mean | fips + year + month, cluster = event_df$fips)
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
    group = ifelse(quartile == 1, "1-2", 
                   ifelse(quartile == 2, "3",
                          ifelse(quartile == 3, "4-6", "7+")) )
  )
}



# run event studies ------------------------------------------------------------

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
       y = "Total Revenue per 100K Residents", color="Years Between Hurricanes")



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




# regressions of same thing ----------------------------------------------------

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

scanner <- scanner |> 
  group_by(fips) |>
  mutate(
    plac_threat = dplyr::lead(Hur_Threat, n=52L),
    plac_land = dplyr::lead(Hur_Landfall, n=52L)
  ) |>
  ungroup()


# regressions with years since last hurricane, but only looking at hurricanes
disc_hur2_lm1 <- feols(data = scanner,
                       log(total_rev_per_cap) ~ plac_threat + plac_land +
                         temp_mean + years_since_landfall_hur|
                         year + month + fips,
                       cluster = c("fips", "year"),
                       mem.clean = TRUE)

summary(disc_hur2_lm1)

# regression with years since last hurricane and interaction,
# but only looking at hurricanes
disc_hur2_lm2 <- feols(data = scanner,
                       log(total_rev_per_cap) ~ plac_threat + plac_land +
                         temp_mean + years_since_landfall_hur +
                         plac_threat:years_since_landfall_hur +
                         plac_land:years_since_landfall_hur  |
                         year + month + fips,
                       cluster = c("fips", "year"),
                       mem.clean = TRUE)

summary(disc_hur2_lm2)


esttex(disc_hur2_lm1, disc_hur2_lm2,
       title = "Years Between Robustness Test",
       fitstat = ~n + r2)
