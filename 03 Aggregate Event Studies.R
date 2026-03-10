# Run aggregated event studies

# Load in packages
library(pacman)

p_load(tidyverse, stringr, viridis, reshape2, jtools,
       data.table, dtplyr, lubridate, plm, estimatr, fixest)


# Set up data ----------------------------------------------------------------

# load in scanner data
scanner <- read.csv('Data\\scanner_with_hur.csv')

# fix format on dates and counties
scanner <- scanner %>% mutate(
  fips = str_pad(fips, 5,'left', '0'),
  year = str_sub(week_end, end = 4),
  month = str_sub(week_end, start =6, end = 7)
) 

scanner<- scanner[order(scanner$fips, scanner$week_end),]

# remove virginia
scanner <- subset(scanner, fips_state_code != '51')


# Event study for bottled water ------------------------------------------------

L = length(scanner$fips)
event_df <- scanner [0,]

# find all landfalls observations and collect 10 weeks before and after for the county
for(i in 1:L){
  
  if(scanner$Landfall[i] == 1){
    df <- scanner[i,]
    week <- as.Date(df$week_end)
    fip_now <- df$fips
    county <- scanner %>% filter(scanner$fips == fip_now)
    result <- county %>% filter(week_end >= week-70 & week_end <= week+70 )
    result <- result %>% mutate(
      ref_num = (as.Date(week_end) - week)/7
    )
  }else{next}
  
  event_df <- rbind(event_df, result)
}

# Set week -2 as the refreence point
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

# add reference point to the data
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

#plot baseline event study
ggplot(data = ES_results, aes(x = time, y = estimate))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0)+
  geom_point(aes(x = -2, y = 0), fill = "white", shape = 21, size = 2)+
  theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20))+
  labs(title = " ", x = "Weeks", y = "Total Revenue per 100K Residents")



# SUBSET BY HURRIANE TYPE ------------------------------------------------------
# <64 TS, 64+ HURRICANE, 96+ MAJOR HURRICANE

# Run for tropical storms: wind <64 knots
event_df <- scanner [0,]

for(i in 1:L){
  
  if(scanner$Landfall[i] == 1 & scanner$Wind[i] <64){
    df <- scanner[i,]
    week <- as.Date(df$week_end)
    fip_now <- df$fips
    county <- scanner %>% filter(scanner$fips == fip_now)
    result <- county %>% filter(week_end >= week-70 & week_end <= week+70 )
    result <- result %>% mutate(
      ref_num = (as.Date(week_end) - week)/7
    )
  }else{next}
  
  event_df <- rbind(event_df, result)
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

ES_results <- mutate(ES_results,type = "TS")
all_results <- ES_results


# Rerun for hurricanes (cat 1-5)
event_df <- scanner [0,]

for(i in 1:L){
  
  if(scanner$Landfall[i] == 1 & (scanner$Wind[i] >=64 & scanner$Wind[i] <96)){
    df <- scanner[i,]
    week <- as.Date(df$week_end)
    fip_now <- df$fips
    county <- scanner %>% filter(scanner$fips == fip_now)
    result <- county %>% filter(week_end >= week-70 & week_end <= week+70 )
    result <- result %>% mutate(
      ref_num = (as.Date(week_end) - week)/7
    )
  }else{next}
  
  event_df <- rbind(event_df, result)
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

ES_results <- mutate(ES_results, type = "Minor")
all_results <- rbind(all_results,ES_results)

# Rerun for Major Hurricanes (Cat 3-5)
event_df <- scanner [0,]

for(i in 1:L){
  
  if(scanner$Landfall[i] == 1 & (scanner$Wind[i] >=96)){
    df <- scanner[i,]
    week <- as.Date(df$week_end)
    fip_now <- df$fips
    county <- scanner %>% filter(scanner$fips == fip_now)
    result <- county %>% filter(week_end >= week-70 & week_end <= week+70 )
    result <- result %>% mutate(
      ref_num = (as.Date(week_end) - week)/7
    )
  }else{next}
  
  event_df <- rbind(event_df, result)
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

ES_results <- mutate(ES_results, type = "Major")
all_results <- rbind(all_results,ES_results)


#plot event study
ggplot(data = all_results, aes(x = time, y = estimate,color = type))+
  geom_line(size = 0.7)+
  geom_point(size = 1.5)+
  theme_minimal()+
  #geom_errorbar(aes(ymin = lci, ymax = uci))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0)+
  geom_point(aes(x = -2, y = 0), fill = "white", shape = 21, size = 2)+
  theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20), 
        legend.text =  element_text(size = 12), legend.title = element_text(size = 15),
        )+
  labs(title = " ", x = "Weeks", y = "Total Revenue per 100K Residents", color = "Storm Class")

