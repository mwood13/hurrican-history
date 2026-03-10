# robustness

library(pacman)

p_load(tidyverse, stringr, viridis, reshape2, jtools,
       data.table, dtplyr, lubridate, plm, estimatr, fixest)

# event study on prices

scanner <- read.csv('Data\\scanner_with_hur.csv')


scanner <- scanner %>% mutate(
  fips = str_pad(fips, 5,'left', '0'),
  year = str_sub(week_end, end = 4),
  month = str_sub(week_end, start =6, end = 7),
  date = str_sub(week_end,start = 6),
  date = as.Date(date, format =  "%m-%d")
) 

scanner<- scanner[order(scanner$fips, scanner$week_end),]

scanner <- subset(scanner, fips_state_code != '51')


# Event study for price ------------------------------------------------
L = length(scanner$fips)
event_df <- scanner [0,]

for(i in 1:L){
  
  if(scanner$Landfall[i] == 1 & scanner$Wind[i] >= 64){
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
ES = feols(data = event_df, price ~ ref_num | fips + year + month, cluster = event_df$fips)
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

#plot event study
ggplot(data = ES_results, aes(x = time, y = estimate))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0)+
  geom_point(aes(x = -2, y = 0), fill = "white", shape = 21, size = 2)+
  theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20))+
  labs(title = " ", x = "Weeks", y = "Average Price")








# Event study for start of summer ------------------------------------------------

landfall_counties <- unique(subset(scanner, Landfall ==1 & Wind >= 64)$fips)

scanner <- subset(scanner , fips %in% landfall_counties)
L = length(scanner$fips)
event_df <- scanner [0,]

for(i in 1:L){
  
  if(scanner$date[i] >= '2026-06-15' & scanner$date[i] <= '2026-06-27'){
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
ES = feols(data = event_df, total_rev_per_cap ~ ref_num +temp_mean | fips + year + month, cluster = event_df$fips)
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

#plot event study
ggplot(data = ES_results, aes(x = time, y = estimate))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0)+
  geom_point(aes(x = -2, y = 0), fill = "white", shape = 21, size = 2)+
  theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20))+
  labs(title = " ", x = "Weeks", y = "Total Revenue Per 100K Residents")

# Summer with price


# run event study
ES = feols(data = event_df, price ~ ref_num  | fips + year + month, cluster = event_df$fips)
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

#plot event study
ggplot(data = ES_results, aes(x = time, y = estimate))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0)+
  geom_point(aes(x = -2, y = 0), fill = "white", shape = 21, size = 2)+
  theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20))+
  labs(title = " ", x = "Weeks", y = "Average Weekly Price")




# historical break out start of summer -----------------------------------------



sub_df <- scanner %>% group_by(fips, year) %>% summarise(
  total_landfall = sum(Landfall),
  total_hist_landfall = head(total_hist_landfall, 1L)
)%>%ungroup()

old_county = sub_df$fips[200]

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



scanner <- scanner %>% rename("past_hist_landfall" = "total_hist_landfall")

scanner <- left_join(scanner, sub_df, by = c("fips", "year"))

scanner <- subset(scanner, total_hist_landfall>0)



# Hist q1

scanner <- na.omit(scanner)

L = length(scanner$fips)
event_df <- scanner [0,]

for(i in 1:L){
  
  if(scanner$date[i] >= '2026-06-15' & scanner$date[i] <= '2026-06-27' &
    (scanner$total_hist_landfall[i]<= 7)){
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
  tercile = " 1 to 7 landfalls"
)

all_results <- ES_results



# Hist q2

scanner <- na.omit(scanner)

L = length(scanner$fips)
event_df <- scanner [0,]

for(i in 1:L){
  
  if(scanner$date[i] >= '2026-06-15' & scanner$date[i] <= '2026-06-27' &
     (scanner$total_hist_landfall[i]> 7 & scanner$total_hist_landfall[i]<=12)){
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
  tercile = " 8 to 12 landfalls"
)

all_results <- rbind(all_results , ES_results)


# Hist q2

scanner <- na.omit(scanner)

L = length(scanner$fips)
event_df <- scanner [0,]

for(i in 1:L){
  
  if(scanner$date[i] >= '2026-06-15' & scanner$date[i] <= '2026-06-27' &
     (scanner$total_hist_landfall[i]> 12 & scanner$total_hist_landfall[i]<=16)){
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

ES_results <- ES_results %>% mutate(
  tercile = "13 to 16 landfalls"
)

all_results <- rbind(all_results , ES_results)


# Hist q2

scanner <- na.omit(scanner)

L = length(scanner$fips)
event_df <- scanner [0,]

for(i in 1:L){
  
  if(scanner$date[i] >= '2026-06-15' & scanner$date[i] <= '2026-06-27' &
     (scanner$total_hist_landfall[i]> 16)){
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
  tercile = "17 or more landfalls"
)

all_results <- rbind(all_results , ES_results)


# plot
graph_df <- subset(all_results, time == -1 | time == 0 | time == 1)


ggplot(data = graph_df, aes(y = estimate, x = seq(1,12, by = 1))) + 
  geom_point(aes(y = estimate, color = tercile), size = 3)+
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin = lci, ymax = uci, color = tercile), linewidth = 0.9)+
  theme_minimal()+
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                     labels=c("-1", "0", "1", "-1", "0", "1", "-1", "0", "1", "-1", "0", "1"))+
  theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20), 
        legend.text = element_text(size = 15), legend.title = element_text(size = 20), 
        plot.title = element_text(size = 30))+
  labs(title = "", x = "Weeks", 
       y = "Total Revenue per 100K Residents", color="Landfalls since 1980")


# beginning of summer recency --------------------------------------------------



sub_df <- subset(scanner, last_wind_hur>= 64)

sub_df <- subset(sub_df, Landfall == 1 & Wind >= 64)

quantile(sub_df$years_since_landfall_hur, prob = c(0,0.25, 0.50, 0.75, 1), na.rm=T)



# 1 to 3 years ---------------------------------------------------------------

scanner <- na.omit(scanner)

L = length(scanner$fips)
event_df <- scanner[0,]

for(i in 1:L){
  
  if((scanner$date[i] >= '2026-06-15' & scanner$date[i] <= '2026-06-27') &
     scanner$years_since_landfall_hur[i]<= 2.86 ){
    df <- scanner[i,]
    week <- as.Date(df$week_end)
    fip_now <- df$fips
    county <- scanner %>% filter(scanner$fips == fip_now)
    result <- county %>% filter(week_end >= week-70 & week_end <= week+70 )
    result <- result %>% mutate(
      ref_num = (as.Date(week_end) - week)/7
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
  tercile = " 1 to 2 years"
)

all_results <- ES_results


# run event study for 1 to 2 landfall counties --------------------------------


L = length(scanner$fips)
event_df <- scanner [0,]

for(i in 1:L){
  
  if(scanner$date[i] >= '2026-06-15' & scanner$date[i] <= '2026-06-27' &
     (scanner$years_since_landfall_hur[i] > 2.86 & scanner$years_since_landfall_hur[i] <= 3.99)){
    df <- scanner[i,]
    week <- as.Date(df$week_end)
    fip_now <- df$fips
    county <- scanner %>% filter(scanner$fips == fip_now)
    result <- county %>% filter(week_end >= week-70 & week_end <= week+70 )
    result <- result %>% mutate(
      ref_num = (as.Date(week_end) - week)/7
    )
  }else{next}
  
  event_df <- rbind(event_df, result, fill=T)
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
  tercile = " 3 years"
)

all_results <- rbind(all_results , ES_results)


# run event study for 2 to 4 landfall counties --------------------------------


L = length(scanner$fips)
event_df <- scanner [0,]

for(i in 1:L){
  
  if(scanner$date[i] >= '2026-06-15' & scanner$date[i] <= '2026-06-27' &
     (scanner$years_since_landfall_hur[i]>3.99 & scanner$years_since_landfall_hur[i]<=6.87)){
    df <- scanner[i,]
    week <- as.Date(df$week_end)
    fip_now <- df$fips
    county <- scanner %>% filter(scanner$fips == fip_now)
    result <- county %>% filter(week_end >= week-70 & week_end <= week+70 )
    result <- result %>% mutate(
      ref_num = (as.Date(week_end) - week)/7
    )
  }else{next}
  
  event_df <- rbind(event_df, result, fill = T)
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
  tercile = " 4 to 6 years"
)

all_results <- rbind(all_results , ES_results)


# run event study for 5 plus landfall counties --------------------------------

L = length(scanner$fips)
event_df <- scanner [0,]

for(i in 1:L){
  
  if(scanner$date[i] >= '2026-06-15' & scanner$date[i] <= '2026-06-27' & 
     (scanner$years_since_landfall_hur[i]>6.87)){
    df <- scanner[i,]
    week <- as.Date(df$week_end)
    fip_now <- df$fips
    county <- scanner %>% filter(scanner$fips == fip_now)
    result <- county %>% filter(week_end >= week-70 & week_end <= week+70 )
    result <- result %>% mutate(
      ref_num = (as.Date(week_end) - week)/7
    )
  }else{next}
  
  event_df <- rbind(event_df, result, fill=T)
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
  tercile = " 7 or more years"
)


all_results <- rbind(all_results , ES_results)


#plot event study

# plot all three on same graph

graph_df <- subset(all_results, time == -1 | time == 0 | time == 1)


ggplot(data = graph_df, aes(y = estimate, x = seq(1,12, by = 1))) + 
  geom_point(aes(y = estimate, color = tercile), size = 3)+
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin = lci, ymax = uci, color = tercile), linewidth = 0.9)+
  theme_minimal()+
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                     labels=c("-1", "0", "1", "-1", "0", "1", "-1", "0", "1", "-1", "0", "1"))+
  theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20), 
        legend.text = element_text(size = 15), legend.title = element_text(size = 20), 
        plot.title = element_text(size = 30))+
  labs(title = "", x = "Weeks", 
       y = "Total Revenue per 100K Residents", color="Years Since Last Hurricane")