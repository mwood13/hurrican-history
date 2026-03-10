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

# get quartiles of years since any landfall for hurricane level landfalls
quantile(sub_df$years_since_landfall, prob = c(0, 0.25, 0.50, 0.75, 1), na.rm=T)

# breaks 0,1,3,14

# Event study for 1st quartile (same season events) -----------------------------

scanner <- na.omit(scanner)

L = length(scanner$fips)
event_df <- scanner[0,]

for(i in 1:L){
  
  if(scanner$Landfall[i] == 1 & scanner$years_since_landfall[i]<= 0.365 & 
     scanner$Wind[i] >=64){
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
  tercile = " 0 years"
)

all_results <- ES_results

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

# run event study for 2nd quartile (1 to 2 seasons) --------------------------------


L = length(scanner$fips)
event_df <- scanner [0,]

for(i in 1:L){
  
  if(scanner$Landfall[i] == 1 & 
     (scanner$years_since_landfall[i]> 0.365 & scanner$years_since_landfall[i] <=2.11)&
     scanner$Wind[i] >=64){
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
  tercile = " 1 to 2 years"
)

all_results <- rbind(all_results , ES_results)

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


# run event study for 3rd quartile (3 seasons) --------------------------------


L = length(scanner$fips)
event_df <- scanner [0,]

for(i in 1:L){
  
  if(scanner$Landfall[i] == 1 & 
     (scanner$years_since_landfall[i]> 2.11 & scanner$years_since_landfall[i] <=3.03)&
     scanner$Wind[i] >=64){
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

# run event study for 4th quartile (more than 3 seasons) --------------------------------

L = length(scanner$fips)
event_df <- scanner [0,]

for(i in 1:L){
  
  if(scanner$Landfall[i] == 1 & 
     (scanner$years_since_landfall[i]> 3.03)&
     scanner$Wind[i] >=64){
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
  tercile = " 4 or more years"
)


all_results <- rbind(all_results , ES_results)

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

#plot event study

# plot all results on the same graph

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
       y = "Total Revenue per 100K Residents", color="Years Since Last Landfall")



# subset data to look at years since last true hurricane event -----------------


scanner <- subset(scanner, last_wind_hur>= 64)

sub_df <- subset(scanner, Landfall == 1 & Wind >= 64)

quantile(sub_df$years_since_landfall_hur, prob = c(0,0.25, 0.50, 0.75, 1), na.rm=T)

# 

# 1st quartile (1 to 2 years) ---------------------------------------------------------------

scanner <- na.omit(scanner)

L = length(scanner$fips)
event_df <- scanner[0,]

for(i in 1:L){
  
  if(scanner$Landfall[i] == 1 & scanner$years_since_landfall_hur[i]<= 2.86 & 
     scanner$Wind[i] >=64){
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

# run event study for 2nd quartile (3 seasons) --------------------------------


L = length(scanner$fips)
event_df <- scanner [0,]

for(i in 1:L){
  
  if(scanner$Landfall[i] == 1 & 
     (scanner$years_since_landfall_hur[i] > 2.86 & scanner$years_since_landfall_hur[i] <= 3.99)&
     scanner$Wind[i] >=64){
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


# run event study for 3rd quartile (4 to 6 seasons) --------------------------------


L = length(scanner$fips)
event_df <- scanner [0,]

for(i in 1:L){
  
  if(scanner$Landfall[i] == 1 & 
     (scanner$years_since_landfall_hur[i]>3.99 & scanner$years_since_landfall_hur[i]<=6.87) &
     scanner$Wind[i] >=64){
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



# run event study for 4th quartile (7+ seasons) --------------------------------

L = length(scanner$fips)
event_df <- scanner [0,]

for(i in 1:L){
  
  if(scanner$Landfall[i] == 1 & 
     (scanner$years_since_landfall_hur[i]>6.87)&
     scanner$Wind[i] >=64){
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

#plot event study

# plot all results on the same graph

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


