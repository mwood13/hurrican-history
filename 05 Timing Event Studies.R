# Recency Event Studies

# load in packages

library(pacman)

p_load(tidyverse, stringr, viridis, reshape2, jtools,tmap,RColorBrewer,
       data.table, dtplyr, lubridate, plm, estimatr, fixest)

source("event study functions.R")

# load in data ----------------------------------------------------

# load in full dataset
scanner <- fread('Data\\scanner_with_hur.csv')

# format data
scanner <- scanner %>% mutate(
  fips = str_pad(fips, 5,'left', '0'),
  year = str_sub(week_end, end = 4),
  month = str_sub(week_end, start =6, end = 7),
  minor_hur = ifelse(Wind >= 64 & Wind < 96, 1, 0)
) 

scanner<- scanner[order(scanner$fips, scanner$week_end),]

scanner <- subset(scanner, fips_state_code != '51')


# get historical landfall count
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

# subset for all hurricane level landfalls
sub_df <- subset(scanner, Landfall == 1)
sub_df <- subset(sub_df, Wind >= 64)

# get quartiles of years since any landfall for hurricane level landfalls
quantile(sub_df$years_since_landfall, prob = c(0, 0.25, 0.50, 0.75, 1), na.rm=T)


ggplot(data = sub_df, aes(x = years_since_landfall))+
  geom_histogram(binwidth = 1, color = "black", fill = "grey", alpha = 0.5)+
  theme_minimal()+
  labs(title = "Distribution of Years Since Last Hurricane Landfall", x = "Years Since Last Hurricane Landfall", y = "Count")
# breaks 0,1,3,14

# subset data to look at years since last true hurricane event -----------------


scanner <- subset(scanner, last_wind_hur>= 64)

sub_df <- subset(scanner, Landfall == 1 & Wind >= 64)

quantile(sub_df$years_since_landfall_hur, prob = c(0,0.25, 0.50, 0.75, 1), na.rm=T)

# breaks, 0.98, 2.85, 3.98, 6.86, 16.78


ggplot(data = sub_df, aes(x = years_since_landfall_hur))+
  geom_histogram(binwidth = 1, color = "black", fill = "grey", alpha = 0.5)+
  theme_minimal()+
  labs(title = "Distribution of Years Since Last Hurricane Landfall", x = "Years Since Last Hurricane Landfall", y = "Count")+
  theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20), plot.title = element_text(size = 30))


# 1st quartile (1 to 2 years) ----------------------------------------------

scanner <- na.omit(scanner)

q1_df <- get_event_data(scanner, "years", c(0.98, 2.85), c(64, 200))
ES_q1 = feols(data = q1_df, total_rev_per_cap ~ ref_num + temp_mean +total_hist_landfall | fips + year + month, cluster = q1_df$fips)
q1_results <- make_ES_table(ES_q1, "0.98 to 2.85 years")

ggplot(data = q1_results, aes(x = time, y = estimate))+
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

q2_df <- get_event_data(scanner, "years", c(2.85, 3.98), c(64, 200))
ES_q2 = feols(data = q2_df, total_rev_per_cap ~ ref_num + temp_mean +total_hist_landfall | fips + year + month, cluster = q2_df$fips)
q2_results <- make_ES_table(ES_q2, "2.86 to 3.98 years")

ggplot(data = q2_results, aes(x = time, y = estimate))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0)+
  geom_point(aes(x = -2, y = 0), fill = "white", shape = 21, size = 2)+
  theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20))+
  labs(title = " ", x = "Weeks", y = "Total Revenue per 100K Residents")


# run event study for 3rd quartile (4 to 6 seasons) ----------------------------

q3_df <- get_event_data(scanner, "years", c(3.98, 6.86), c(64, 200))
ES_q3 = feols(data = q3_df, total_rev_per_cap ~ ref_num + temp_mean +total_hist_landfall | fips + year + month, cluster = q3_df$fips)
q3_results <- make_ES_table(ES_q3, "3.99 to 6.86 years")

ggplot(data = q3_results, aes(x = time, y = estimate))+
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

q4_df <- get_event_data(scanner, "years", c(6.86, 20), c(64, 200))
ES_q4 = feols(data = q4_df, total_rev_per_cap ~ ref_num + temp_mean +total_hist_landfall | fips + year + month, cluster = q4_df$fips)
q4_results <- make_ES_table(ES_q4, "6.86 to 16.78 years")

ggplot(data = q4_results, aes(x = time, y = estimate))+
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

all_results <- rbind(q1_results, q2_results, q3_results, q4_results)

graph_df <- subset(all_results, time == -1 | time == 0 | time == 1)


ggplot(data = graph_df, aes(y = estimate, x = seq(1,12, by = 1))) + 
  geom_point(aes(y = estimate, color = id), size = 3)+
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin = lci, ymax = uci, color = id), linewidth = 0.9)+
  theme_minimal()+
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                     labels=c("-1", "0", "1", "-1", "0", "1", "-1", "0", "1", "-1", "0", "1"))+
  theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20), 
        legend.text = element_text(size = 15), legend.title = element_text(size = 20), 
        plot.title = element_text(size = 30))+
  labs(title = "", x = "Weeks", 
       y = "Total Revenue per 100K Residents", color="Years Since Last Hurricane")


