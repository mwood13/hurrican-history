# Run aggregated event studies

# Load in packages
library(pacman)

p_load(tidyverse, stringr, viridis, reshape2, jtools,
       data.table, dtplyr, lubridate, plm, estimatr, fixest)

source("event study functions.R")

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

base_df <- get_event_data(scanner, "none", c(0,100), c(0,200))


# run event study for total revenue
ES_rev = feols(data = base_df, total_rev_per_cap ~ ref_num + temp_mean | fips + year + month, cluster = base_df$fips)

ES_results <- make_ES_table(ES_rev, "revenue")

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


# Test for only quantity effect by ES on price

ES_price = feols(data = base_df, price ~ ref_num + temp_mean | fips + year + month, cluster = base_df$fips)

ES_price_results <- make_ES_table(ES_price, "price")

ggplot(data = ES_price_results, aes(x = time, y = estimate))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0)+
  geom_point(aes(x = -2, y = 0), fill = "white", shape = 21, size = 2)+
  theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20))+
  labs(title = " ", x = "Weeks", y = "Average Price")

# Same test with Event study by volume

ES_vol <- feols(data = base_df, total_vol/CENSUS2010POP*100000 ~ ref_num + temp_mean | fips + year + month, cluster = base_df$fips)

ES_vol_results <- make_ES_table(ES_vol, "volume")

ggplot(data = ES_vol_results, aes(x = time, y = estimate))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0)+
  geom_point(aes(x = -2, y = 0), fill = "white", shape = 21, size = 2)+
  theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20))+
  labs(title = " ", x = "Weeks", y = "Total Ounces Sold per 100K Residents")



# SUBSET BY HURRIANE TYPE ------------------------------------------------------
# <64 TS, 64+ HURRICANE, 96+ MAJOR HURRICANE

# Run for tropical storms: wind <64 knots
ts_df <- get_event_data(scanner, "none", c(0,100), c(0,64))
ES_ts = feols(data = ts_df, total_rev_per_cap ~ ref_num + temp_mean | fips + year + month, cluster = ts_df$fips)
ts_results <- make_ES_table(ES_ts, "Below Hurricane")


# hurricanes cat 1

cat1_df <- get_event_data(scanner, "none", c(0,100), c(64,83))
ES_cat1 = feols(data = cat1_df, total_rev_per_cap ~ ref_num + temp_mean | fips + year + month, cluster = cat1_df$fips)
cat1_results <- make_ES_table(ES_cat1, "Hurricane Cat 1")

# hurricanes cat 2

cat2_df <- get_event_data(scanner, "none", c(0,100), c(83,96))
ES_cat2 = feols(data = cat2_df, total_rev_per_cap ~ ref_num + temp_mean | fips + year + month, cluster = cat2_df$fips)
cat2_results <- make_ES_table(ES_cat2, "Hurricane Cat 2")

# major 

major_df <- get_event_data(scanner, "none", c(0,100), c(96,200))
ES_major = feols(data = major_df, total_rev_per_cap ~ ref_num + temp_mean | fips + year + month, cluster = major_df$fips)
major_results <- make_ES_table(ES_major, "Hurricane Cat 3+")


all_results <- rbind(ts_results, cat1_results, cat2_results, major_results)


#plot event study
ggplot(data = all_results, aes(x = time, y = estimate,color = id))+
  geom_line(size = 0.7)+
  geom_point(size = 1.5)+
  theme_minimal()+
  geom_errorbar(aes(ymin = lci, ymax = uci))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0)+
  geom_point(aes(x = -2, y = 0), fill = "white", shape = 21, size = 2)+
  theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20), 
        legend.text =  element_text(size = 12), legend.title = element_text(size = 15),
        )+
  labs(title = " ", x = "Weeks", y = "Total Revenue per 100K Residents", color = "Storm Class")



# histogram of hurricane categories

scanner <- scanner %>% mutate(
  hur_cat = ifelse(Wind < 64, 0, 
                   ifelse(Wind >= 64 & Wind < 83, 1, 
                          ifelse(Wind >= 83 & Wind < 96, 2, 
                                 ifelse(Wind >= 96 & Wind < 113, 3, 
                                        ifelse(Wind >= 113 & Wind < 137, 4, 5))))))

plot_df <- subset(scanner, Landfall == 1)
plot_df <- plot_df %>% group_by(week_end) %>% summarize(
  hur_cat = head(hur_cat, 1)
)

ggplot(data=plot_df, aes(x=hur_cat))+
  geom_histogram(binwidth = 1, color = "black", fill = "grey", alpha = 0.5)+
  theme_minimal()+
  labs(title = "Distribution of Landfalls Categories", x = "Historical Landfalls", y = "Count")+
  theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20), plot.title = element_text(size = 30))


