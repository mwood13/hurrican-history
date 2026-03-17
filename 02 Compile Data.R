# combine scanner data with hurricane and demographics data

# load in packages
library(pacman)

p_load(tidyverse, stringr, viridis, reshape2, jtools, sf, tmap,
       data.table, dtplyr, lubridate, plm, estimatr, fixest, stats)


# load in scanner data

scanner <- fread('Data\\water_scanner.csv')

scanner <- scanner %>% mutate(
  fips = ifelse(fips == 12025, 12086, fips),
  week_end = as.Date((gsub("(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3", week_end))),
  fips = str_pad(fips, 5,'left', '0')
)

# get county shapes

us_counties <- tigris::counties(state = c('01', '12', '13',
                                          '22', '28','37', '45',
                                          '48'), year = 2010)


scanner <- subset(scanner, fips %in% us_counties$GEOID10) # 907 counties
image_df <- us_counties %>% filter(GEOID10 %in% scanner$fips)

length(unique(scanner$fips))

# map of counties represented in the dataset
tm_shape(image_df) + tm_fill('pink')+
tm_shape(us_counties) + tm_borders()+
  tm_layout(frame=FALSE)+tm_title("Counties Represented in Scanner Data", size = 4)


# add in current hurricane information -------------------------------------------------

# pull in data from GIS archive
hur <- fread('Data/Daily_Hurricanes.csv')

# build variables for if a county was in the cone of uncertainty for a given date
hur <- hur %>% mutate(
  fips = str_pad(fips, 5,'left', '0'),
  week = ceiling_date(Date, "week")-1,
  Threat = ifelse(Threat24 + Threat48 + Threat72 + Threat96 +Threat120 > 0 , 1, 0),
  After24 = lag(Landfall, 1L),
  After48 = lag(Landfall, 2L),
  After72 = lag(Landfall, 3L),
  After96 = lag(Landfall, 4L),
  After = After24 + After48 + After72 + After96,
  Affected = ifelse(Threat > 0 | Landfall > 0, 1 , 0)
)


# condense to weekly level
hur <- hur %>% group_by(fips, week) %>% summarize(
  Landfall = ifelse(sum(Landfall) > 0 , 1, 0),
  Wind = max(Land_MaxWind),
  Threat = ifelse(sum(Threat) > 0, 1, 0),
  After = ifelse(sum(After) > 0, 1, 0),
  days_affected = sum(Affected),
  percent_affected = days_affected/7
)


# merge with scanner data
scanner <- left_join(scanner, hur, by = c("fips" = "fips", "week_end" = "week"))

# plot total threats and landfall
sub <-scanner %>% group_by(fips, week_end) %>% summarise(
  threat = ifelse(sum(Threat)>0, 1, 0),
  land = ifelse(sum(Landfall)> 0 ,1, 0)
)


sub <- sub %>% group_by(fips) %>% summarise(
  total_threat = sum(threat),
  total_land = sum(land)
)



image_df <- left_join(image_df, sub, by = c('GEOID10' = 'fips'))
im_df <- subset(image_df, total_land > 0)

# total landfalls from 2008 to 2019
tm_shape(us_counties)+tm_shape(im_df) + 
  tm_polygons(fill = 'total_land', 
              fill.scale = tm_scale_intervals(
                values = "mako",
                label.style = "discrete",
                breaks = c(1,3,5,7,8),
                label.format = tm_label_format(digits = 0)
                
              ),
              fill.legend = tm_legend(
                title = "Landfall Count",
                title.size = 1.5,
                text.size = 1.2,
                na.show = FALSE,
                position = tm_pos_out("right")
              )
            )+
  tm_shape(us_counties) + tm_borders()+ 
  tm_layout(frame=FALSE,component.autoscale = F)+
  tm_title("Landfalls by County: 2008 - 2019",
           size=4)



# add in historical hurricane variables ----------------------------------------

# pull in historical count from file 01
hist_hur_df <- read.csv("Data\\Historical Landfall Count.csv")

hist_hur_df <- hist_hur_df %>% mutate(
  fips = str_pad(fips, 5,'left', '0'),
)


# merge with scanner data
scanner <- left_join(scanner, hist_hur_df, by = "fips")

scanner <- scanner %>% mutate(
  total_hist_landfall = ifelse(is.na(total_hist_landfall), 0, total_hist_landfall)
)


# plot landfalls
sub <- scanner  %>% group_by(fips) %>% summarise(
  hist = head(total_hist_landfall, 1L)
)

image_df <- left_join(image_df, sub, by = c('GEOID10' = 'fips'))

hist_df <- subset(image_df, hist > 0)


# plot of hurricane count from 1978 to 2007
tm_shape(us_counties)+tm_borders()+
  tm_shape(hist_df)+
  tm_polygons(fill = 'hist', 
              fill.scale = tm_scale_intervals(
                values = "mako",
                label.style = "discrete",
                breaks = c(1,6,11,16,21),
                label.format = tm_label_format(digits = 0)
                
              ),
              fill.legend = tm_legend(
                title = "Landfall Count",
                title.size = 1.5,
                text.size = 1.2,
                na.show = FALSE,
                position = tm_pos_out("right")
              )
  )+
  tm_borders()+
  tm_layout(frame=FALSE,component.autoscale = F)+
  tm_title("Landfalls by County: 1980-2007",
           size=4)


# add in recency data ----------------------------------------------------------


# pull in recency data from file 01
rec_hur_df <- read.csv("Data\\Recent Landfall Date.csv")

rec_hur_df <- rec_hur_df %>% mutate(
  fips = str_pad(fips, 5,'left', '0'),
)

# merge with scanner data
scanner <- left_join(scanner, rec_hur_df, by = "fips")

scanner <- scanner[order(scanner$fips, scanner$week_end)]

# set up columns to fill in 
scanner <- scanner %>% mutate(
  days_since_last = 0,
  days_since_last_hur = 0,
  last_wind = 0,
  last_wind_hur = 0
)

county = scanner$fips[1000]

# fill in columns to calculate length of time since last storm and last hurricane
for(i in 1:length(scanner$fips)){
  
  new_county = scanner$fips[i]
  week = scanner$week_end[i]
  
  if(county != new_county){
    print(paste("New County", new_county))
    last_hit = scanner$recent_landfall[i]
    last_hit_hur = scanner$recent_landfall[i]
    last_wind_hur = scanner$Last_Wind[i]
    last_wind = scanner$Last_Wind[i]
    county = new_county
  }
  
  scanner$days_since_last[i] = as.Date(week) - as.Date(last_hit)
  scanner$days_since_last_hur[i] = as.Date(week) - as.Date(last_hit_hur)
  scanner$last_wind[i] = last_wind
  scanner$last_wind_hur[i] = last_wind_hur
  
  if(scanner$Landfall[i]==1){
    last_hit = week
    last_wind = scanner$Wind[i]
    if(scanner$Wind[i] >=64){
      last_hit_hur = week
      last_wind_hur = scanner$Wind[i]
    }
  }
  
}

# get years and weeks
scanner <- scanner %>% mutate(
  weeks_since_landfall = days_since_last/7,
  years_since_landfall = days_since_last/365,
  weeks_since_landfall_hur = days_since_last_hur/7,
  years_since_landfall_hur = days_since_last_hur/365
)


scanner <- rename(scanner, "first_wind" = "Last_Wind")
# makes maps of time since for 2008. 

sub_df <- subset(scanner, year == 2008)

sub_df <- sub_df %>% group_by(fips) %>%
  summarize(
    years_since_landfall = tail(years_since_landfall, 1L)
  )

sub_df <- na.omit(sub_df)

image_df <- us_counties %>% filter(GEOID10 %in% scanner$fips)

image_df <- left_join(image_df, sub_df, by = c('GEOID10' = 'fips'))
image_df <- subset(image_df, !is.na(years_since_landfall))


# years since last landfall in Dec 2008
tm_shape(us_counties) +
  tm_shape(image_df) +
  tm_polygons(fill = 'years_since_landfall', 
              fill.scale = tm_scale_intervals(
                values = "mako",
                label.style = "discrete",
                breaks = c(0,1,2,3,4,5, 25),
                label.format = tm_label_format(digits = 1)
                
              ),
              fill.legend = tm_legend(
                title = "Years",
                title.size = 1.5,
                text.size = 1.2,
                na.show = FALSE,
                position = tm_pos_out("right")
              )
  )+
  tm_shape(us_counties) + tm_borders()+
  tm_layout(frame=FALSE,component.autoscale = F)+
  tm_title("Years Since Last Hurricane: End of 2008",
           size=3.5)
  

# map for 2012

sub_df <- subset(scanner, year == 2012)

sub_df <- sub_df %>% group_by(fips) %>%
  summarize(
    years_since_landfall = tail(years_since_landfall, 1L)
  )

sub_df <- na.omit(sub_df)

image_df <- us_counties %>% filter(GEOID10 %in% scanner$fips)

image_df <- left_join(image_df, sub_df, by = c('GEOID10' = 'fips'))
image_df <- subset(image_df, !is.na(years_since_landfall))


# years since last landfall in Dec 2012
tm_shape(us_counties) +
  tm_shape(image_df) +
  tm_polygons(fill = 'years_since_landfall', 
              fill.scale = tm_scale_intervals(
                values = "mako",
                label.style = "discrete",
                breaks = c(0,1,2,3,4,5, 25),
                label.format = tm_label_format(digits = 1)
                
              ),
              fill.legend = tm_legend(
                title = "Years",
                title.size = 1.5,
                text.size = 1.2,
                na.show = FALSE,
                position = tm_pos_out("right")
              )
  )+
  tm_shape(us_counties) + tm_borders()+
  tm_layout(frame=FALSE,component.autoscale = F)+
  tm_title("Years Since Last Hurricane: End of 2012",
           size=3.5)



# map for 2016

sub_df <- subset(scanner, year == 2016)

sub_df <- sub_df %>% group_by(fips) %>%
  summarize(
    years_since_landfall = tail(years_since_landfall, 1L)
  )

sub_df <- na.omit(sub_df)

image_df <- us_counties %>% filter(GEOID10 %in% scanner$fips)

image_df <- left_join(image_df, sub_df, by = c('GEOID10' = 'fips'))

image_df <- subset(image_df, !is.na(years_since_landfall))


# map of years since last landfall in Dec 2016
tm_shape(us_counties) +
  tm_shape(image_df) +
  tm_polygons(fill = 'years_since_landfall', 
              fill.scale = tm_scale_intervals(
                values = "mako",
                label.style = "discrete",
                breaks = c(0,1,2,3,4,5, 25),
                label.format = tm_label_format(digits = 1)
                
              ),
              fill.legend = tm_legend(
                title = "Years",
                title.size = 1.5,
                text.size = 1.2,
                na.show = FALSE,
                position = tm_pos_out("right")
              )
  )+
  tm_shape(us_counties) + tm_borders()+
  tm_layout(frame=FALSE,component.autoscale = F)+
  tm_title("Years Since Last Hurricane: End of 2016",
           size=3.5)





# add in demographics variables ------------------------------------------------

# temperature
weather <- read.csv('Data\\weekly_weather.csv')

weather <- weather %>% mutate(
  fips = ifelse(fips == 12025, 12086, fips),
  week_end = as.Date(week_end),
  fips = str_pad(fips, 5,'left', '0')
)

weather<- weather[,c(1,2,6)]


scanner <- left_join(scanner, weather, by =c("fips", "week_end"))


# Income
inc <- read.csv('Data\\rep_income.csv')

inc <- inc %>% mutate(
  fips = ifelse(fips == 12025, 12086, fips),
  fips = str_pad(fips, 5,'left', '0')
)

scanner <- left_join(scanner , inc, by = "fips")


# Race
race <- read.csv('Data\\rep_race.csv')

race <- race %>% mutate(
  fips = ifelse(fips == 12025, 12086, fips),
  fips = str_pad(fips, 5,'left', '0')
)


scanner <- left_join(scanner , race, by = "fips")

# population

pop <- read.csv('Data\\County_pop.csv')

pop <- pop %>% mutate(
  state = str_pad(state, 2,'left', '0'),
 county = str_pad(county, 3,'left', '0'),
 fips = str_c(state, county, sep = ""),
  fips = ifelse(fips == 12025, 12086, fips),
  fips = str_pad(fips, 5,'left', '0')
)

pop <- pop[,c(8,37)]


scanner <- left_join(scanner , pop, by = "fips")

scanner <- scanner %>% mutate(
  total_rev_per_cap = total_rev/CENSUS2010POP * 100000
)



# map of average weekly sales per county
weekly_sales <- scanner %>% group_by(fips)%>%
  summarise(
    sales = mean(total_rev)
  )


image_df <- left_join(image_df, weekly_sales, by  = c('GEOID10' = 'fips'))

tm_shape(us_counties) +
  tm_shape(image_df) + 
  tm_polygons(fill = 'sales', 
              fill.scale = tm_scale_intervals(
                values = "heat",
                label.style = "continuous",
               # breaks = c(0,1,2,3,4,5, 25),
                label.format = tm_label_format(digits = 0)
                
              ),
              fill.legend = tm_legend(
                title = "Average Weekly Sales",
                title.size = 1.5,
                text.size = 1.2,
                na.show = FALSE,
                position = tm_pos_out("right")
              )
  )+
  tm_shape(us_counties) + tm_borders()+
  tm_layout(frame=FALSE,component.autoscale = F)+
  tm_title("Sales of Bottled Water: 2008-2019",
           size=4)


scanner <- scanner %>% mutate(
  major_hur = ifelse(Wind >= 96, 1, 0)
)

# save base dataframes

fwrite(scanner, "Data/scanner_with_hur.csv", row.names = F)
scanner <- fread("Data/scanner_with_hur.csv")


