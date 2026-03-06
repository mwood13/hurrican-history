# testing branching

# Create Experience and Recency variables.

# Scare and clean HURDAT 2 data
library(pacman)

library(doParallel)
cl<- makeCluster(8)
registerDoParallel(cl)

p_load(tidyverse, stringr, viridis, reshape2, jtools,foreach,
       data.table, dtplyr, lubridate, plm, estimatr, fixest,
       sf, reshape2, ggmosaic, GISTools, tmap, ggthemes, huxtable, stargazer)

# Get HURDAT Variables ----
# Load in historical hurricanes

HURDAT <- fread("Data//HURDAT2.csv")


# filter for landfalls after 1970
HURDAT <- HURDAT %>% filter(Year >= 1978)
HURDAT<- HURDAT %>% filter(Record_identifier == "L")

# save data

# filter for before Nielsen starts
HURDAT <- HURDAT %>% filter(Year < 2008)

# get dates in a usable format
HURDAT <- HURDAT %>% mutate(
  Month =  str_pad(Month, 2,'left', '0'),
  Day = str_pad(Day, 2, 'left', '0'),
  date = paste(Year, Month, sep = "-"),
  date = paste(date, Day, sep = "-"),
  date = as.Date(date)
)

date = unique(HURDAT$date)

# create data frame with counties as rows and landfall as column.
us_counties <- tigris::counties(state = c('01', '12', '13',
                                          '22', '28','37', '45',
                                          '48'), year = 2010)

us_counties <- us_counties %>% mutate(
  fips = paste(STATEFP10, COUNTYFP10, sep= "")
)

landfall_df <- data.frame(matrix(ncol=4,nrow=length(date)*length(us_counties$NAME10), 
                                            dimnames=list(NULL, c("fips", "County", "Date", 
                                                                  "Landfall"
                                            ))))


landfall_df <- landfall_df %>% mutate(
  fips = rep(us_counties$fips, each = length(date)),
  County = rep(us_counties$NAME10, each = length(date)),
  Date = rep(date, length(us_counties$NAME10))
)

# converst HURDAT into a spatial file.
coord <- HURDAT[, c('Longitude', 'Latitude')]
data <- HURDAT#[,c(1:5)]
crs <- CRS("+init=EPSG:4269")
weather_t <- SpatialPointsDataFrame(coords = coord, data = data, proj4string = crs)

# convert spdf to sf
HURDAT_sf <- st_as_sf(weather_t)

# Make a 100 nautical mile buffer for each points
HURDAT_buf <- st_buffer(HURDAT_sf, dist=185200)

land_vec <- c()

land_vec <- foreach( i = 1:length(landfall_df$fips),
 # i = 1:length(landfall_df$fips), 
                    #.combine=c, 
                    .packages="sf"
                    )%dopar%{
  
  df_s <- subset(HURDAT_buf, date == landfall_df$Date[i])
  df_c <- subset(us_counties, fips == landfall_df$fips[i])
  
  res <- as.integer(st_intersects(df_s, df_c))
  res <- ifelse(is.na(res), 0, 1)
  res <- ifelse(sum(as.integer(res))>0, 1, 0)
  #landfall_df$Landfall[i] <- res
  
  #print(i)
  
  return(res)
  
               
}

stopCluster(cl)

landfall_res <- unlist(land_vec)

landfall_df$Landfall <- landfall_res


rm( HURDAT_buf)
gc()

landfall_df <- subset(landfall_df, Landfall==1)

hist_df <- landfall_df %>% group_by(fips)%>% summarize(
  total_hist_landfall = sum(Landfall)
)

library(data.table)
fwrite(hist_df , "Data\\Historical Landfall Count.csv", row.names = F)


# get most recent landfall


recent_df <- landfall_df %>% group_by(fips) %>% summarize(
  recent_landfall = tail(Date, 1L)
)

land_type <- HURDAT[,c(17,31)]

recent_df <- left_join(recent_df, land_type, by = c("recent_landfall" = "date"))
recent_df <- rename(recent_df, "Last_Wind" = "Maximum_sustained_wind_in_knots")

recent_df <- unique(recent_df)

recent_df <- recent_df %>% group_by(fips) %>% summarize(
  recent_landfall = head(recent_landfall,1L),
  Last_Wind = max(Last_Wind)
)

fwrite(recent_df, "Data\\Recent Landfall Date.csv", row.names = F)

# Maps of historical and recent info.

hist_image_df <- left_join(us_counties, hist_df, by = "fips") 
hist_image_df <- hist_image_df %>% mutate(
  total_hist_landfall = ifelse(is.na(total_hist_landfall), 0, total_hist_landfall) 
)

hist_image_df <- subset(hist_image_df, total_hist_landfall > 0)

scanner <- fread('Data\\water_scanner.csv')

scanner <- scanner %>% mutate(
  fips = ifelse(fips == 12025, 12086, fips),
  week_end = as.Date((gsub("(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3", week_end))),
  fips = str_pad(fips, 5,'left', '0')
)

hist_image_df <- subset(hist_image_df, fips %in% scanner$fips)

quantile(hist_image_df$total_hist_landfall, probs = c(0, 0.33, 0.67, 1))
# terciles are 6, 12, and 21

rm(scanner)
gc()

tm_shape(us_counties)+tm_borders()+
  tm_shape(hist_image_df)+
  tm_fill(col = 'total_hist_landfall', title = "Landfall Count", palette = 'mako',
          breaks = c(1,6,11,16,21),
          labels = c("1 to 6", "7 to 11", "12 to 16", "17 to 21"))+
  tm_borders()+
  tm_layout(frame=FALSE, legend.outside = F, main.title = "Landfalls by County: 1980 - 2007",
          main.title.size=2.5, legend.title.size = 1.5, legend.text.size = 1.5)


