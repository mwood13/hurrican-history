

library(data.table)
library(tmap)
library(sf)
library(tigris)

tmap_mode("plot")


# 1. LOAD DATA

scanner <- fread("Data/combined_scanner.csv")

us_counties <- counties(
  state = c('01', '12', '13', '22', '28','37', '45', '48'),
  year = 2010,
  class = "sf"
)

# FIX FIPS FORMAT
scanner[, fips := sprintf("%05d", as.integer(fips))]
us_counties$GEOID10 <- sprintf("%05d", as.integer(us_counties$GEOID10))



# 2. COUNTIES IN DATA

image_df <- us_counties[us_counties$GEOID10 %in% scanner$fips, ]

tm_shape(image_df) + 
  tm_fill("pink") +
  tm_shape(us_counties) + 
  tm_borders() +
  tm_layout(frame = FALSE) +
  tm_title("Counties Represented in Scanner Data", size = 4)



# 3. CURRENT LANDFALL MAP

sub <- scanner[, .(
  land = as.integer(sum(landfall) > 0 & sum(wind) >= 64)
), by = .(fips, week_end)]

sub <- sub[, .(
  total_land = sum(land)
), by = fips]

image_df <- merge(image_df, sub,
                  by.x = "GEOID10",
                  by.y = "fips",
                  all.x = TRUE)

im_df <- image_df[ which(image_df$total_land > 0), ]

tm_shape(us_counties)+tm_shape(im_df) + 
  tm_polygons(
    fill = "total_land", 
    fill.scale = tm_scale_discrete(
      values = "mako",
     # breaks = c(1,2,3,4),
      label.format = list(digits = 0)
    ),
    fill.legend = tm_legend(
      title = "Landfall Count",
      title.size = 1.5,
      text.size = 1.2,
      na.show = FALSE,
      position = tm_pos_out("right")
    )
  ) +
  tm_shape(us_counties) + tm_borders()+ 
  tm_layout(frame=FALSE,component.autoscale = FALSE)+
  tm_title("Hurricane Landfalls: 2008-2019", size=3.5)



# 4. HISTORICAL LANDFALL MAP

hist_df <- scanner[, .(
  hist = past_25_h1_5[1]
), by = fips]

image_hist <- merge(us_counties, hist_df,
                    by.x = "GEOID10",
                    by.y = "fips",
                    all.x = TRUE)


hist_df <- image_hist[ which(image_hist$hist > 0), ]

tm_shape(us_counties)+tm_borders()+
  tm_shape(hist_df)+
  tm_polygons(
    fill = "hist", 
    fill.scale = tm_scale_intervals(
      values = "mako",
      breaks = c(0,2,6,10,14),
      label.format = list(digits = 0)
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
  tm_layout(frame=FALSE,component.autoscale = FALSE)+
  tm_title("Hurricane Landfalls: 1983-2007", size=3.5)



# 5. YEARS SINCE LANDFALL MAPS

make_recency_map <- function(year_target, title_text) {
  
  sub_df <- scanner[
    as.integer(format(week_end, "%Y")) == year_target,
    .(years_since_landfall = tail(years_since_landfall, 1L)),
    by = fips
  ]
  
  sub_df <- sub_df[!is.na(years_since_landfall)]
  
  image_df <- merge(us_counties, sub_df,
                    by.x = "GEOID10",
                    by.y = "fips")
  
  tm_shape(us_counties) +
    tm_shape(image_df) +
    tm_polygons(
      fill = "years_since_landfall",
      fill.scale = tm_scale_intervals(
        values = "mako",
        breaks = c(0,1,3,5,7, 9,25),
        label.format = list(digits = 0)
      ),
      fill.legend = tm_legend(
        title = "Years",
        title.size = 1.5,
        text.size = 1.2,
        na.show = FALSE,
        position = tm_pos_out("right")
      )
    ) +
    tm_shape(us_counties) + tm_borders()+
    tm_layout(frame=FALSE,component.autoscale = FALSE)+
    tm_title(title_text, size=3.5)
}

make_recency_map(2008, "Years Since Last Hurricane: End of 2008")
make_recency_map(2012, "Years Since Last Hurricane: End of 2012")
make_recency_map(2016, "Years Since Last Hurricane: End of 2016")


# 6. SALES MAP

weekly_sales <- scanner[, .(
  per_cap_sales = mean(total_rev_per_cap),
  sales = mean(total_rev)
), by = fips]

image_sales <- merge(us_counties, weekly_sales,
                     by.x = "GEOID10",
                     by.y = "fips",
                     all.x = TRUE)

tm_shape(us_counties) +
  tm_shape(image_sales) + 
  tm_polygons(
    fill = "sales", 
    fill.scale = tm_scale_continuous(
      values = "heat",
      label.format = list(digits = 0)
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
  tm_layout(frame=FALSE,component.autoscale = FALSE)+
  tm_title("Sales of Bottled Water: 2008-2019", size=3.5)


tm_shape(us_counties) +
  tm_shape(image_sales) + 
  tm_polygons(
    fill = "per_cap_sales", 
    fill.scale = tm_scale_continuous_pseudo_log(
      values = "heat",
      label.format = list(digits = 0)
    ),
    fill.legend = tm_legend(
      title = "Sales Per Capita",
      title.size = 1.5,
      text.size = 1.2,
      na.show = FALSE,
      position = tm_pos_out("right")
    )
  )+
  tm_shape(us_counties) + tm_borders()+
  tm_layout(frame=FALSE,component.autoscale = FALSE)+
  tm_title("Sales of Bottled Water: 2008-2019", size=3.5)

