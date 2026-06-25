# Event Studies

library(data.table)
library(fixest)
library(ggplot2)
library(lubridate)

source("Functions.R")


# LOAD DATA

scanner <- fread("Data/combined_scanner.csv")

scanner[, fips := sprintf("%05d", as.integer(fips))]
scanner[, week_end := as.Date(week_end)]

scanner[, `:=`(
  year = year(week_end),
  month = month(week_end)
)]

setorder(scanner, fips, week_end)

# Remove Virginia
scanner <- scanner[substr(fips,1,2) != "51"]


# BUILD BASE EVENT DATA

base_df <- get_event_data(scanner, "none", c(0,100), c(0,200))


# MODEL + PLOT FUNCTIONS

run_es_model <- function(df, formula_str) {
  feols(as.formula(formula_str),
        data = df,
        cluster = ~fips)
}

plot_es <- function(df, ylab) {
  
  ggplot(df, aes(x = time, y = estimate)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = 0) +
    geom_point(aes(x = -2, y = 0),
               fill = "white", shape = 21, size = 2) +
    theme(
      axis.title = element_text(size = 25),
      axis.text = element_text(size = 20)
    ) +
    labs(x = "Weeks", y = ylab)
}


# MAIN EVENT STUDIES

outcomes <- list(
  list("total_rev_per_cap", "Total Revenue per 100K Residents"),
  list("log(total_rev_per_cap)", "Log Total Revenue per 100K Residents"),
  list("price", "Average Price"),
  list("log(price)", "Log Average Price"),
  list("total_vol/population*100000", "Total Ounces per 100K"),
  list("log(total_vol/population*100000)", "Log Total Ounces per 100K")
)

for (o in outcomes) {
  
  formula_str <- paste0(o[[1]], " ~ ref_num + temp_mean | fips + year + month")
  
  model <- run_es_model(base_df, formula_str)
  
  results <- make_ES_table(model, "baseline")
  
  print(plot_es(results, o[[2]]))
}


# CATEGORY EVENT STUDIES

run_category <- function(range, label, log = FALSE) {
  
  df <- get_event_data(scanner, "none", c(0,100), range)
  
  if (!log) {
    f <- "total_rev_per_cap ~ ref_num + temp_mean | fips + year + month"
  } else {
    f <- "log(total_rev_per_cap) ~ ref_num + temp_mean | fips + year + month"
  }
  
  model <- run_es_model(df, f)
  
  return(make_ES_table(model, label))
}

# Levels
ts_results    <- run_category(c(0,64), "Tropical Storm")
cat1_results  <- run_category(c(64,96), "Minor Hurricane")
major_results <- run_category(c(96,200), "Major Hurricane")

all_results <- rbind(ts_results, cat1_results, major_results)

# Plot combined
ggplot(all_results, aes(x = time, y = estimate, color = id)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5) +
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  geom_point(aes(x = -2, y = 0),
             fill = "white", shape = 21, size = 2) +
  theme(
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15)
  ) +
  labs(
    x = "Weeks",
    y = "Total Revenue per 100K Residents",
    color = "Storm Class"
  )


# Plot combined with error bars
ggplot(all_results, aes(x = time, y = estimate, color = id)) +
  geom_line(size = 0.7) +
  geom_point(size = 1.5) +
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  geom_point(aes(x = -2, y = 0),
             fill = "white", shape = 21, size = 2) +
  geom_errorbar(aes(ymin = lci, ymax = uci))+
  theme(
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15)
  ) +
  labs(
    x = "Weeks",
    y = "Total Revenue per 100K Residents",
    color = "Storm Class"
  )

# HISTOGRAM

scanner[, hur_cat :=
          fifelse(wind < 64, 0,
                  fifelse(wind < 83, 1,
                          fifelse(wind < 96, 2,
                                  fifelse(wind < 113, 3,
                                          fifelse(wind < 137, 4, 5)))))
]

plot_df <- scanner[landfall == 1]

plot_df <- plot_df[, .(hur_cat = hur_cat[1]), by = week_end]

ggplot(plot_df, aes(x = hur_cat)) +
  geom_histogram(binwidth = 1, color = "black",
                 fill = "grey", alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Distribution of Landfall Categories",
    x = "Hurricane Category",
    y = "Count"
  ) +
  theme(
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 20),
    plot.title = element_text(size = 30)
  )
