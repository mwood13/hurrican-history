# ======================================
# SCRIPT 4: EVENT STUDIES BY EXPOSURE
# ======================================

library(data.table)
library(fixest)
library(ggplot2)
library(lubridate)

source("Functions.R")

# -----------------------------
# 1. LOAD DATA
# -----------------------------
scanner <- fread("Data/combined_scanner.csv")

scanner[, fips := sprintf("%05d", as.integer(fips))]
scanner[, week_end := as.Date(week_end)]

scanner[, `:=`(
  year = year(week_end),
  month = month(week_end)
)]

setorder(scanner, fips, week_end)

# Drop Virginia
scanner <- scanner[substr(fips,1,2) != "51"]

# -----------------------------
# 2. DEFINE EXPOSURE VARIABLE
# -----------------------------
# Use your Script 2 variable
scanner[, total_hist_landfall := base_25_h1_5]

# Remove zero-exposure counties
scanner <- scanner[total_hist_landfall > 0]

# -----------------------------
# 3. EXPOSURE DISTRIBUTION
# -----------------------------
graph_df <- scanner[landfall == 1 & wind >= 64,
                    .(total_hist_landfall = first(total_hist_landfall)),
                    by = fips]

print(quantile(graph_df$total_hist_landfall,
               probs = c(0,0.25,0.5,0.75,1), na.rm=TRUE))

ggplot(graph_df, aes(x=total_hist_landfall))+
  geom_histogram(binwidth = 1, color = "black",
                 fill = "grey", alpha = 0.5)+
  theme_minimal()+
  labs(title = "Distribution of Historical Landfalls",
       x = "Historical landfalls", y = "Count")+
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        plot.title = element_text(size = 30))


# -----------------------------
# 4. HELPER FUNCTION
# -----------------------------
run_es <- function(range_low, range_high, label) {
  
  df <- get_event_data(scanner, "total",
                       c(range_low, range_high),
                       c(64,200))
  
  model <- feols(
    total_rev_per_cap ~ ref_num + temp_mean |
      fips + year + month,
    data = df,
    cluster = ~fips
  )
  
  return(make_ES_table(model, label))
}

plot_es <- function(results) {
  ggplot(results, aes(x = time, y = estimate))+
    geom_line()+
    geom_point()+
    theme_minimal()+
    geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2)+
    geom_vline(xintercept = 0, linetype = "dashed")+
    geom_hline(yintercept = 0)+
    geom_point(aes(x = -2, y = 0),
               fill = "white", shape = 21, size = 2)+
    theme(axis.title = element_text(size = 25),
          axis.text = element_text(size = 20))+
    labs(x = "Weeks",
         y = "Total Revenue per 100K Residents")
}

# -----------------------------
# 5. QUARTILE EVENT STUDIES
# -----------------------------
q1_results <- run_es(0, 4, " 1 to 4 landfalls")
q2_results <- run_es(4, 6, " 5 to 6 landfalls")
q3_results <- run_es(6, 8, " 7 to 8 landfalls")
q4_results <- run_es(8, 13, " 9 to 13 landfalls")

plot_es(q1_results)
plot_es(q2_results)
plot_es(q3_results)
plot_es(q4_results)

# Combine safely
all_results <- rbindlist(
  list(q1_results, q2_results, q3_results, q4_results),
  fill = TRUE
)

# -----------------------------
# 6. +/- 1 WEEK PLOT
# -----------------------------
graph_df <- all_results[time %in% c(-1,0,1)]
graph_df[, x := seq_len(.N)]

ggplot(graph_df,
       aes(y = estimate,
           x = x)) +
  geom_point(aes(color = id), size = 3)+
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin = lci, ymax = uci,
                    color = id), linewidth = 0.9)+
  theme_minimal()+
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20))+
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                     labels=c("-1", "0", "1", "-1", "0", "1",
                              "-1", "0", "1", "-1", "0", "1"))+
  labs(x = "Weeks",
       y = "Total Revenue per 100K Residents",
       color = "Hurricanes from 1983 to 2007")



# -----------------------------
# 7. EVENT STUDY BY EXACT COUNT
# -----------------------------
counts <- 1:10

results_list <- lapply(counts, function(k) {
  
  df <- get_event_data(scanner, "total",
                       c(k-1, k),
                       c(64,200))
  
  model <- feols(
    total_rev_per_cap ~ ref_num + temp_mean |
      fips + year + month,
    data = df,
    cluster = ~fips
  )
  
  make_ES_table(model, paste(k, "landfalls"))
})

all_results <- rbindlist(results_list, fill = TRUE)

# Week -1
graph_df <- all_results[time == -1]

ggplot(graph_df,
       aes(y = estimate,
           x = seq_along(counts))) +
  geom_point(size = 3)+
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin = lci, ymax = uci),
                linewidth = 0.9)+
  theme_minimal()+
  scale_x_continuous(
    breaks = 1:length(counts),
    labels = counts
  )+
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20))+
  labs(x = "Historical Landfall Count",
       y = "Total Revenue per 100K Residents")


# Week 0
graph_df <- all_results[time == 0]

ggplot(graph_df,
       aes(y = estimate,
           x = seq_along(counts))) +
  geom_point(size = 3)+
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin = lci, ymax = uci),
                linewidth = 0.9)+
  theme_minimal()+
  scale_x_continuous(
    breaks = 1:length(counts),
    labels = counts
  )+
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20))+
  labs(x = "Historical Landfall Count",
       y = "Total Revenue per 100K Residents")
