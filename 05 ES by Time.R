# ======================================
# SCRIPT 5: RECENCY EVENT STUDIES
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
  month = month(week_end),
  minor_hur = as.integer(wind >= 64 & wind < 96),
  total_hist_landfall = past_25_h1_5 # from Script 2
)]

setorder(scanner, fips, week_end)

scanner <- scanner[substr(fips,1,2) != "51"]
scanner <- scanner[total_hist_landfall > 0]

# -----------------------------
# 2. RECENCY DISTRIBUTION
# -----------------------------
sub_df <- scanner[landfall == 1 & wind >= 64]

print(quantile(sub_df$years_since_landfall,
               probs = c(0,0.25,0.5,0.75,1),
               na.rm = TRUE))

ggplot(sub_df, aes(x = years_since_landfall))+
  geom_histogram(binwidth = 1, color="black",
                 fill="grey", alpha=0.5)+
  theme_minimal()+
  labs(title = "Distribution of Years Since Last Hurricane Landfall",
       x = "Years Since Last Hurricane Landfall",
       y = "Count")+
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        plot.title = element_text(size = 30))

# -----------------------------
# 3. HELPER FUNCTION
# -----------------------------
run_es <- function(low, high, label) {
  
  df <- get_event_data(scanner, "years",
                       c(low, high),
                       c(64,200))
  
  model <- feols(
    total_rev_per_cap ~ ref_num + temp_mean |
       fips +year + month,
    data = df,
    cluster = ~fips
  )
  
  make_ES_table(model, label)
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
    theme(
      axis.title = element_text(size = 25),
      axis.text = element_text(size = 20)
    )+
    labs(
      title = " ",
      x = "Weeks",
      y = "Total Revenue per 100K Residents"
    )
}

# -----------------------------
# 4. QUARTILE EVENT STUDIES
# -----------------------------
q1 <- run_es(0, 3, " 0 to 3 years") # lower bound >, upper <=
q2 <- run_es(3, 6, " 4 to 6 years")
q3 <- run_es(6, 12, " 7 to 12 years")
q4 <- run_es(12, Inf, "13 or more years")

plot_es(q1)
plot_es(q2)
plot_es(q3)
plot_es(q4)

all_results <- rbindlist(
  list(q1, q2, q3, q4),
  use.names = TRUE,
  fill = TRUE,
  ignore.attr = TRUE
)

# -----------------------------
# 5. +/- 1 WEEK COMPARISON
# -----------------------------
graph_df <- all_results[time %in% c(-1,0,1)]

setorder(graph_df, id, time)
graph_df[, x := seq_len(.N)]

ggplot(graph_df,
       aes(y = estimate,
           x = seq(1,12, by = 1))) + 
  geom_point(aes(color = id), size = 3)+
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin = lci, ymax = uci,
                    color = id), linewidth = 0.9)+
  theme_minimal()+
  scale_x_continuous(
    breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),
    labels=c("-1","0","1",
             "-1","0","1",
             "-1","0","1",
             "-1","0","1")
  )+
  theme(
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 20),
    plot.title = element_text(size = 30)
  )+
  labs(
    title = "",
    x = "Weeks",
    y = "Total Revenue per 100K Residents",
    color = "Years Since Last Hurricane"
  )

# -----------------------------
# 6. SELECTED YEARS (1–12)
# -----------------------------
years_vec <- c(1,2,3,4,5,7,9,12)

results_list <- lapply(years_vec, function(k) {
  
  df <- get_event_data(scanner, "years",
                       c(k-0.5, k+0.5),
                       c(64,200))
  
  model <- feols(
    total_rev_per_cap ~ ref_num + temp_mean + total_hist_landfall |
       year + month,
    data = df,
    cluster = ~fips
  )
  
  make_ES_table(model, paste(k, "years"))
})

all_results <- rbindlist(results_list,
                         use.names=TRUE,
                         fill=TRUE,
                         ignore.attr=TRUE)

# Week -1
graph_df <- all_results[time == -1]
graph_df[, yrs := as.integer(gsub(" years","", id))]
setorder(graph_df, yrs)

ggplot(graph_df,
       aes(y = estimate, x = seq(1,8, by = 1))) + 
  geom_point(aes(y = estimate), size = 3)+
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin = lci, ymax = uci), linewidth = 0.9)+
  theme_minimal()+
  scale_x_continuous(
    breaks=c(1,2,3,4,5,6,7,8),
    labels=c("1","2","3","4","5","7","9","12")
  )+
  theme(
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 20),
    plot.title = element_text(size = 30)
  )+
  labs(
    title = "",
    x = "Years Since Last Hurricane",
    y = "Total Revenue per 100K Residents"
  )


# Week 0
graph_df <- all_results[time == 0]

ggplot(graph_df,
       aes(y = estimate, x = seq(1,8, by = 1))) + 
  geom_point(aes(y = estimate), size = 3)+
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin = lci, ymax = uci), linewidth = 0.9)+
  theme_minimal()+
  scale_x_continuous(
    breaks=c(1,2,3,4,5,6,7,8),
    labels=c("1","2","3","4","5","7","9","12")
  )+
  theme(
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 20),
    plot.title = element_text(size = 30)
  )+
  labs(
    title = "",
    x = "Years Since Last Hurricane",
    y = "Total Revenue per 100K Residents"
  )
