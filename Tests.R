# nolint start

# ! 01_TEST.R 

rm(list = ls())

library(plm)
library(fixest)
library(lmtest)
library(sandwich)
library(dplyr)
library(readr)
library(tidyr)
library(ggrepel)
library(scales)

select <- dplyr::select
filter <- dplyr::filter

setwd("/Users/ondrejhobza/Documents/Studies/VSE/Bc. Thesis")

theme_simple <- theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(color = "black", linewidth = 0.5),
    plot.title       = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle    = element_text(hjust = 0.5, size = 10, color = "grey40"),
    axis.ticks       = element_line(color = "black"),
    legend.position  = "bottom"
  )

# ! LOAD DATA
# --------------------------------------------------------------------------- #

data <- read_csv(
  "Data/Final Data/Final_Thesis_Data_2000_2021.csv",
  show_col_types = FALSE
)


# ! FILTER TO ANALYSIS SAMPLE
# --------------------------------------------------------------------------- #

data <- data %>%
  filter(
    income_group %in% c("Low income", "Lower middle income", "Upper middle income"),
    year >= 2000,
    year <= 2018
  ) %>%
  mutate(
    log_infant_mortality   = log(infant_mortality_rate),
    log_under5_mortality   = log(under5_mortality_rate),
    log_maternal_mortality = log(maternal_mortality),
    log_doctors            = log(doctors_per_10k),
    log_nursing            = log(nursing_per_10k),
    log_hospital_beds      = log(hospital_beds_per_1k),
    log_health_exp         = log(health_expenditure_ppp),
    log_gdp                = log(gdp_per_capita)
  ) %>%
  arrange(country_code, year) %>%
  group_by(country_code) %>%
  mutate(
    .no_mortality = all(is.na(infant_mortality_rate)) & all(is.na(under5_mortality_rate)) & all(is.na(maternal_mortality)),
    .no_doctors   = all(is.na(doctors_per_10k)),
    .no_health_exp = all(is.na(health_expenditure_ppp)),
  ) %>%
  ungroup() %>%
  filter(!.no_doctors, !.no_health_exp, !.no_mortality)


pdata <- pdata.frame(data, index = c("country_code", "year"))

cat("Countries :", n_distinct(data$country_code), "\n")
cat("Years     :", n_distinct(data$year), "\n")
cat("Obs       :", nrow(data), "\n\n")

write_csv(data, "Data/Final Data/Filtered_Thesis_Data.csv")

# ! PANEL STRUCTURE AND MISSINGNESS
# --------------------------------------------------------------------------- #

# Panel Structure
print(pdim(pdata))

# Missingness
miss_tbl <- data.frame(
  variable = names(data),
  n_missing = colSums(is.na(data)),
  pct_missing = round(100 * colMeans(is.na(data)), 2),
  row.names = NULL
)

print(miss_tbl[order(-miss_tbl$pct_missing), ], row.names = FALSE)
write_csv(miss_tbl, "Output/Final/Tests/01_missingness_summary.csv")


print(summary(pdata))

# ? Data Coverage by WHO Region 
# --------------------------------------------------------------------------- #

data_coverage_by_region <- pdata %>%
  group_by(who_region) %>%
  summarise(
    Countries     = n_distinct(country_code),
    Obs           = n(),
    Doctors_pct   = round(100 * mean(!is.na(doctors_per_10k)), 1),
    Nurses_pct    = round(100 * mean(!is.na(nursing_per_10k)), 1),
    Health_Exp_pct = round(100 * mean(!is.na(health_expenditure_ppp)), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(Obs))

total_data_coverage <- pdata %>%
  summarise(
    who_region     = "Total",
    Countries      = n_distinct(country_code),
    Obs            = n(),
    Doctors_pct    = round(100 * mean(!is.na(doctors_per_10k)), 1),
    Nurses_pct     = round(100 * mean(!is.na(nursing_per_10k)), 1),
    Health_Exp_pct = round(100 * mean(!is.na(health_expenditure_ppp)), 1),
  )

data_coverage_by_region <- bind_rows(data_coverage_by_region, total_data_coverage)

print(as.data.frame(data_coverage_by_region), row.names = FALSE)
write_csv(data_coverage_by_region, "Output/Final/Tests/01_coverage_by_region.csv")


# ? Data Coverage by Year
# --------------------------------------------------------------------------- #

N_total <- n_distinct(data$country_code)
selected_years <- c(2000, 2005, 2010, 2015, 2018, 2021)

data_coverage_by_year <- pdata %>%
  filter(year %in% selected_years) %>%
  group_by(year) %>%
  summarise(
    Doctors_pct    = round(100 * sum(!is.na(doctors_per_10k)) / N_total, 1),
    Nurses_pct     = round(100 * sum(!is.na(nursing_per_10k)) / N_total, 1),
    Health_Exp_pct = round(100 * sum(!is.na(health_expenditure_ppp)) / N_total, 1),
    .groups = "drop"
  )

print(as.data.frame(data_coverage_by_year), row.names = FALSE)
write_csv(data_coverage_by_year, "Output/Final/Tests/01_coverage_by_year.csv")

# ? Panel Observation Grid
# --------------------------------------------------------------------------- #

obs_grid <- data %>%
  mutate(observed = ifelse(
    !is.na(log_infant_mortality) & !is.na(log_doctors), 1, 0
  )) %>%
  select(country_code, year, observed)

p_balance <- ggplot(obs_grid, aes(x = year, y = reorder(country_code, -year), fill = factor(observed))) +
  geom_tile(color = "white", linewidth = 0.1) +
  scale_fill_manual(values = c("0" = "grey90", "1" = "steelblue"),
                    labels = c("Missing", "Observed"), name = NULL) +
  labs(title = "Panel Observation Matrix",
       subtitle = "Each row is a country; blue = observed in that year",
       x = "Year", y = NULL) +
  theme_simple +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

ggsave("Output/Final/Tests/01_panel_balance.png", p_balance,
       width = 10, height = 12, dpi = 300)

# ? Treatment variable distributions
# --------------------------------------------------------------------------- #

treat_levels <- data %>%
  select(doctors_per_10k, nursing_per_10k, hospital_beds_per_1k, health_expenditure_ppp) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  mutate(transform = "Levels")

p_dist_levels <- ggplot(treat_levels, aes(x = value)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white", alpha = 0.8) +
  facet_wrap(~variable, scales = "free", ncol = 4) +
  labs(title = "Treatment Variables in Levels",
       subtitle = "Right skew motivates log transformation",
       x = NULL, y = "Count") +
  theme_simple

ggsave("Output/Final/Tests/01_treatment_distributions.png", p_dist_levels,
       width = 14, height = 8, dpi = 300)


# ? Variable skewness
# --------------------------------------------------------------------------- #

mortality_long <- data %>%
  select(infant_mortality_rate, under5_mortality_rate, maternal_mortality) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(variable = recode(variable,
    "infant_mortality_rate" = "Infant Mortality Rate",
    "under5_mortality_rate" = "Under-5 Mortality Rate",
    "maternal_mortality"    = "Maternal Mortality Ratio"
  ))

p_skew_mortality <- ggplot(mortality_long, aes(x = value)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white", alpha = 0.8) +
  facet_wrap(~variable, scales = "free", ncol = 3) +
  labs(
    title = "Outcome Variables: Distribution",
    x = "Value", y = "Frequency"
  ) +
  theme_simple

ggsave("Output/Final/Tests/01_mortality_skewness.png", p_skew_mortality,
       width = 14, height = 5, dpi = 300)

# ? Income group composition
# --------------------------------------------------------------------------- #
income_tbl <- data %>%
  distinct(country_code, .keep_all = TRUE) %>%
  count(income_group, sort = TRUE) %>%
  mutate(income_group = reorder(income_group, n))

print(income_tbl)

# ? Spaghetti plots
# --------------------------------------------------------------------------- #

income_colors <- c(
  "Low income"          = "#B22028",
  "Lower middle income" = "#4A7FB5",
  "Upper middle income" = "#999999"
)

who_palette <- c("#E41A1C", "#4DAF4A", "#377EB8", "#FF7F00", "#984EA3", "#A65628", "#F781BF")
who_levels  <- sort(unique(na.omit(data$who_region)))
who_colors  <- setNames(who_palette[seq_along(who_levels)], who_levels)

spaghetti <- function(df, var, group_var, colors, title, ylab, n_outliers = 5) {
  legend_title <- if (group_var == "income_group") "Income Group" else "WHO Region"
  
  d <- df %>%
    select(country_code, country_name, year,
           grp = all_of(group_var), value = all_of(var)) %>%
    filter(!is.na(value), !is.na(grp))
  
  tops <- d %>%
    group_by(country_code, country_name) %>%
    summarise(m = mean(value), .groups = "drop") %>%
    slice_max(m, n = n_outliers)
  
  labs_df <- d %>%
    filter(country_code %in% tops$country_code) %>%
    group_by(country_code, country_name) %>%
    filter(year == max(year)) %>%
    ungroup()
  
  ggplot(d, aes(x = year, y = value, group = country_code, color = grp)) +
    geom_line(alpha = 0.25, linewidth = 0.4) +
    stat_summary(aes(group = grp), fun = mean, geom = "line", linewidth = 1.4) +
    stat_summary(aes(group = grp), fun = mean, geom = "point",
                 size = 2.5, shape = 21, fill = "white", stroke = 0.8,
                 show.legend = FALSE) +
    geom_text_repel(data = labs_df, aes(label = country_name),
                    size = 3, fontface = "italic", direction = "y",
                    nudge_x = 0.5, hjust = 0, segment.size = 0.3,
                    max.overlaps = 20, show.legend = FALSE) +
    scale_color_manual(values = colors, name = legend_title) +
    guides(fill = "none") +
    labs(title = title, x = "Year", y = ylab) +
    theme_simple
}

vars <- list(
  list(var = "infant_mortality_rate",  title = "Infant Mortality Rate (2000-2018)",       ylab = "Deaths per 1,000 live births",  tag = "infant_mortality"),
  list(var = "under5_mortality_rate",  title = "Under-5 Mortality Rate (2000-2018)",      ylab = "Deaths per 1,000 live births",  tag = "under5_mortality"),
  list(var = "maternal_mortality",     title = "Maternal Mortality Ratio (2000-2018)",     ylab = "Deaths per 100,000 live births", tag = "maternal_mortality"),
  list(var = "doctors_per_10k",        title = "Doctors per 10k (2000-2018)",           ylab = "Doctors per 10,000 population", tag = "doctors"),
  list(var = "nursing_per_10k",        title = "Nursing & Midwifery per 10k (2000-2018)", ylab = "Nurses per 10,000 population",  tag = "nursing"),
  list(var = "hospital_beds_per_1k",   title = "Hospital Beds per 1k (2000-2018)",        ylab = "Beds per 1,000 population",     tag = "beds"),
  list(var = "health_expenditure_ppp", title = "Health Expenditure PPP (2000-2018)",       ylab = "PPP$ per capita",               tag = "health_exp"),
  list(var = "gdp_per_capita",         title = "GDP per Capita (2000-2018)",               ylab = "PPP$ per capita",               tag = "gdp"),
  list(var = "fertility_rate",         title = "Fertility Rate (2000-2018)",               ylab = "Births per woman",              tag = "fertility"),
  list(var = "urban_population_pct",   title = "Urban Population (2000-2018)",             ylab = "% of total population",         tag = "urban",       n_out = 3),
  list(var = "population_growth_pct",  title = "Population Growth (2000-2018)",            ylab = "Annual %",                      tag = "popgrowth"),
  list(var = "unemployment_rate",      title = "Unemployment Rate (2000-2018)",            ylab = "% of labor force",              tag = "unemployment")
)

# Generate panel strips
inc_plots <- list()
who_plots <- list()

for (v in vars) {
  n_out <- ifelse(is.null(v$n_out), 5, v$n_out)
  
  inc_plots[[length(inc_plots) + 1]] <- spaghetti(data, v$var, "income_group", income_colors, v$title, v$ylab, n_out) +
    theme(legend.position = "none")
  
  who_plots[[length(who_plots) + 1]] <- spaghetti(data, v$var, "who_region", who_colors, v$title, v$ylab, n_out) +
    theme(legend.position = "none")
}

# Income group panels
for (j in seq(1, length(inc_plots), by = 3)) {
  idx <- j:min(j + 2, length(inc_plots))
  strip <- wrap_plots(inc_plots[idx], ncol = 3, nrow = 1, guides = "collect") &
    theme(legend.position = "bottom")
  ggsave(sprintf("Output/Final/Tests/01_spag_inc_panel_%02d.png", ceiling(j / 3)),
         strip, width = 20, height = 6, dpi = 300)
}

# WHO region panels
for (j in seq(1, length(who_plots), by = 3)) {
  idx <- j:min(j + 2, length(who_plots))
  strip <- wrap_plots(who_plots[idx], ncol = 3, nrow = 1, guides = "collect") &
    theme(legend.position = "bottom")
  ggsave(sprintf("Output/Final/Tests/01_spag_who_panel_%02d.png", ceiling(j / 3)),
         strip, width = 20, height = 6, dpi = 300)
}


# ! CROSS-SECTIONAL DEPENDENCE
# ? H0: cross-sectional independence
# ? H1: cross-sectional dependence
# --------------------------------------------------------------------------- #

run_cd_var <- function(df, varname, label, min_countries_per_year = 10) {
  tmp <- df %>%
    select(country_code, year, all_of(varname)) %>%
    na.omit()
  
  if (nrow(tmp) == 0L || n_distinct(tmp$country_code) < 2L || n_distinct(tmp$year) < 3L) {
    cat("\n", label, "\nSkipped: insufficient usable panel variation.\n", sep = "")
    return(data.frame(
      variable = varname,
      statistic = NA_real_,
      p_value = NA_real_,
      n_obs = nrow(tmp),
      row.names = NULL
    ))
  }
  
  test <- tryCatch(
    pcdtest(
      as.formula(paste0(varname, " ~ 1")),
      data = tmp,
      index = c("country_code", "year"),
      test = "cd"
    ),
    error = function(e) e
  )
  
  cat("\n", label, "\n", sep = "")
  print(test)
  
  if (inherits(test, "error")) {
    return(data.frame(
      variable = varname,
      statistic = NA_real_,
      p_value = NA_real_,
      n_obs = nrow(tmp),
      row.names = NULL
    ))
  }
  
  data.frame(
    variable = varname,
    statistic = as.numeric(test$statistic),
    p_value = as.numeric(test$p.value),
    n_obs = nrow(tmp),
    row.names = NULL
  )
}

cd_results <- bind_rows(
  run_cd_var(data, "log_infant_mortality",   "Log infant mortality"),
  run_cd_var(data, "log_under5_mortality",   "Log under-5 mortality"),
  run_cd_var(data, "log_maternal_mortality", "Log maternal mortality"),
  run_cd_var(data, "log_doctors",            "Log doctors"),
  run_cd_var(data, "log_nursing",            "Log nursing"),
  run_cd_var(data, "log_hospital_beds",      "Log hospital beds"),
  run_cd_var(data, "log_health_exp",         "Log health expenditure"),
  run_cd_var(data, "log_gdp",                "Log GDP per capita"),
  run_cd_var(data, "fertility_rate",         "Fertility rate"),
  run_cd_var(data, "urban_population_pct",   "Urban population pct"),
  run_cd_var(data, "population_growth_pct",  "Population growth pct"),
  run_cd_var(data, "unemployment_rate",      "Unemployment rate")
)

print(cd_results)
write_csv(cd_results, "Output/Final/Tests/01_cd_tests_variables.csv")

# ! CIPS UNIT ROOT TESTS
# ? H0: all panels contain a unit root I(1)
# ? H1: a fraction of panels is stationary
# --------------------------------------------------------------------------- #

level_vars <- c(
  "log_infant_mortality", "log_under5_mortality", "log_maternal_mortality",
  "log_doctors", "log_nursing", "log_hospital_beds",
  "log_health_exp", "log_gdp", "fertility_rate",
  "urban_population_pct", "population_growth_pct", "unemployment_rate"
)

pdf_data <- pdata.frame(
  data %>% select(country_code, year, all_of(level_vars)),
  index = c("country_code", "year")
)

# LEVELS — DRIFT

cips_stats_drift <- rep(NA_real_, length(level_vars))
cips_pvals_drift <- rep(NA_real_, length(level_vars))

for (i in seq_along(level_vars)) {
  v <- level_vars[i]
  cat("\n---", v, "[drift] ---\n")
  
  test <- tryCatch(
    cipstest(pdf_data[[v]], lags = 1L, type = "drift", model = "cmg", truncated = TRUE),
    error = function(e) { cat("ERROR:", e$message, "\n"); NULL }
  )
  
  if (!is.null(test)) {
    print(test)
    cips_stats_drift[i] <- as.numeric(test$statistic)
    cips_pvals_drift[i] <- as.numeric(test$p.value)
  }
}

cips_results <- data.frame(
  variable  = level_vars,
  CIPS_stat = cips_stats_drift,
  p_value   = cips_pvals_drift
)


print(cips_results)
write_csv(cips_results, "Output/Final/Tests/01_cips_results.csv")


# LEVELS — TREND

cips_stats_trend <- rep(NA_real_, length(level_vars))
cips_pvals_trend <- rep(NA_real_, length(level_vars))

for (i in seq_along(level_vars)) {
  v <- level_vars[i]
  cat("\n---", v, "[trend] ---\n")
  
  test <- tryCatch(
    cipstest(pdf_data[[v]], lags = 1L, type = "trend", model = "cmg", truncated = TRUE),
    error = function(e) { cat("ERROR:", e$message, "\n"); NULL }
  )
  
  if (!is.null(test)) {
    print(test)
    cips_stats_trend[i] <- as.numeric(test$statistic)
    cips_pvals_trend[i] <- as.numeric(test$p.value)
  }
}

cips_trend_results <- data.frame(
  variable  = level_vars,
  CIPS_stat = cips_stats_trend,
  p_value   = cips_pvals_trend
)

print(cips_trend_results)
write_csv(cips_trend_results, "Output/Final/Tests/01_cips_results_trend.csv")


# ! CIPS ON FIRST DIFFERENCES
# --------------------------------------------------------------------------- #

data_fd <- data %>%
  arrange(country_code, year) %>%
  group_by(country_code) %>%
  mutate(
    d_log_infant_mortality   = log_infant_mortality - lag(log_infant_mortality),
    d_log_under5_mortality   = log_under5_mortality - lag(log_under5_mortality),
    d_log_maternal_mortality = log_maternal_mortality - lag(log_maternal_mortality),
    d_log_doctors            = log_doctors - lag(log_doctors),
    d_nursing_per_10k        = log_nursing - lag(log_nursing),
    d_hospital_beds_per_1k   = log_hospital_beds - lag(log_hospital_beds),
    d_log_health_exp         = log_health_exp - lag(log_health_exp),
    d_log_gdp                = log_gdp - lag(log_gdp),
    d_fertility_rate         = fertility_rate - lag(fertility_rate),
    d_urban_population_pct   = urban_population_pct - lag(urban_population_pct),
    d_population_growth_pct  = population_growth_pct - lag(population_growth_pct),
    d_unemployment_rate      = unemployment_rate - lag(unemployment_rate)
  ) %>%
  ungroup()

pdf_fd <- pdata.frame(data_fd, index = c("country_code", "year"))

fd_test_vars <- c(
  "d_log_infant_mortality", "d_log_under5_mortality", "d_log_maternal_mortality",
  "d_log_doctors", "d_nursing_per_10k", "d_hospital_beds_per_1k",
  "d_log_health_exp", "d_log_gdp", "d_fertility_rate",
  "d_urban_population_pct", "d_population_growth_pct", "d_unemployment_rate"
)

cips_lags <- 2L
cips_stats <- rep(NA_real_, length(fd_test_vars))
cips_pvals <- rep(NA_real_, length(fd_test_vars))

for (i in seq_along(fd_test_vars)) {
  v <- fd_test_vars[i]
  cat("\n---", v, "---\n")
  
  test <- tryCatch(
    cipstest(pdf_fd[[v]], lags = cips_lags, type = "none", model = "cmg", truncated = TRUE),
    error = function(e) { cat("ERROR:", e$message, "\n"); NULL }
  )
  
  if (!is.null(test)) {
    print(test)
    cips_stats[i] <- as.numeric(test$statistic)
    cips_pvals[i] <- as.numeric(test$p.value)
  }
}

# SUMMARY
cips_fd_results <- data.frame(
  variable  = fd_test_vars,
  CIPS_stat = cips_stats,
  p_value   = cips_pvals
)

print(cips_fd_results)

write_csv(cips_fd_results, "Output/Final/Tests/01_cips_fd_results.csv")


# ---------- Second difference (I(2)) ----------
data_fd2 <- data_fd %>%
  arrange(country_code, year) %>%
  group_by(country_code) %>%
  mutate(
    d2_log_infant_mortality   = d_log_infant_mortality   - lag(d_log_infant_mortality),
    d2_log_under5_mortality   = d_log_under5_mortality   - lag(d_log_under5_mortality),
    d2_log_maternal_mortality = d_log_maternal_mortality  - lag(d_log_maternal_mortality),
    d2_log_doctors            = d_log_doctors             - lag(d_log_doctors),
    d2_nursing_per_10k        = d_nursing_per_10k         - lag(d_nursing_per_10k),
    d2_hospital_beds_per_1k   = d_hospital_beds_per_1k   - lag(d_hospital_beds_per_1k),
    d2_log_health_exp         = d_log_health_exp          - lag(d_log_health_exp),
    d2_log_gdp                = d_log_gdp                 - lag(d_log_gdp),
    d2_fertility_rate         = d_fertility_rate           - lag(d_fertility_rate),
    d2_urban_population_pct   = d_urban_population_pct    - lag(d_urban_population_pct),
    d2_population_growth_pct  = d_population_growth_pct   - lag(d_population_growth_pct),
    d2_unemployment_rate      = d_unemployment_rate        - lag(d_unemployment_rate)
  ) %>%
  ungroup()

pdf_fd2 <- pdata.frame(data_fd2, index = c("country_code", "year"))

fd2_test_vars <- c(
  "d2_log_infant_mortality", "d2_log_under5_mortality", "d2_log_maternal_mortality",
  "d2_log_doctors", "d2_nursing_per_10k", "d2_hospital_beds_per_1k",
  "d2_log_health_exp", "d2_log_gdp", "d2_fertility_rate",
  "d2_urban_population_pct", "d2_population_growth_pct", "d2_unemployment_rate"
)

cips_lags2 <- 2L
cips_stats2 <- rep(NA_real_, length(fd2_test_vars))
cips_pvals2 <- rep(NA_real_, length(fd2_test_vars))

for (i in seq_along(fd2_test_vars)) {
  v <- fd2_test_vars[i]
  cat("\n---", v, "---\n")
  
  test <- tryCatch(
    cipstest(pdf_fd2[[v]], lags = cips_lags2, type = "none", model = "cmg", truncated = TRUE),
    error = function(e) { cat("ERROR:", e$message, "\n"); NULL }
  )
  
  if (!is.null(test)) {
    print(test)
    cips_stats2[i] <- as.numeric(test$statistic)
    cips_pvals2[i] <- as.numeric(test$p.value)
  }
}

cips_fd2_results <- data.frame(
  variable  = fd2_test_vars,
  CIPS_stat = cips_stats2,
  p_value   = cips_pvals2
)

print(cips_fd2_results)
write_csv(cips_fd2_results, "Output/Final/Tests/01_cips_fd2_results.csv")

# nolint end