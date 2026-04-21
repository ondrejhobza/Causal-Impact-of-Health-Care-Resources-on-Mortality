# nolint start

# ! 02_TWFE_ANALYSIS.R — FD-TWFE only (S1 = treatments; S2+ add one control each)

rm(list = ls())

library(fixest)
library(plm)
library(lmtest)
library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)


select <- dplyr::select
filter <- dplyr::filter

project_root <- if (dir.exists("Data") && dir.exists("Output")) "." else if (dir.exists(file.path("..", "Data")) && dir.exists(file.path("..", "Output"))) ".." else "."
data_file <- file.path(project_root, "Data", "Final Data", "Filtered_Thesis_Data.csv")
twfe_output_dir <- file.path(project_root, "Output", "Final", "TWFE")
dir.create(twfe_output_dir, recursive = TRUE, showWarnings = FALSE)
twfe_output_file <- function(name) file.path(twfe_output_dir, name)

theme_simple <- theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", linewidth = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.ticks = element_line(color = "black"),
    legend.position = "bottom"
  )


# 1. LOAD DATA
# --------------------------------------------------------------------------- #

data <- as.data.frame(read_csv(
  data_file,
  show_col_types = FALSE
))

cat("N =", n_distinct(data$country_code), "countries, T =",
    length(unique(data$year)), "years\n")
cat("Rows:", nrow(data), "\n\n")


# 2. MAIN MODELS — FD-TWFE (first differences + year FE)
#    S1: treatments only (doctors, health expenditure).
#    S2–S8: add one control per column (nurses, beds, GDP, …, unemployment).
# --------------------------------------------------------------------------- #

# * LOG INFANT MORTALITY

fd_inf_s1 <- feols(
  d(log_infant_mortality) ~ d(doctors_per_10k) + d(log_health_exp) |
    year,
  data = data, panel.id = ~country_code + year
)

fd_inf_s2 <- feols(
  d(log_infant_mortality) ~ d(doctors_per_10k) + d(log_health_exp) +
    d(nursing_per_10k) |
    year,
  data = data, panel.id = ~country_code + year
)

fd_inf_s3 <- feols(
  d(log_infant_mortality) ~ d(doctors_per_10k) + d(log_health_exp) +
    d(nursing_per_10k) + d(hospital_beds_per_1k) |
    year,
  data = data, panel.id = ~country_code + year
)

fd_inf_s4 <- feols(
  d(log_infant_mortality) ~ d(doctors_per_10k) + d(log_health_exp) +
    d(nursing_per_10k) + d(hospital_beds_per_1k) +
    d(log_gdp) |
    year,
  data = data, panel.id = ~country_code + year
)

fd_inf_s5 <- feols(
  d(log_infant_mortality) ~ d(doctors_per_10k) + d(log_health_exp) +
    d(nursing_per_10k) + d(hospital_beds_per_1k) +
    d(log_gdp) + d(fertility_rate) |
    year,
  data = data, panel.id = ~country_code + year
)

fd_inf_s6 <- feols(
  d(log_infant_mortality) ~ d(doctors_per_10k) + d(log_health_exp) +
    d(nursing_per_10k) + d(hospital_beds_per_1k) +
    d(log_gdp) + d(fertility_rate) + d(urban_population_pct) |
    year,
  data = data, panel.id = ~country_code + year
)

fd_inf_s7 <- feols(
  d(log_infant_mortality) ~ d(doctors_per_10k) + d(log_health_exp) +
    d(nursing_per_10k) + d(hospital_beds_per_1k) +
    d(log_gdp) + d(fertility_rate) + d(urban_population_pct) +
    d(population_growth_pct) |
    year,
  data = data, panel.id = ~country_code + year
)

fd_inf_s8 <- feols(
  d(log_infant_mortality) ~ d(doctors_per_10k) + d(log_health_exp) +
    d(nursing_per_10k) + d(hospital_beds_per_1k) +
    d(log_gdp) + d(fertility_rate) + d(urban_population_pct) +
    d(population_growth_pct) + d(unemployment_rate) |
    year,
  data = data, panel.id = ~country_code + year
)

# * LOG UNDER-5 MORTALITY

fd_u5_s1 <- feols(
  d(log_under5_mortality) ~ d(doctors_per_10k) + d(log_health_exp) |
    year,
  data = data, panel.id = ~country_code + year
)

fd_u5_s2 <- feols(
  d(log_under5_mortality) ~ d(doctors_per_10k) + d(log_health_exp) +
    d(nursing_per_10k) |
    year,
  data = data, panel.id = ~country_code + year
)

fd_u5_s3 <- feols(
  d(log_under5_mortality) ~ d(doctors_per_10k) + d(log_health_exp) +
    d(nursing_per_10k) + d(hospital_beds_per_1k) |
    year,
  data = data, panel.id = ~country_code + year
)

fd_u5_s4 <- feols(
  d(log_under5_mortality) ~ d(doctors_per_10k) + d(log_health_exp) +
    d(nursing_per_10k) + d(hospital_beds_per_1k) +
    d(log_gdp) |
    year,
  data = data, panel.id = ~country_code + year
)

fd_u5_s5 <- feols(
  d(log_under5_mortality) ~ d(doctors_per_10k) + d(log_health_exp) +
    d(nursing_per_10k) + d(hospital_beds_per_1k) +
    d(log_gdp) + d(fertility_rate) |
    year,
  data = data, panel.id = ~country_code + year
)

fd_u5_s6 <- feols(
  d(log_under5_mortality) ~ d(doctors_per_10k) + d(log_health_exp) +
    d(nursing_per_10k) + d(hospital_beds_per_1k) +
    d(log_gdp) + d(fertility_rate) + d(urban_population_pct) |
    year,
  data = data, panel.id = ~country_code + year
)

fd_u5_s7 <- feols(
  d(log_under5_mortality) ~ d(doctors_per_10k) + d(log_health_exp) +
    d(nursing_per_10k) + d(hospital_beds_per_1k) +
    d(log_gdp) + d(fertility_rate) + d(urban_population_pct) +
    d(population_growth_pct) |
    year,
  data = data, panel.id = ~country_code + year
)

fd_u5_s8 <- feols(
  d(log_under5_mortality) ~ d(doctors_per_10k) + d(log_health_exp) +
    d(nursing_per_10k) + d(hospital_beds_per_1k) +
    d(log_gdp) + d(fertility_rate) + d(urban_population_pct) +
    d(population_growth_pct) + d(unemployment_rate) |
    year,
  data = data, panel.id = ~country_code + year
)

# * LOG MATERNAL MORTALITY

fd_mat_s1 <- feols(
  d(log_maternal_mortality) ~ d(doctors_per_10k) + d(log_health_exp)  |
    year,
  data = data, panel.id = ~country_code + year
)

fd_mat_s2 <- feols(
  d(log_maternal_mortality) ~ d(doctors_per_10k) + d(log_health_exp) +
    d(nursing_per_10k) |
    year,
  data = data, panel.id = ~country_code + year
)

fd_mat_s3 <- feols(
  d(log_maternal_mortality) ~ d(doctors_per_10k) + d(log_health_exp) +
    d(nursing_per_10k) + d(hospital_beds_per_1k) |
    year,
  data = data, panel.id = ~country_code + year
)


fd_mat_s4 <- feols(
  d(log_maternal_mortality) ~ d(doctors_per_10k) + d(log_health_exp) +
    d(nursing_per_10k) + d(hospital_beds_per_1k) +
    d(log_gdp) |
    year,
  data = data, panel.id = ~country_code + year
)

fd_mat_s5 <- feols(
  d(log_maternal_mortality) ~ d(doctors_per_10k) + d(log_health_exp) +
    d(nursing_per_10k) + d(hospital_beds_per_1k) +
    d(log_gdp) + d(fertility_rate) |
    year,
  data = data, panel.id = ~country_code + year
)

fd_mat_s6 <- feols(
  d(log_maternal_mortality) ~ d(doctors_per_10k) + d(log_health_exp) +
    d(nursing_per_10k) + d(hospital_beds_per_1k) +
    d(log_gdp) + d(fertility_rate) + d(urban_population_pct) |
    year,
  data = data, panel.id = ~country_code + year
)

fd_mat_s7 <- feols(
  d(log_maternal_mortality) ~ d(doctors_per_10k) + d(log_health_exp) +
    d(nursing_per_10k) + d(hospital_beds_per_1k) +
    d(log_gdp) + d(fertility_rate) + d(urban_population_pct) +
    d(population_growth_pct) |
    year,
  data = data, panel.id = ~country_code + year
)

fd_mat_s8 <- feols(
  d(log_maternal_mortality) ~ d(doctors_per_10k) + d(log_health_exp) +
    d(nursing_per_10k) + d(hospital_beds_per_1k) +
    d(log_gdp) + d(fertility_rate) + d(urban_population_pct) +
    d(population_growth_pct) + d(unemployment_rate) |
    year,
  data = data, panel.id = ~country_code + year
)

# ? MAIN RESULTS TABLES — DK SEs (FD-TWFE)
# --------------------------------------------------------------------------- #

spec_headers <- c("S1:Treat", "S2:+GDP", "S3:+Fert", "S4:+Urban", "S5:+PopGr", "S6:+Unemp", "S7:+PopGr+Unemp", "S8:Full")

etable(
  fd_inf_s1, fd_inf_s2, fd_inf_s3, fd_inf_s4, fd_inf_s5, fd_inf_s6, fd_inf_s7, fd_inf_s8,
  vcov = "DK",
  headers = spec_headers,
  title = "Main — FD-TWFE: Log Infant Mortality"
)

etable(
  fd_inf_s1, fd_inf_s2, fd_inf_s3, fd_inf_s4, fd_inf_s5, fd_inf_s6, fd_inf_s7, fd_inf_s8,
  vcov = "DK",
  headers = spec_headers,
  title = "Main — FD-TWFE: Log Infant Mortality", 
  caption = "FD-TWFE: Log Infant Mortality",
  label = "tab:fd_twfe_infant",
  tex = TRUE,
  file = twfe_output_file("02_twfe_log_infant_mortality.tex"),
  replace = TRUE,
  adjustbox = TRUE,
  create_dirs = TRUE
)


etable(
  fd_u5_s1, fd_u5_s2, fd_u5_s3, fd_u5_s4, fd_u5_s5, fd_u5_s6, fd_u5_s7, fd_u5_s8,
  vcov = "DK",
  headers = spec_headers,
  title = "Main — FD-TWFE: Log Under-5 Mortality"
)

etable(
  fd_u5_s1, fd_u5_s2, fd_u5_s3, fd_u5_s4, fd_u5_s5, fd_u5_s6, fd_u5_s7, fd_u5_s8,
  vcov = "DK",
  headers = spec_headers,
  title = "Main — FD-TWFE: Log Under-5 Mortality", 
  caption = "FD-TWFE: Log Under-5 Mortality",
  label = "tab:fd_twfe_u5",
  tex = TRUE,
  file = twfe_output_file("02_twfe_log_under5_mortality.tex"),
  replace = TRUE,
  adjustbox = TRUE,
  create_dirs = TRUE
)

etable(
  fd_mat_s1, fd_mat_s2, fd_mat_s3, fd_mat_s4, fd_mat_s5, fd_mat_s6, fd_mat_s7, fd_mat_s8,
  vcov = "DK",
  headers = spec_headers,
  title = "Main — FD-TWFE: Log Maternal Mortality"
)

etable(
  fd_mat_s1, fd_mat_s2, fd_mat_s3, fd_mat_s4, fd_mat_s5, fd_mat_s6, fd_mat_s7, fd_mat_s8,
  vcov = "DK",
  headers = spec_headers,
  title = "Main — FD-TWFE: Log Maternal Mortality", 
  caption = "FD-TWFE: Log Maternal Mortality",
  label = "tab:fd_twfe_maternal",
  tex = TRUE,
  file = twfe_output_file("02_twfe_log_maternal_mortality.tex"),
  replace = TRUE,
  adjustbox = TRUE,
  create_dirs = TRUE
)

# RESULTS TO CSV (main FD coefficients on policy treatments)
# --------------------------------------------------------------------------- #

treatment_vars <- c("d(doctors_per_10k)", "d(log_health_exp)")

all_models <- list(
  list(model = fd_inf_s1, outcome = "d_log_infant_mortality", spec = "S1"),
  list(model = fd_inf_s2, outcome = "d_log_infant_mortality", spec = "S2"),
  list(model = fd_inf_s3, outcome = "d_log_infant_mortality", spec = "S3"),
  list(model = fd_inf_s4, outcome = "d_log_infant_mortality", spec = "S4"),
  list(model = fd_inf_s5, outcome = "d_log_infant_mortality", spec = "S5"),
  list(model = fd_inf_s6, outcome = "d_log_infant_mortality", spec = "S6"),
  list(model = fd_inf_s7, outcome = "d_log_infant_mortality", spec = "S7"),
  list(model = fd_inf_s8, outcome = "d_log_infant_mortality", spec = "S8"),
  list(model = fd_u5_s1,  outcome = "d_log_under5_mortality", spec = "S1"),
  list(model = fd_u5_s2,  outcome = "d_log_under5_mortality", spec = "S2"),
  list(model = fd_u5_s3,  outcome = "d_log_under5_mortality", spec = "S3"),
  list(model = fd_u5_s4,  outcome = "d_log_under5_mortality", spec = "S4"),
  list(model = fd_u5_s5,  outcome = "d_log_under5_mortality", spec = "S5"),
  list(model = fd_u5_s6,  outcome = "d_log_under5_mortality", spec = "S6"),
  list(model = fd_u5_s7,  outcome = "d_log_under5_mortality", spec = "S7"),
  list(model = fd_u5_s8,  outcome = "d_log_under5_mortality", spec = "S8"),
  list(model = fd_mat_s1, outcome = "d_log_maternal_mortality", spec = "S1"),
  list(model = fd_mat_s2, outcome = "d_log_maternal_mortality", spec = "S2"),
  list(model = fd_mat_s3, outcome = "d_log_maternal_mortality", spec = "S3"),
  list(model = fd_mat_s4, outcome = "d_log_maternal_mortality", spec = "S4"),
  list(model = fd_mat_s5, outcome = "d_log_maternal_mortality", spec = "S5"),
  list(model = fd_mat_s6, outcome = "d_log_maternal_mortality", spec = "S6"),
  list(model = fd_mat_s7, outcome = "d_log_maternal_mortality", spec = "S7"),
  list(model = fd_mat_s8, outcome = "d_log_maternal_mortality", spec = "S8")
)

results_list <- list()

for (i in seq_along(all_models)) {
  m   <- all_models[[i]]
  fit <- m$model
  
  ct_dk <- coeftable(fit, vcov = "DK")
  p_col_dk <- grep("^Pr\\(", colnames(ct_dk), value = TRUE)[1]
  
  ct_cl <- coeftable(fit, vcov = ~country_code)
  p_col_cl <- grep("^Pr\\(", colnames(ct_cl), value = TRUE)[1]
  
  r2w <- tryCatch(as.numeric(fitstat(fit, "wr2")), error = function(e) NA_real_)
  r2_ov <- tryCatch(as.numeric(fitstat(fit, "r2")), error = function(e) NA_real_)
  
  for (term in treatment_vars) {
    if (!(term %in% rownames(ct_dk))) next
    
    est   <- as.numeric(ct_dk[term, "Estimate"])
    se_dk <- as.numeric(ct_dk[term, "Std. Error"])
    p_dk  <- as.numeric(ct_dk[term, p_col_dk])
  
    
    results_list[[length(results_list) + 1L]] <- data.frame(
      outcome      = m$outcome,
      spec         = m$spec,
      term         = term,
      estimate     = est,
      se_dk        = se_dk,
      p_dk         = p_dk,
      ci_low_dk    = est - 1.96 * se_dk,
      ci_high_dk   = est + 1.96 * se_dk,
      n_obs        = as.numeric(nobs(fit)),
      r2           = r2_ov,
      r2_within    = r2w,
      stringsAsFactors = FALSE
    )
  }
}

results <- do.call(rbind, results_list)
write_csv(results, twfe_output_file("02_twfe_results.csv"))


# # 5. ROBUSTNESS — LEVELS TWFE (country + year FE; series often non-stationary)
# # --------------------------------------------------------------------------- #

# m_inf_s1 <- feols(
#   log_infant_mortality ~ doctors_per_10k + log_health_exp +
#     nursing_per_10k + hospital_beds_per_1k |
#     country_code + year,
#   data = data, panel.id = ~country_code + year
# )

# m_inf_s2 <- feols(
#   log_infant_mortality ~ doctors_per_10k + log_health_exp +
#     nursing_per_10k + hospital_beds_per_1k +
#     log_gdp |
#     country_code + year,
#   data = data, panel.id = ~country_code + year
# )

# m_inf_s3 <- feols(
#   log_infant_mortality ~ doctors_per_10k + log_health_exp +
#     nursing_per_10k + hospital_beds_per_1k +
#     log_gdp + fertility_rate |
#     country_code + year,
#   data = data, panel.id = ~country_code + year
# )

# m_inf_s4 <- feols(
#   log_infant_mortality ~ doctors_per_10k + log_health_exp +
#     nursing_per_10k + hospital_beds_per_1k +
#     log_gdp + fertility_rate + urban_population_pct |
#     country_code + year,
#   data = data, panel.id = ~country_code + year
# )

# m_inf_s5 <- feols(
#   log_infant_mortality ~ doctors_per_10k + log_health_exp +
#     nursing_per_10k + hospital_beds_per_1k +
#     log_gdp + fertility_rate + urban_population_pct +
#     population_growth_pct |
#     country_code + year,
#   data = data, panel.id = ~country_code + year
# )

# m_inf_s6 <- feols(
#   log_infant_mortality ~ doctors_per_10k + log_health_exp +
#     nursing_per_10k + hospital_beds_per_1k +
#     log_gdp + fertility_rate + urban_population_pct +
#     population_growth_pct + unemployment_rate |
#     country_code + year,
#   data = data, panel.id = ~country_code + year
# )

# m_u5_s1 <- feols(
#   log_under5_mortality ~ doctors_per_10k + log_health_exp +
#     nursing_per_10k + hospital_beds_per_1k |
#     country_code + year,
#   data = data, panel.id = ~country_code + year
# )

# m_u5_s2 <- feols(
#   log_under5_mortality ~ doctors_per_10k + log_health_exp +
#     nursing_per_10k + hospital_beds_per_1k +
#     log_gdp |
#     country_code + year,
#   data = data, panel.id = ~country_code + year
# )

# m_u5_s3 <- feols(
#   log_under5_mortality ~ doctors_per_10k + log_health_exp +
#     nursing_per_10k + hospital_beds_per_1k +
#     log_gdp + fertility_rate |
#     country_code + year,
#   data = data, panel.id = ~country_code + year
# )

# m_u5_s4 <- feols(
#   log_under5_mortality ~ doctors_per_10k + log_health_exp +
#     nursing_per_10k + hospital_beds_per_1k +
#     log_gdp + fertility_rate + urban_population_pct |
#     country_code + year,
#   data = data, panel.id = ~country_code + year
# )

# m_u5_s5 <- feols(
#   log_under5_mortality ~ doctors_per_10k + log_health_exp +
#     nursing_per_10k + hospital_beds_per_1k +
#     log_gdp + fertility_rate + urban_population_pct +
#     population_growth_pct |
#     country_code + year,
#   data = data, panel.id = ~country_code + year
# )

# m_u5_s6 <- feols(
#   log_under5_mortality ~ doctors_per_10k + log_health_exp +
#     nursing_per_10k + hospital_beds_per_1k +
#     log_gdp + fertility_rate + urban_population_pct +
#     population_growth_pct + unemployment_rate |
#     country_code + year,
#   data = data, panel.id = ~country_code + year
# )

# m_mat_s1 <- feols(
#   log_maternal_mortality ~ doctors_per_10k + log_health_exp +
#     nursing_per_10k + hospital_beds_per_1k |
#     country_code + year,
#   data = data, panel.id = ~country_code + year
# )

# m_mat_s2 <- feols(
#   log_maternal_mortality ~ doctors_per_10k + log_health_exp +
#     nursing_per_10k + hospital_beds_per_1k +
#     log_gdp |
#     country_code + year,
#   data = data, panel.id = ~country_code + year
# )

# m_mat_s3 <- feols(
#   log_maternal_mortality ~ doctors_per_10k + log_health_exp +
#     nursing_per_10k + hospital_beds_per_1k +
#     log_gdp + fertility_rate |
#     country_code + year,
#   data = data, panel.id = ~country_code + year
# )

# m_mat_s4 <- feols(
#   log_maternal_mortality ~ doctors_per_10k + log_health_exp +
#     nursing_per_10k + hospital_beds_per_1k +
#     log_gdp + fertility_rate + urban_population_pct |
#     country_code + year,
#   data = data, panel.id = ~country_code + year
# )

# m_mat_s5 <- feols(
#   log_maternal_mortality ~ doctors_per_10k + log_health_exp +
#     nursing_per_10k + hospital_beds_per_1k +
#     log_gdp + fertility_rate + urban_population_pct +
#     population_growth_pct |
#     country_code + year,
#   data = data, panel.id = ~country_code + year
# )

# m_mat_s6 <- feols(
#   log_maternal_mortality ~ doctors_per_10k + log_health_exp +
#     nursing_per_10k + hospital_beds_per_1k +
#     log_gdp + fertility_rate + urban_population_pct +
#     population_growth_pct + unemployment_rate |
#     country_code + year,
#   data = data, panel.id = ~country_code + year
# )

# etable(
#   m_inf_s1, m_inf_s2, m_inf_s3, m_inf_s4, m_inf_s5, m_inf_s6,
#   vcov = "DK",
#   headers = spec_headers,
#   title = "Robustness — Levels TWFE: Log Infant Mortality"
# )

# etable(
#   m_u5_s1, m_u5_s2, m_u5_s3, m_u5_s4, m_u5_s5, m_u5_s6,
#   vcov = "DK",
#   headers = spec_headers,
#   title = "Robustness — Levels TWFE: Log Under-5 Mortality"
# )

# etable(
#   m_mat_s1, m_mat_s2, m_mat_s3, m_mat_s4, m_mat_s5, m_mat_s6,
#   vcov = "DK",
#   headers = spec_headers,
#   title = "Robustness — Levels TWFE: Log Maternal Mortality"
# )

# etable(
#   fd_inf_s6, m_inf_s6, fd_u5_s6, m_u5_s6, fd_mat_s6, m_mat_s6,
#   vcov = "DK",
#   headers = c("Infant FD", "Infant Levels",
#               "Under-5 FD", "Under-5 Levels",
#               "Maternal FD", "Maternal Levels"),
#   title = "S6 comparison: Main (FD-TWFE) vs robustness (Levels TWFE)"
# )



# ? INTERACTION MODELS (FD main; levels = robustness)
# --------------------------------------------------------------------------- #

m_int_skillmix_inf_FD <- feols(
  d(log_infant_mortality) ~ d(doctors_per_10k) * d(nursing_per_10k) +
    d(log_health_exp) + d(hospital_beds_per_1k) + d(log_gdp) +
    d(fertility_rate) + d(urban_population_pct) +
    d(population_growth_pct) + d(unemployment_rate) |
    year,
  data = data, panel.id = ~country_code + year
)

m_int_spend_inf_FD <- feols(
  d(log_infant_mortality) ~ d(doctors_per_10k) * d(log_health_exp) +
    d(nursing_per_10k) + d(hospital_beds_per_1k) + d(log_gdp) +
    d(fertility_rate) + d(urban_population_pct) +
    d(population_growth_pct) + d(unemployment_rate) |
    year,
  data = data, panel.id = ~country_code + year
)

etable(
  m_int_skillmix_inf_FD, m_int_spend_inf_FD,
  vcov = "DK",
  headers = c("Doctors × Nurses", "Doctors × Health Exp"),
  title = "Main — Interactions (FD-TWFE): Log Infant Mortality"
)

etable(
  m_int_skillmix_inf_FD, m_int_spend_inf_FD,
  vcov = "DK",
  headers = c("Doctors × Nurses", "Doctors × Health Exp"),
  title = "Interactions (FD-TWFE): Log Infant Mortality", 
  caption = "Interactions (FD-TWFE): Log Infant Mortality",
  label = "tab:fd_twfe_infant_interactions",
  tex = TRUE,
  file = twfe_output_file("02_twfe_interactions_infant.tex"),
  replace = TRUE,
  adjustbox = TRUE,
  create_dirs = TRUE
)

m_int_skillmix_u5_FD <- feols(
  d(log_under5_mortality) ~ d(doctors_per_10k) * d(nursing_per_10k) +
    d(log_health_exp) + d(hospital_beds_per_1k) + d(log_gdp) +
    d(fertility_rate) + d(urban_population_pct) +
    d(population_growth_pct) + d(unemployment_rate) |
    year,
  data = data, panel.id = ~country_code + year
)

m_int_spend_u5_FD <- feols(
  d(log_under5_mortality) ~ d(doctors_per_10k) * d(log_health_exp) +
    d(nursing_per_10k) + d(hospital_beds_per_1k) + d(log_gdp) +
    d(fertility_rate) + d(urban_population_pct) +
    d(population_growth_pct) + d(unemployment_rate) |
    year,
  data = data, panel.id = ~country_code + year
)

etable(
  m_int_skillmix_u5_FD, m_int_spend_u5_FD,
  vcov = "DK",
  headers = c("Doctors × Nurses", "Doctors × Health Exp"),
  title = "Main — Interactions (FD-TWFE): Log Under-5 Mortality"
)

etable(
  m_int_skillmix_u5_FD, m_int_spend_u5_FD,
  vcov = "DK",
  headers = c("Doctors × Nurses", "Doctors × Health Exp"),
  title = "Interactions (FD-TWFE): Log Under-5 Mortality", 
  caption = "Interactions (FD-TWFE): Log Under-5 Mortality",
  label = "tab:fd_twfe_u5_interactions",
  tex = TRUE,
  file = twfe_output_file("02_twfe_interactions_u5.tex"),
  replace = TRUE,
  adjustbox = TRUE,
  create_dirs = TRUE
)

m_int_skillmix_mat_FD <- feols(
  d(log_maternal_mortality) ~ d(doctors_per_10k) * d(nursing_per_10k) +
    d(log_health_exp) + d(hospital_beds_per_1k) + d(log_gdp) +
    d(fertility_rate) + d(urban_population_pct) +
    d(population_growth_pct) + d(unemployment_rate) |
    year,
  data = data, panel.id = ~country_code + year
)

m_int_spend_mat_FD <- feols(
  d(log_maternal_mortality) ~ d(doctors_per_10k) * d(log_health_exp) +
    d(nursing_per_10k) + d(hospital_beds_per_1k) + d(log_gdp) +
    d(fertility_rate) + d(urban_population_pct) +
    d(population_growth_pct) + d(unemployment_rate) |
    year,
  data = data, panel.id = ~country_code + year
)

etable(
  m_int_skillmix_mat_FD, m_int_spend_mat_FD,
  vcov = "DK",
  headers = c("Doctors × Nurses", "Doctors × Health Exp"),
  title = "Main — Interactions (FD-TWFE): Log Maternal Mortality"
)

etable(
  m_int_skillmix_mat_FD, m_int_spend_mat_FD,
  vcov = "DK",
  headers = c("Doctors × Nurses", "Doctors × Health Exp"),
  title = "Interactions (FD-TWFE): Log Maternal Mortality", 
  caption = "Interactions (FD-TWFE): Log Maternal Mortality",
  label = "tab:fd_twfe_maternal_interactions",
  tex = TRUE,
  file = twfe_output_file("02_twfe_interactions_maternal.tex"),
  replace = TRUE,
  adjustbox = TRUE,
  create_dirs = TRUE
)

# m_int_skillmix_inf <- feols(
#   log_infant_mortality ~ doctors_per_10k * nursing_per_10k +
#     log_health_exp + hospital_beds_per_1k + log_gdp + fertility_rate +
#     urban_population_pct + population_growth_pct + unemployment_rate |
#     country_code + year,
#   data = data, panel.id = ~country_code + year
# )

# m_int_spend_inf <- feols(
#   log_infant_mortality ~ doctors_per_10k * log_health_exp +
#     nursing_per_10k + hospital_beds_per_1k + log_gdp + fertility_rate +
#     urban_population_pct + population_growth_pct + unemployment_rate |
#     country_code + year,
#   data = data, panel.id = ~country_code + year
# )

# etable(
#   m_int_skillmix_inf, m_int_spend_inf,
#   vcov = "DK",
#   headers = c("Doctors × Nurses", "Doctors × Health Exp"),
#   title = "Robustness — Interactions (Levels TWFE): Log Infant Mortality"
# )


# ? RESIDUAL DIAGNOSTICS (FD-TWFE S6 — main specification)
# --------------------------------------------------------------------------- #

get_resid_df <- function(model, data_used, outcome_label) {
  idx <- fixest::obs(model)
  
  df_model <- data_used[idx, c("country_code", "year"), drop = FALSE]
  df_model$fitted  <- as.numeric(fitted(model))
  df_model$resid   <- as.numeric(resid(model))
  df_model$outcome <- outcome_label
  
  df_model
}

res_inf <- get_resid_df(fd_inf_s6, data, "Log Infant Mortality")
res_u5  <- get_resid_df(fd_u5_s6,  data, "Log Under-5 Mortality")
res_mat <- get_resid_df(fd_mat_s6, data, "Log Maternal Mortality")

res_all <- bind_rows(res_inf, res_u5, res_mat)

# 1. Residuals vs fitted
p_fit <- ggplot(res_all, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  facet_wrap(~outcome, scales = "free_x") +
  labs(
    title = "FD-TWFE S6: residuals vs fitted",
    x = "Fitted value",
    y = "FD Residual"
  ) +
  theme_simple

ggsave(twfe_output_file("02_twfe_resid_vs_fitted.png"),
       p_fit, width = 14, height = 5, dpi = 300)

# 2. Residuals vs year
p_time <- ggplot(res_all, aes(x = year, y = resid)) +
  geom_point(alpha = 0.25, size = 1) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  facet_wrap(~outcome, scales = "free_y") +
  labs(
    title = "FD-TWFE S6: residuals vs year",
    x = "Year",
    y = "Residual"
  ) +
  theme_simple

ggsave(twfe_output_file("02_twfe_resid_vs_year.png"),
       p_time, width = 14, height = 5, dpi = 300)

# 3. Country-average residuals
country_resid <- res_all %>%
  group_by(outcome, country_code) %>%
  summarise(mean_resid = mean(resid, na.rm = TRUE), .groups = "drop")

p_country <- ggplot(country_resid, aes(x = reorder(country_code, mean_resid), y = mean_resid)) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  facet_wrap(~outcome, scales = "free_y") +
  coord_flip() +
  labs(
    title = "FD-TWFE S8: mean residual by country",
    x = "Country",
    y = "Mean residual"
  ) +
  theme_simple

ggsave(twfe_output_file("02_twfe_country_mean_residuals.png"),
       p_country, width = 12, height = 12, dpi = 300)

# 4. Simple ACF plots on pooled residuals
acf_plot_fn <- function(x, title_text) {
  acf_obj <- acf(x, lag.max = 15, plot = FALSE)
  acf_df  <- data.frame(lag = acf_obj$lag[-1], acf = acf_obj$acf[-1])
  ci_bound <- 1.96 / sqrt(length(x))
  
  ggplot(acf_df, aes(x = lag, y = acf)) +
    geom_hline(yintercept = 0, color = "grey40") +
    geom_hline(yintercept = c(-ci_bound, ci_bound), linetype = "dashed", color = "blue") +
    geom_segment(aes(x = lag, xend = lag, y = 0, yend = acf)) +
    geom_point(size = 2) +
    labs(title = title_text, x = "Lag", y = "ACF") +
    theme_simple
}

p_acf_inf <- acf_plot_fn(res_inf$resid, "Infant Mortality")
p_acf_u5  <- acf_plot_fn(res_u5$resid,  "Under-5 Mortality")
p_acf_mat <- acf_plot_fn(res_mat$resid, "Maternal Mortality")

p_acf <- p_acf_inf + p_acf_u5 + p_acf_mat +
  plot_annotation(
    title = "FD-TWFE S8: ACF of residuals",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
  )

ggsave(twfe_output_file("02_twfe_residual_acf.png"),
       p_acf, width = 14, height = 5, dpi = 300)

# nolint end