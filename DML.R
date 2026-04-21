# nolint start

# ! DML.R

rm(list = ls())

library(xtdml)
library(mlr3)
library(mlr3learners)
library(mlr3torch)
library(dplyr)
library(readr)
library(ggplot2)
library(fixest)
library(patchwork)
library(plm)
library(lmtest)
library(tidyr)

select <- dplyr::select
filter <- dplyr::filter

lgr::get_logger("mlr3")$set_threshold("warn")

set.seed(42)

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

# ? LOAD DATA
# --------------------------------------------------------------------------- #

data <- as.data.frame(read_csv(
  "Data/Final Data/Filtered_Thesis_Data.csv",
  show_col_types = FALSE
))

# Build year dummies for explicit year fixed effects in the FD model
year_dummies <- model.matrix(~ factor(year) - 1, data = data)
colnames(year_dummies) <- gsub("factor\\(year\\)", "year_", colnames(year_dummies))
data <- cbind(data, as.data.frame(year_dummies))
year_x <- colnames(year_dummies)

cat("N =", n_distinct(data$country_code), "countries, T =",
    length(unique(data$year)), "years\n")
cat("Rows:", nrow(data), "\n\n")


# ? SPECIFICATIONS
# --------------------------------------------------------------------------- #

controls_vars <- c(year_x, "nursing_per_10k", "hospital_beds_per_1k", "log_gdp", "fertility_rate", "urban_population_pct",
              "population_growth_pct", "unemployment_rate")

x_doctors <- c("log_health_exp", controls_vars)
x_hexp    <- c("doctors_per_10k", controls_vars)


# ? LEARNERS
# --------------------------------------------------------------------------- #

lrn_lasso <- lrn("regr.cv_glmnet", s = "lambda.min", alpha = 1)
lrn_enet  <- lrn("regr.cv_glmnet", s = "lambda.min", alpha = 0.5)
lrn_rf    <- lrn("regr.ranger", num.trees = 500, min.node.size = 5)

# Neural network: MLP via mlr3torch
lrn_nn <- as_learner(
  po("scale") %>>%
    lrn("regr.mlp",
        epochs        = 100,
        batch_size    = 64,
        neurons       = c(64, 32),
        p             = 0.2,
        optimizer     = t_opt("adam", lr = 1e-3),
        loss          = t_loss("mse"),
        predict_type  = "response")
)
lrn_nn$id <- "regr.mlp_pipe"


# ? LOOKUPS
# --------------------------------------------------------------------------- #

x_cols_lookup <- list(
  "doctors_per_10k" = x_doctors,
  "log_health_exp"  = x_hexp
)

learner_objs <- list(
  "LASSO"          = lrn_lasso,
  "Elastic Net"    = lrn_enet,
  "Random Forest"  = lrn_rf,
  "Neural Network" = lrn_nn
)

treat_label_map <- c(
  "doctors_per_10k" = "Doctors per 10k",
  "log_health_exp"  = "Log Health Exp."
)

outcome_label_map <- c(
  "log_infant_mortality"   = "Log Infant Mortality",
  "log_under5_mortality"   = "Log Under-5 Mortality",
  "log_maternal_mortality" = "Log Maternal Mortality"
)

# TWFE outcome names carry a `d_` prefix (first-difference) — map them to plot labels
twfe_outcome_label_map <- setNames(outcome_label_map, paste0("d_", names(outcome_label_map)))


# ? MODEL GRID
# --------------------------------------------------------------------------- #

learners <- tibble::tribble(
  ~learner_name,    ~transformX,
  "LASSO",          "poly",
  "Elastic Net",    "poly",
  "Random Forest",  "no",
  "Neural Network", "no"
)

grid_all <- tidyr::crossing(
  outcome_var = names(outcome_label_map),
  treatment   = names(treat_label_map),
  learners
)


# ? FIT DML MODELS
# --------------------------------------------------------------------------- #

all_results <- list()
failed_log  <- list()

for (i in seq_len(nrow(grid_all))) {

  outcome_var  <- grid_all$outcome_var[i]
  treatment    <- grid_all$treatment[i]
  learner_name <- grid_all$learner_name[i]
  transformX   <- grid_all$transformX[i]

  x_cols      <- x_cols_lookup[[treatment]]
  learner_obj <- learner_objs[[learner_name]]

  run_label <- paste(outcome_var, treatment, learner_name, sep = " | ")

  cat("\n====================================================\n")
  cat(sprintf("[%d/%d] %s\n", i, nrow(grid_all), run_label))
  cat("====================================================\n")

  cat("  Creating data object... ")
  data_obj <- xtdml_data_from_data_frame(
    df         = data,
    x_cols     = x_cols,
    y_col      = outcome_var,
    d_cols     = treatment,
    panel_id   = "country_code",
    time_id    = "year",
    approach   = "fd-exact",
    transformX = transformX
  )
  cat("OK (n_obs:", data_obj$n_obs, ")\n")

  # FD-transformed variances for pseudo-R2
  var_y <- var(as.numeric(data_obj$data_model[[outcome_var]]), na.rm = TRUE)
  var_d <- var(as.numeric(data_obj$data_model[[treatment]]),   na.rm = TRUE)

  cat("  Fitting DML model... ")
  fit <- tryCatch({
    model <- xtdml_plr$new(
      data_obj,
      ml_l    = learner_obj$clone(),
      ml_m    = learner_obj$clone(),
      n_folds = 5,
      n_rep   = 5,
      score   = "orth-PO"
    )
    model$fit()
    cat("OK\n")
    model
  }, error = function(e) {
    cat("FAILED\n  ERROR:", conditionMessage(e), "\n")
    NULL
  })

  if (is.null(fit)) {
    failed_log[[length(failed_log) + 1L]] <- run_label
    next
  }

  ci         <- fit$confint()
  rmse_l_val <- mean(as.numeric(fit$rmses$ml_l))
  rmse_m_val <- mean(as.numeric(fit$rmses$ml_m))

  all_results[[length(all_results) + 1L]] <- data.frame(
    outcome_var   = outcome_var,
    outcome_label = unname(outcome_label_map[outcome_var]),
    treatment     = treatment,
    treat_label   = unname(treat_label_map[treatment]),
    learner       = learner_name,
    theta         = as.numeric(fit$coef_theta),
    se            = as.numeric(fit$se_theta),
    p_value       = as.numeric(fit$pval_theta),
    ci_low        = as.numeric(ci[1, 1]),
    ci_high       = as.numeric(ci[1, 2]),
    n_obs         = fit$data$n_obs,
    rmse_l        = rmse_l_val,
    rmse_m        = rmse_m_val,
    r2_l          = 1 - (rmse_l_val^2 / var_y),
    r2_m          = 1 - (rmse_m_val^2 / var_d),
    stringsAsFactors = FALSE
  )

  res <- all_results[[length(all_results)]]
  cat("  theta =", round(res$theta, 4),
      " se =",     round(res$se, 4),
      " p =",      round(res$p_value, 4),
      " rmse_l =", round(res$rmse_l, 4),
      " rmse_m =", round(res$rmse_m, 4),
      " r2_l =",   round(res$r2_l, 4),
      " r2_m =",   round(res$r2_m, 4), "\n")
}

cat("Succeeded:", length(all_results), "/", nrow(grid_all), "\n")
cat("Failed:   ", length(failed_log), "\n")
if (length(failed_log) > 0) {
  cat("\nFailed runs:\n")
  for (fl in failed_log) cat("  -", fl, "\n")
}

if (length(all_results) == 0L) stop("No DML models succeeded.")

results_df <- bind_rows(all_results)
rownames(results_df) <- NULL
write_csv(results_df, "Output/Final/DML/03_dml_all_results.csv")


# ? SUMMARY TABLES
# --------------------------------------------------------------------------- #

# Main Model Results
stars <- function(p) {
  ifelse(p < 0.01, "***",
         ifelse(p < 0.05,  "**",
                ifelse(p < 0.1,  "*","")))
}

dml_lasso <- results_df %>%
  filter(learner == "LASSO") %>%
  mutate(sig = stars(p_value))
print(dml_lasso)
write_csv(dml_lasso, "Output/Final/DML/03_dml_lasso.csv")

# Learner Comparison
comp_df <- results_df %>%
  mutate(coef_str = sprintf("%.4f%s (%.4f)", theta, stars(p_value), se)) %>%
  select(outcome_label, treat_label, learner, coef_str)

comp_wide <- reshape(
  as.data.frame(comp_df),
  idvar     = c("outcome_label", "treat_label"),
  timevar   = "learner",
  direction = "wide",
  v.names   = "coef_str"
)
colnames(comp_wide) <- gsub("coef_str\\.", "", colnames(comp_wide))
comp_wide <- comp_wide[order(comp_wide$outcome_label, comp_wide$treat_label), ]

print(comp_wide)
write_csv(comp_wide, "Output/Final/DML/03_dml_learner_comparison.csv")

# First-stage diagnostics across all learners
firststage_diag <- results_df %>%
  select(outcome_label, treat_label, learner, rmse_l, rmse_m, r2_l, r2_m, n_obs) %>%
  mutate(across(c(rmse_l, rmse_m, r2_l, r2_m), ~ round(.x, 4)))

print(firststage_diag)
write_csv(firststage_diag, "Output/Final/DML/03_dml_firststage_diagnostics.csv")

# ? FD-TWFE COMPARISON
# --------------------------------------------------------------------------- #

twfe_results <- read_csv("Output/Final/TWFE/02_twfe_results.csv", show_col_types = FALSE)

twfe_comp <- twfe_results %>%
  filter(spec == "S8",
         term %in% c("d(doctors_per_10k)", "d(log_health_exp)")) %>%
  transmute(
    outcome   = unname(twfe_outcome_label_map[outcome]),
    treatment = gsub("d\\(|\\)", "", term),
    method    = "FD-TWFE",
    theta     = estimate,
    se        = se_dk,
    p_value   = p_dk
  )

dml_comp <- dml_lasso %>%
  transmute(
    outcome = outcome_label,
    treatment = treatment,
    method = "DML-LASSO",
    theta = theta,
    se = se,
    p_value = p_value
  )

comparison <- rbind(twfe_comp, dml_comp)
print(comparison)
write_csv(comparison, "Output/Final/DML/03_dml_vs_twfe_comparison.csv")



# ? PLOTS OF DML RESULTS
# --------------------------------------------------------------------------- #

sig_label <- function(p) {
  dplyr::case_when(
    p < 0.01  ~ "p<0.01",
    p < 0.05  ~ "p<0.05",
    p < 0.1   ~ "p<0.10",
    TRUE      ~ "n.s."
  )
}

all_plot <- results_df %>%
  mutate(
    sig = factor(sig_label(p_value),
                 levels = c( "p<0.01", "p<0.05", "p<0.10", "n.s.")),
    outcome_label = factor(
      outcome_label,
      levels = c("Log Infant Mortality", "Log Under-5 Mortality", "Log Maternal Mortality")
    ),
    treat_label = factor(
      treat_label,
      levels = c("Doctors per 10k", "Log Health Exp.")
    ),
    learner = factor(learner, levels = c("LASSO", "Elastic Net", "Random Forest", "Neural Network"))
  )

sig_colours <- c(
  "p<0.01" = "#d73027",
  "p<0.05"  = "#fc8d59",
  "p<0.10"  = "#91bfdb",
  "n.s."    = "grey60"
)

p_lasso <- all_plot %>%
  filter(learner == "LASSO") %>%
  ggplot(aes(x = theta, y = treat_label, colour = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.25) +
  geom_point(size = 3.5) +
  facet_wrap(~outcome_label, scales = "free_x", ncol = 3) +
  scale_colour_manual(values = sig_colours) +
  labs(
    title = "DML-FD-Exact: Causal Estimates (LASSO)",
    x = expression(hat(theta) ~ "(95% CI)"),
    y = NULL,
    colour = "Significance"
  ) +
  theme_simple

ggsave("Output/Final/DML/03_dml_coef_lasso.png", p_lasso,
       width = 14, height = 5, dpi = 300)


# Learner comparison — one plot per treatment
for (tr in names(treat_label_map)) {
  tr_label <- treat_label_map[[tr]]

  p <- all_plot %>%
    filter(treatment == tr) %>%
    ggplot(aes(x = theta, y = learner, colour = learner, shape = learner)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.25) +
    geom_point(size = 3) +
    facet_wrap(~outcome_label, scales = "free_x", ncol = 3) +
    scale_colour_brewer(palette = "Set1") +
    scale_shape_manual(values = c(16, 17, 15, 18)) +
    labs(
      title = paste0("DML-FD-Exact: Learner Comparison (", tr_label, ")"),
      x = expression(hat(theta) ~ "(95% CI)"),
      y = NULL,
      colour = "Learner",
      shape = "Learner"
    ) +
    theme_simple

  ggsave(sprintf("Output/Final/DML/03_dml_learner_comparison_%s.png", tr),
         p, width = 14, height = 5, dpi = 300)
}

# DML vs FD-TWFE — one plot per treatment
comparison_plot_df <- comparison %>%
  mutate(
    treat_label = unname(treat_label_map[treatment]),
    ci_low  = theta - 1.96 * se,
    ci_high = theta + 1.96 * se,
    outcome = factor(outcome, levels = unname(outcome_label_map))
  )

for (tr in names(treat_label_map)) {
  tr_label <- treat_label_map[[tr]]

  p <- comparison_plot_df %>%
    filter(treatment == tr) %>%
    ggplot(aes(x = theta, y = method, colour = method, shape = method)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.25) +
    geom_point(size = 3) +
    facet_wrap(~outcome, scales = "free_x", ncol = 3) +
    scale_colour_manual(values = c("FD-TWFE" = "steelblue", "DML-LASSO" = "darkred")) +
    labs(
      title = paste0("DML-FD-Exact (LASSO) vs FD-TWFE (", tr_label, ")"),
      x = expression(hat(theta) ~ "(95% CI)"),
      y = NULL,
      colour = "Method",
      shape = "Method"
    ) +
    theme_simple

  ggsave(sprintf("Output/Final/DML/03_dml_vs_twfe_%s.png", tr),
         p, width = 14, height = 4, dpi = 300)
}

# nolint end