# BLUESHIFT
# CLIENTE: IBEMA
# PROJETO: POC MICROSOFT

# OBJETIVOS ----
# - PREVER A DEMANDA E O PREÇO NUM CERTO HORIZONTE, POR UF, SITE E TIPO DE EMBALAGEM

# PACOTES ----

# Core
library(readr)
library(tidyverse)
library(magrittr)
# Datas
library(timetk)
library(lubridate)
# Análise Exploratória
library(DataExplorer)
# ML
library(tidymodels)
library(rules)
library(modeltime)
library(modeltime.ensemble)
library(modeltime.resample)
# Data Viz / Stats
library(ggstatsplot)
# library(collapsibleTree)

# INGESTÃO DE DADOS & PIPELINE DE DATA PREP ----

UF        <- 'SP'
SITE      <- 'TVO'
EMBALAGEM <- 'BOBINA'

ibema <- read_delim(
  "00_data/Dados_BlueShift_Total.csv", ";",
  escape_double = FALSE,
  col_types = cols(
    VLR_PRECO_LIQUIDO           = col_number(),
    VLR_FATURAMENTO_BRUTO       = col_number(),
    VLR_FATURAMENTO_SEM_IPI     = col_number(),
    VLR_FATURAMENTO_SEM_IMPOSTO = col_number(),
    VLR_TAXA_NOTA               = col_number(),
    QTD_VOLUME                  = col_number()
  ),
  trim_ws = TRUE
) %>% 
  janitor::clean_names() %>% 
  mutate(
    cod_gramatura      = as.factor(cod_gramatura),
    cod_site           = as.factor(cod_site),
    cod_uf             = as.factor(cod_uf),
    cod_tipo_embalagem = as.factor(cod_tipo_embalagem),
    cod_configuracao   = as.factor(cod_configuracao),
    cod_produto        = as.factor(cod_produto),
    dat_movimento      = as.Date(dat_movimento),
    cod_produto        = fct_lump_prop(cod_produto, prop = 0.01),
    cod_configuracao   = fct_lump_prop(cod_configuracao, prop = 0.01),
    cod_uf             = fct_lump_prop(cod_uf, prop = 0.01),
    cod_gramatura      = fct_lump_prop(cod_gramatura, prop = 0.01)
  ) %>% 
  filter(
    num_comprimento > 0, num_largura > 0,  qtd_volume > 0, 
    vlr_faturamento_bruto > 0, vlr_preco_liquido > 0
  ) %>% 
  select(
    -vlr_despesa_financeira, -vlr_faturamento_bruto,
    -vlr_comissao, -vlr_despesa_venda, -vlr_frete_unitario,
    -nom_cidade, -vlr_faturamento_sem_imposto, -vlr_impostos,
    -vlr_taxa_nota, -vlr_faturamento_sem_ipi, -vlr_frete
  ) %>% 
  filter(cod_uf == UF, cod_site == SITE, cod_tipo_embalagem == EMBALAGEM) %>% 
  select(-cod_uf, -cod_site, -cod_tipo_embalagem) %>% 
  arrange(dat_movimento) %>% 
  pad_by_time(.date_var = dat_movimento, .by = "day", .pad_value = NA) %>% 
  summarise_by_time(.date_var = dat_movimento, .by = "day", 
                    preco_liq_medio = mean(vlr_preco_liquido), 
                    volume_total = sum(qtd_volume)) %>% 
  mutate(
    preco_liq_medio = zoo::na.locf(preco_liq_medio),
    volume_total    = replace_na(volume_total, 0)
  )

# FEATURE ENGINEERING ----

ibema %>% glimpse()

visdat::vis_miss(ibema, sort_miss = TRUE)

ibema %>% tk_summary_diagnostics(.date_var = dat_movimento) %>% glimpse()

ibema %>% 
  plot_acf_diagnostics(.date_var = dat_movimento, .value = volume_total)

ibema %>% 
  plot_seasonal_diagnostics(.date_var = dat_movimento, .value = volume_total)

# FORECASTING ----

# Aux function
calibrate_and_plot <- function(..., type = "testing") {
  
  if (type == "testing") {
    new_data <- testing(splits)
  } else {
    new_data <- training(splits) %>% drop_na()
  }
  
  calibration_tbl <- modeltime_table(...) %>% 
    modeltime_calibrate(new_data)
  
  print(modeltime_accuracy(calibration_tbl))
  
  calibration_tbl %>% 
    modeltime_forecast(
      new_data    = new_data,
      actual_data = data_prepared_tbl
    ) %>% 
    plot_modeltime_forecast(.conf_interval_show = FALSE)
  
}

refit_and_plot <- function(...) {
  
  calibration_tbl <- modeltime_table(...) %>% 
    modeltime_calibrate(testing(splits))
  
  refit_tbl <- calibration_tbl %>%
    modeltime_refit(data_prepared_tbl)
  
  refit_tbl %>%
    modeltime_forecast(new_data    = forecast_tbl,
                       actual_data = data_prepared_tbl) %>%
    
    # Invert Transformation
    mutate(across(.value:.conf_hi, .fns = ~ standardize_inv_vec(
      x    = .,
      mean = std_mean,
      sd   = std_sd
    ))) %>%
    mutate(across(.value:.conf_hi, .fns = ~ log_interval_inv_vec(
      x           = ., 
      limit_lower = limit_lower, 
      limit_upper = limit_upper, 
      offset      = offset
    ))) %>%
    
    plot_modeltime_forecast()
  
}

# Transformação dos dados
ibema_transformed_tbl <- ibema %>%
  
  # Preprocessamento do Alvo
  mutate(preco_avg_trans = log(preco_liq_medio)) %>%
  mutate(preco_avg_trans = standardize_vec(preco_avg_trans)) %>%
  select(-volume_total, -preco_liq_medio)

# Parâmetros chave
std_mean    <- 8.26575793917595
std_sd      <- 0.125079020650008

ibema_transformed_tbl %>% 
  plot_time_series(dat_movimento, preco_avg_trans)

ibema_transformed_tbl %>% 
  plot_acf_diagnostics(dat_movimento, preco_avg_trans)

plot_time_series_regression(
  .data = ibema_transformed_tbl %>% 
    tk_augment_lags(.value = preco_avg_trans, .lags = c(1, 2, 3, 7, 14, 28)) %>% 
    drop_na(),
  .date_var = dat_movimento,
  .formula = preco_avg_trans ~ as.numeric(dat_movimento)
  + wday(dat_movimento, label = TRUE)
  + month(dat_movimento, label = TRUE)
  + .
  - dat_movimento,
  .show_summary = TRUE
)

# 1.0 PASSO 1 - CRIAR FULL DATA SET ----
# - Estender pela janela futura
# - Feature engineering
# - Regressores externos

horizon    <- 14 # 14 dias
lag_period <- 14
rolling_periods <- c(7, 14)
dot_value <- str_glue("preco_avg_trans_lag{lag_period}")

data_prepared_full_tbl <- ibema_transformed_tbl %>%
  
  # Janela futura
  bind_rows(
    future_frame(.data = ., .date_var = dat_movimento, .length_out = horizon)
  ) %>%
  
  # Lags autocorrelacionados
  tk_augment_lags(preco_avg_trans, .lags = lag_period) %>%
  
  # Rolling features to stabilize trend from M5 Competition
  tk_augment_slidify(
    .value   = rlang::sym(dot_value),
    .f       = mean,
    .period  = rolling_periods,
    .align   = "center",
    .partial = TRUE
  ) %>%
  
  # Format Columns
  rename_with(.cols = contains("lag"), .fn = ~ str_c("lag_", .x))

# visual check
data_prepared_full_tbl %>% 
  pivot_longer(-dat_movimento) %>%
  plot_time_series(dat_movimento, value, name, .smooth = FALSE)

data_prepared_full_tbl %>% head() %>% glimpse()
data_prepared_full_tbl %>% tail(horizon + 1) # ok!

# 2.0 PASSO 2 - SEPARAR os DADOS DE MODELAGEM E DE FORECAST ----

data_prepared_tbl <- data_prepared_full_tbl %>%
  filter(!is.na(preco_avg_trans))

forecast_tbl <- data_prepared_full_tbl %>%
  filter(is.na(preco_avg_trans))

# 3.0 TRAIN/TEST (DATASET DE MODELAGEM) ----

splits <- time_series_split(data_prepared_tbl, assess = horizon, 
                            cumulative = TRUE)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(dat_movimento, preco_avg_trans)

# 4.0 RECIPES ----
# - Time Series Signature - Adds bulk time-based features
# - More Transformation to Time Series Signature vars
# - Interactions, if any
# - Fourier Features

recipe_spec_base <- recipe(preco_avg_trans ~ ., data = training(splits)) %>%
  
  # Time Series Signature
  step_timeseries_signature(dat_movimento) %>%
  step_rm(matches("(iso)|(xts)|(hour)|(minute)|(second)|(am.pm)")) %>%
  step_rm(ends_with("(day)|(day7)")) %>% 
  
  # Standardization
  step_normalize(matches("(index.num)|(year)")) %>%
  
  # Dummy Encoding (One Hot Encoding)
  step_dummy(all_nominal(), one_hot = TRUE) %>%
  
  # Fourier
  step_fourier(dat_movimento, period = c(1, 14), K = 2)


recipe_spec_base %>% prep() %>% juice() %>% glimpse()

# 5.0 LINEAR REGRESSION MODEL ----

# * Model spec ----

model_spec_lm <- linear_reg() %>% 
  set_engine("lm")

# * Model Workflow  ----

recipe_spec_lm <- recipe_spec_base %>% 
  step_rm(dat_movimento) %>% 
  step_naomit(contains("lag"))

workflow_fit_lm <- workflow() %>%
  add_model(model_spec_lm) %>%
  add_recipe(recipe_spec_lm) %>%
  fit(training(splits))

calibrate_and_plot(workflow_fit_lm)

# 6.0 GLM REGRESSION MODEL ----

# * GLM Model Spec ----

model_spec_glm <- linear_reg(penalty = 0.1, mixture = 0.5) %>%
  set_engine("glmnet")

# * GLM Model Workflow  ----

recipe_spec_glm <- recipe_spec_base %>% 
  step_rm(dat_movimento) %>% 
  step_naomit(contains("lag"))

workflow_fit_glm <- workflow() %>%
  add_model(model_spec_glm) %>%
  add_recipe(recipe_spec_glm) %>%
  fit(training(splits))

calibrate_and_plot(workflow_fit_lm, workflow_fit_glm)

# 7.0 RANDOM FOREST MODEL ----

# * RF Model Spec ----

model_spec_rf <- rand_forest() %>%
  set_engine("randomForest")

# * RF Model Workflow  ----

workflow_fit_rf <- workflow() %>%
  add_model(model_spec_rf) %>%
  add_recipe(recipe_spec_glm) %>%
  fit(training(splits))

calibrate_and_plot(workflow_fit_lm, workflow_fit_glm, workflow_fit_rf)

# 8.0 ARIMA MODEL ----

model_fit_auto_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(
    preco_avg_trans ~ dat_movimento 
    + fourier_vec(dat_movimento, period = 7)
    + fourier_vec(dat_movimento, period = 14)
    + fourier_vec(dat_movimento, period = 21),
    data = training(splits)
  )

calibrate_and_plot(workflow_fit_lm, workflow_fit_glm, workflow_fit_rf,
                   model_fit_auto_arima)

# 9.0 PROPHET MODEL, NO XREGS ----

model_fit_prophet <- prophet_reg(
  # changepoint_num    = 25,
  # changepoint_range  = 0.8,
  # seasonality_yearly = FALSE,
  # seasonality_weekly = TRUE,
  # seasonality_daily  = TRUE
) %>%
  set_engine("prophet") %>%
  fit(preco_avg_trans ~ dat_movimento, data = training(splits))

calibrate_and_plot(workflow_fit_lm, workflow_fit_glm, workflow_fit_rf,
                   model_fit_auto_arima, model_fit_prophet)

# 10.0 PROPHET MODEL, XREGS ----

model_fit_prophet_xregs <- prophet_reg(
  # changepoint_num    = 25,
  # changepoint_range  = 0.8,
  # seasonality_yearly = FALSE,
  seasonality_weekly = TRUE
  # seasonality_daily  = TRUE
) %>%
  set_engine("prophet") %>%
  fit(
    preco_avg_trans ~ dat_movimento 
    + fourier_vec(dat_movimento, period = 7)
    + fourier_vec(dat_movimento, period = 14)
    + fourier_vec(dat_movimento, period = 21),
    data = training(splits)
  )

calibrate_and_plot(workflow_fit_lm, workflow_fit_glm, workflow_fit_rf,
                   model_fit_auto_arima, model_fit_prophet, 
                   model_fit_prophet_xregs)

# 11.0 CUBIST ----

model_spec_cubist <- cubist_rules(
  committees = 7,
  neighbors = 14
) %>% 
  set_engine("Cubist")

set.seed(123)
workflow_fit_cubist <- workflow() %>% 
  add_model(model_spec_cubist) %>% 
  add_recipe(recipe_spec_lm) %>% 
  fit(training(splits))

calibrate_and_plot(workflow_fit_lm, workflow_fit_glm, workflow_fit_rf,
                   model_fit_auto_arima, model_fit_prophet, 
                   model_fit_prophet_xregs, workflow_fit_cubist)

# 12.0 XGBOOST ----

workflow_fit_xgboost <- workflow() %>%
  add_model(
    boost_tree() %>% 
      set_engine("xgboost")
  ) %>%
  add_recipe(recipe_spec_lm) %>%
  fit(training(splits))

calibrate_and_plot(workflow_fit_lm, workflow_fit_glm, workflow_fit_rf,
                   model_fit_auto_arima, model_fit_prophet, 
                   model_fit_prophet_xregs, workflow_fit_cubist, 
                   workflow_fit_xgboost)

# 13.0 NNETAR ----

model_spec_nnetar <- nnetar_reg(
  # non_seasonal_ar = 1,
  seasonal_ar     = 7,
  hidden_units    = 10
  # penalty         = 1,
  # num_networks    = 30,
  # epochs          = 50
) %>%
  set_engine("nnetar")

set.seed(123)
workflow_fit_nnetar <- workflow() %>%
  add_model(model_spec_nnetar) %>%
  add_recipe(recipe_spec_base) %>%
  fit(training(splits) %>% drop_na())

calibrate_and_plot(workflow_fit_nnetar)

calibrate_and_plot(workflow_fit_lm, workflow_fit_glm, workflow_fit_rf,
                   model_fit_auto_arima, model_fit_prophet, 
                   model_fit_prophet_xregs, workflow_fit_cubist, 
                   workflow_fit_xgboost, workflow_fit_nnetar)

# 14.0 MODELTIME WORKFLOW ----

# * Modeltime Table ----
submodels_tbl <- modeltime_table(
  workflow_fit_lm, workflow_fit_glm, workflow_fit_rf,
  model_fit_auto_arima, model_fit_prophet, 
  model_fit_prophet_xregs, workflow_fit_cubist, 
  workflow_fit_xgboost, workflow_fit_nnetar
)

# * Calibrate Testing Data ----
submodels_calibrated_tbl <- submodels_tbl %>%
  modeltime_calibrate(testing(splits))

# * Measure Test Accuracy ----
submodels_calibrated_tbl %>% 
  modeltime_accuracy() %>% 
  table_modeltime_accuracy(
    .interactive = TRUE,
    bordered = TRUE, 
    resizable = TRUE
  )

# * Visualize Test Forecast ----
submodels_calibrated_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = data_prepared_tbl,
    keep_data   = TRUE
  ) %>%
  plot_modeltime_forecast()

# * Refit on Full Training Dataset -----
submodels_refit_tbl <- submodels_calibrated_tbl %>%
  modeltime_refit(data_prepared_tbl)

# * Visualize Submodel Forecast ----
submodels_refit_tbl %>%
  modeltime_forecast(
    new_data    = forecast_tbl,
    actual_data = data_prepared_tbl,
    keep_data   = TRUE
  ) %>%
  plot_modeltime_forecast()

# 15.0 BEST MODEL ----

best_model_fit <- model_fit_auto_arima

calibration_tbl <- modeltime_table(best_model_fit) %>% 
  modeltime_calibrate(new_data = testing(splits))

print(modeltime_accuracy(calibration_tbl))

calibration_tbl %>% 
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = data_prepared_tbl
  ) %>% 
  # Invert Transformation
  mutate(across(.value:.conf_hi, .fns = ~ standardize_inv_vec(
    x    = .,
    mean = std_mean,
    sd   = std_sd
  ))) %>%
  mutate(across(.value:.conf_hi, .fns = exp
  )) %>%
  
  plot_modeltime_forecast()

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data_prepared_tbl)

# Visualize Forecast
refit_tbl %>%
  
  modeltime_forecast(
    new_data    = forecast_tbl,
    actual_data = data_prepared_tbl,
    keep_data   = TRUE
  ) %>%
  
  # Invert Transformation
  mutate(across(.value:.conf_hi, .fns = ~ standardize_inv_vec(
    x    = .,
    mean = std_mean,
    sd   = std_sd
  ))) %>%
  mutate(across(.value:.conf_hi, .fns = exp
  )) %>%
  
  plot_modeltime_forecast()

# 16.0 ENSEMBLE ----

# * Make Ensemble ----
ensemble_fit_mean <- submodels_tbl %>% 
  filter(.model_desc %in% c("RANDOMFOREST", "CUBIST",
                            "REGRESSION WITH ARIMA(1,1,1) ERRORS")) %>% 
  ensemble_average(type = "mean")

# * Modeltime Table ----
ensemble_tbl <- modeltime_table(
  ensemble_fit_mean
)

# * Ensemble Test Accuracy ----
ensemble_tbl %>%
  combine_modeltime_tables(
    submodels_tbl %>% 
      filter(.model_desc %in% c("RANDOMFOREST", "CUBIST",
                                "REGRESSION WITH ARIMA(1,1,1) ERRORS"))
  ) %>%
  modeltime_accuracy(testing(splits)) %>% 
  table_modeltime_accuracy(
    .interactive = TRUE,
    bordered = TRUE, 
    resizable = TRUE
  )

# * Ensemble Test Forecast ----
ensemble_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = data_prepared_tbl,
    keep_data   = TRUE
  ) %>%
  plot_modeltime_forecast()

# * Refit Ensemble ----
ensemble_refit_tbl <- ensemble_tbl %>%
  modeltime_calibrate(testing(splits)) %>% 
  modeltime_refit(data_prepared_tbl)

# * Visualize Ensemble Forecast ----
ensemble_refit_tbl %>%
  
  modeltime_forecast(
    new_data    = forecast_tbl,
    actual_data = data_prepared_tbl,
    keep_data   = TRUE
  ) %>%
  
  # Invert Transformation
  mutate(across(.value:.conf_hi, .fns = ~ standardize_inv_vec(
    x    = .,
    mean = std_mean,
    sd   = std_sd
  ))) %>%
  mutate(across(.value:.conf_hi, .fns = exp
  )) %>%
  
  plot_modeltime_forecast()
