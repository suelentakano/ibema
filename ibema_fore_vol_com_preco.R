# BLUESHIFT
# CLIENTE: IBEMA
# PROJETO: POC MICROSOFT

# OBJETIVOS ----
# - PREVER A DEMANDA E O PREÇO NUM HORIZONTE DE 4 SEMANAS, POR UF (E TIPO DE EMBALAGEM?)

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
library(modeltime)
# Data Viz / Stats
library(ggstatsplot)
library(collapsibleTree)

# DADOS ----

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
)

ibema <- janitor::clean_names(ibema)

ibema %>% glimpse()

report_config <- configure_report(
  add_introduce        = TRUE,
  add_plot_intro       = TRUE,
  add_plot_str         = TRUE,
  add_plot_missing     = TRUE,
  add_plot_histogram   = TRUE,
  add_plot_density     = FALSE,
  add_plot_qq          = FALSE,
  add_plot_bar         = TRUE,
  add_plot_correlation = TRUE,
  add_plot_prcomp      = FALSE,
  add_plot_boxplot     = FALSE,
  add_plot_scatterplot = FALSE
)

create_report(ibema, 
              output_file = 'ibema_report', config = report_config)

# DATA PREP ----

ibema_prep <- ibema %>% 
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
  )

glimpse(ibema_prep)

create_report(ibema_prep, 
              output_file = 'ibema_prep.html', config = report_config)

saveRDS(ibema, "00_data/ibema.rds")
saveRDS(ibema_prep, "00_data/ibema_prep.rds")

ibema_prep %>% 
  collapsibleTree(
    hierarchy = c("cod_site", "cod_produto", "cod_uf", "cod_tipo_embalagem", 
                  "cod_gramatura" ,"cod_configuracao"),
    attribute = "qtd_volume",
    root      = "All Sites",
    aggFun    = sum,
    nodeSize  = "qtd_volume",
    tooltip   = TRUE,
    fontSize  = 16
  )

ibema_sp <- filter(ibema_prep, cod_uf == 'SP')

create_report(ibema_sp, 
              output_file = 'ibema_sp.html', config = report_config)

# ibema_sp_bobina <- filter(ibema_sp, cod_tipo_embalagem == 'BOBINA')
# create_report(ibema_sp_bobina, 
#               output_file = 'ibema_sp_bobina.html', config = report_config)

# CORRELOGRAMAS ----

ggcorrmat(
  data = select(ibema_sp, where(is.numeric)),
  p.adjust.method = "none",
  # type = "spearman",
  output = "plot"
) -> p
p

ggcorrmat(
  data = select(ibema_sp, where(is.numeric)),
  p.adjust.method = "none",
  output = "dataframe"
) -> cm
cm

# ANÁLISE EXPLORATÓRIA ----

ibema_sp %>% glimpse()

ibema_sp %>% 
  plot_time_series(dat_movimento, qtd_volume)

ibema_sp %>% 
  plot_seasonal_diagnostics(.date_var = dat_movimento, 
                            .feature_set = c("wday.lbl", "month.lbl",
                                             "quarter", "year"), 
                            log1p(qtd_volume))

ibema_sp %>% 
  plot_seasonal_diagnostics(.date_var = dat_movimento, 
                            .feature_set = c("wday.lbl", "month.lbl",
                                             "quarter", "year"), 
                            log1p(vlr_preco_liquido))

ibema_sp %>% 
  plot_seasonal_diagnostics(.date_var = dat_movimento, 
                            .feature_set = c("month.lbl"), 
                            log1p(vlr_preco_liquido))

ibema_sp_week <- ibema_sp %>% 
  summarise_by_time(
    .date_var = dat_movimento, 
    .by = "week",
    vlr_preco_liquido = mean(vlr_preco_liquido),
    qtd_volume = sum(qtd_volume)
  )

# ibema_sp_week <- ibema_sp_week %>% 
#   mutate(elast = (c(0, diff(qtd_volume)) / qtd_volume) / 
#            (c(0, diff(vlr_preco_liquido)) / vlr_preco_liquido))
# 
# ibema_sp_week$elast <- ifelse(
#   is.na(ibema_sp_week$elast), 0, ibema_sp_week$elast
# )

ibema_sp_week %>% glimpse()

ibema_sp_week %>% 
  plot_time_series(dat_movimento, qtd_volume)

ibema_sp_week %>% 
  plot_time_series(dat_movimento, log1p(qtd_volume))

ibema_sp_week %>% 
  plot_acf_diagnostics(.date_var = dat_movimento, .value = qtd_volume)

ibema_sp_week %>% 
  plot_time_series(dat_movimento, vlr_preco_liquido)

ibema_sp_week %>% 
  plot_time_series(dat_movimento, log1p(vlr_preco_liquido))

ibema_sp_week %>% 
  plot_acf_diagnostics(.date_var = dat_movimento, .value = vlr_preco_liquido)

# ibema_sp_week$elast %>% summary()
# 
# ibema_sp_week %>% 
#   plot_time_series(dat_movimento, elast)

# FORECASTING ----

# Transformação dos dados
ibema_sp_week_transformed_tbl <- ibema_sp_week %>%
  
  # Preprocessamento do Alvo
  mutate(qtd_volume_trans = log_interval_vec(qtd_volume, limit_lower = 0, 
                                             offset = 1)) %>%
  mutate(qtd_volume_trans = standardize_vec(qtd_volume_trans)) %>%
  mutate(vlr_preco_liquido_trans = log_interval_vec(vlr_preco_liquido, 
                                                    offset = 1)) %>%
  mutate(vlr_preco_liquido_trans = standardize_vec(vlr_preco_liquido_trans)) %>%
  select(-qtd_volume, -vlr_preco_liquido)

# Parâmetros chave
limit_lower_vol <- 0
limit_upper_vol <- 2988.1145
offset          <- 1
std_mean_vol    <- -0.809329982189386
std_sd_vol      <- 0.869851884603995

limit_lower_preco <- 0
limit_upper_preco <- 4708.33547066409
std_mean_preco    <- 1.66730614628552
std_sd_preco      <- 0.437635664864141

# 1.0 PASSO 1 - CRIAR FULL DATA SET ----
# - Extender pela janela futura
# - Feature engineering
# - Regressores externos

horizon    <- 4 # 4 semanas
lag_period <- 4
rolling_periods <- c(4, 9, 13)

data_prepared_full_tbl <- ibema_sp_week_transformed_tbl %>%
  
  # Janela futura
  bind_rows(
    future_frame(.data = ., .date_var = dat_movimento, .length_out = horizon)
  ) %>% 
  
  # Lags autocorrelacionados
  tk_augment_lags(qtd_volume_trans, .lags = lag_period) %>%
  tk_augment_lags(vlr_preco_liquido_trans, .lags = lag_period) %>% 
  
  # 'Rolling features'
  tk_augment_slidify(
    .value   = qtd_volume_trans_lag4,
    .f       = mean, 
    .period  = rolling_periods,
    .align   = "center",
    .partial = TRUE
  ) %>%
  
  # Format Columns
  rename_with(.cols = contains("lag"), .fn = ~ str_c("lag_", .))


data_prepared_full_tbl %>%
  pivot_longer(-dat_movimento) %>%
  plot_time_series(dat_movimento, value, name, .smooth = FALSE)

data_prepared_full_tbl %>% tail(4 + 1) # ok!

# 2.0 PASSO 2 - SEPARAR os DADOS DE MODELAGEM E DE FORECAST ----

data_prepared_tbl <- data_prepared_full_tbl %>%
  filter(!is.na(qtd_volume_trans))
data_prepared_tbl

forecast_tbl <- data_prepared_full_tbl %>%
  filter(is.na(qtd_volume_trans))
forecast_tbl

# 3.0 TRAIN/TEST (DATASET DE MODELAGEM) ----

data_prepared_tbl

splits <- time_series_split(data_prepared_tbl, assess = horizon, 
                            cumulative = TRUE)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(dat_movimento, qtd_volume_trans)

# 4.0 RECIPES ----
# - Time Series Signature - Adds bulk time-based features
# - More Transformation to  Time Series Signature vars
# - Interactions, if any
# - Fourier Features

recipe_spec_base <- recipe(qtd_volume_trans ~ ., data = training(splits)) %>%
  
  # Time Series Signature
  step_timeseries_signature(dat_movimento) %>%
  step_rm(matches("(iso)|(xts)|(hour)|(minute)|(second)|(am.pm)|(day)")) %>%
  
  # Standardization
  step_normalize(matches("(index.num)|(year)")) %>%
  
  # Dummy Encoding (One Hot Encoding)
  step_dummy(all_nominal(), one_hot = TRUE) %>%
  
  # Interaction
  # step_interact(~ matches("week2") * matches("wday.lbl")) %>%
  
  # Fourier
  step_fourier(dat_movimento, period = c(4, 8, 12), K = 2) %>% 
  step_naomit(contains("lag"))


recipe_spec_base %>% prep() %>% juice() %>% glimpse()

# 5.0 LINEAR REGRESSION MODEL ----

# * LM Model Spec ----

model_spec_lm <- linear_reg() %>%
  set_engine("lm")

# * LM Model Workflow  ----

workflow_fit_lm_1 <- workflow() %>%
  add_model(model_spec_lm) %>%
  add_recipe(recipe_spec_base) %>%
  fit(training(splits))

workflow_fit_lm_1

workflow_fit_lm_1 %>% 
  pull_workflow_fit() %>%
  pluck("fit") %>%
  summary()

# * RF Model Spec ----

model_spec_rf <- rand_forest() %>%
  set_engine("ranger")

# * RF Model Workflow  ----

recipe_spec_rf <- recipe_spec_base %>% 
  step_rm(contains("dat_movimento"))

workflow_fit_rf_1 <- workflow() %>%
  add_model(model_spec_rf) %>%
  add_recipe(recipe_spec_rf) %>%
  fit(training(splits))

workflow_fit_rf_1

workflow_fit_rf_1 %>% 
  pull_workflow_fit() %>%
  pluck("fit") %>%
  summary()

# 6.0 MODELTIME  ----

calibration_tbl <- modeltime_table(
  workflow_fit_lm_1,
  workflow_fit_rf_1
) %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl %>%
  modeltime_forecast(new_data    = testing(splits), 
                     actual_data = data_prepared_tbl) %>%
  plot_modeltime_forecast()

calibration_tbl %>% modeltime_accuracy()

# 7.0 FUTURE FORECAST ----

forecast_tbl$vlr_preco_liquido_trans <- c(0.01577437, 0.52270183, 0.29359550, 
                                          -4.59973273)

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = data_prepared_tbl)

refit_tbl %>%
  modeltime_forecast(new_data    = forecast_tbl,
                     actual_data = data_prepared_tbl) %>%
  
  # Invert Transformation
  mutate(across(.value:.conf_hi, .fns = ~ standardize_inv_vec(
    x    = .,
    mean = std_mean_vol,
    sd   = std_sd_vol
  ))) %>%
  mutate(across(.value:.conf_hi, .fns = ~ log_interval_inv_vec(
    x           = ., 
    limit_lower = limit_lower_vol, 
    limit_upper = limit_upper_vol, 
    offset      = offset
  ))) %>%
  plot_modeltime_forecast()
