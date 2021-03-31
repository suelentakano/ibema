# PACOTES ----

# Core
library(tidyverse)
library(magrittr)
# Datas
library(timetk)
library(lubridate)
library(tsibble)
# Análise Exploratória
library(DataExplorer)
# ML
library(tidymodels)
# Data Viz / Stats
library(ggstatsplot)

# DADOS ----

# Ibema

ibema <- readRDS("00_data/dataset_ibema.rds")

ibema <- janitor::clean_names(ibema)

ibema %>% glimpse()

ibema$delta_qd <- NULL # não faz parte dos dados originais

# Macroeconômicos

macro <- rio::import("00_data/indicadores_macro.xlsx")

macro %>% glimpse()

macro_long <- macro %>% 
  pivot_longer(names_to = "Período", cols = 2:37, names_repair = "unique") %>% 
  janitor::clean_names()

names(macro_long)  <- c("var", "date", "value")

macro_long$date <- janitor::excel_numeric_to_date(as.numeric(macro_long$date))

macro_long %<>% relocate(date)

macro_wide <- macro_long %>% 
  pivot_wider(names_from = "var") %>% 
  janitor::clean_names() %>% 
  mutate(ano_mes = as.character(yearmonth(date))) %>% 
  relocate(ano_mes)

ibema$ano_mes <- as.character(yearmonth(ibema$dat_movimento))
ibema %<>% relocate(ano_mes)

ibema_macro_joined <- ibema %>% 
  left_join(macro_wide, by = "ano_mes") %>% 
  select(-ano_mes, -date) %>% 
  relocate(qtd_volume, .after = last_col())

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

create_report(ibema_macro_joined, 
              output_file = 'ibema_macro_joined', config = report_config)

# VARIÁVEIS CONTÍNUAS ----

# cod_gramatura tem jeito de variável categórica (fator).
# num_comprimento e num_largura podem ser zero? Acho que não.
# Não faz sentido qtd_volume <= 0.
# As variáveis vlr_faturamento_xx <= 0 devem estar associadas a 
#   qtd_volume <= 0. Parece-me tratar-se de dados inválidos.
# vlr_preco_liquido == 0 não faz sentido.

# VARIÁVEIS CATEGÓRICAS ----

# cod_site == TVO somente.
# cod_produto tem vários níveis. Será que posso agrupá-los para diminuir o no. de níveis?
# Posso desprezar cod_regiao_vendedor.
# cod_grupo_vendas tem vários níveis. Será que posso agrupá-los para diminuir o no. de níveis?
# Posso desprezar cod_configuracao, vlr_despesa_venda, vlr_comissao, vlr_taxa_nota.

# DATA PREP ----

ibema_prep <- ibema_macro_joined %>% 
  mutate(
    cod_gramatura    = as.factor(cod_gramatura),
    cod_produto      = fct_lump_prop(cod_produto, prop = 0.01),
    cod_grupo_vendas = fct_lump_prop(cod_grupo_vendas, prop = 0.01)
  ) %>% 
  filter(
    num_comprimento > 0, num_largura > 0,  qtd_volume > 0, 
    vlr_faturamento_bruto > 0, vlr_preco_liquido > 0, cod_site == "TVO"
  ) %>% 
  select(-cod_site, -cod_regiao_vendedor, -cod_configuracao, 
         -vlr_despesa_venda, -vlr_comissao, -vlr_taxa_nota) %>% 
  drop_na()

create_report(ibema_prep, 
              output_file = 'ibema_prep.html', config = report_config)

# CORRELOGRAMS ----

ggcorrmat(
  data = select(ibema_prep, where(is.numeric)),
  p.adjust.method = "none",
  output = "plot"
) -> p
p

ggcorrmat(
  data = select(ibema_prep, where(is.numeric)),
  p.adjust.method = "none",
  output = "dataframe"
) -> cm
cm

descr_cor <- cor(select(ibema_prep, where(is.numeric)))

highly_cor_descr <- caret::findCorrelation(descr_cor, cutoff = 0.75)

ibema_prep_non_highly_cor <- select(
  ibema_prep, where(is.numeric)
) %>% 
  .[, -highly_cor_descr]

ibema_final <- bind_cols(
  date = ibema_prep$dat_movimento,
  select(ibema_prep, where(is.factor)),
  select(ibema_prep_non_highly_cor, -vlr_frete),
  qtd_volume = ibema_prep$qtd_volume
)

ggcorrmat(
  data = ibema_final,
  p.adjust.method = "none",
  output = "plot"
) -> p
p

plot_histogram(ibema_final)

# FEATURE ENGINEERING DA SÉRIE TEMPORAL ----

ibema_final <- ibema_final %>% 
  tk_augment_timeseries_signature() %>% 
  select(cod_produto:qtd_volume, month.lbl, wday.lbl) %>% 
  relocate(qtd_volume, .after = last_col()) %>% 
  mutate(across(where(is.factor), ~ factor(.x, ordered = FALSE)))

# MACHINE LEARNING ----

# data splitting
ibema_final %>% plot_bar()
ibema_final %>% plot_histogram()

set.seed(123)
ibema_split <- initial_split(ibema_final)

ibema_train <- training(ibema_split)
ibema_train %>% plot_bar()
ibema_train %>% plot_histogram()

ibema_test <- testing(ibema_split)
ibema_test %>% plot_bar()
ibema_test %>% plot_histogram()

# Random Forest
rf_spec <- rand_forest(mode = "regression") %>% 
  set_engine("ranger")

rf_fit <- rf_spec %>% 
  fit(qtd_volume ~ ., data = ibema_train)

# * H2O Auto ML ----
library(h2o)

# Start a clean h2o cluster
Sys.setenv(JAVA_HOME = "C:/Program Files (x86)/Java/jre1.8.0_281")
h2o.init()
h2o.removeAll()

# The training set (convert to an H2OFrame)
train <- as.h2o(ibema_train)

# The test set (convert to an H2OFrame)
test <- as.h2o(ibema_test)

# Take a look at the training set
h2o.describe(train)

# Identify the response column
y <- "qtd_volume"

# Identify the predictor columns (remove response and ID column)
x <- setdiff(names(train), y)


# ** H2O AutoML Training ----

# Execute an AutoML run for 20 models
aml <- h2o.automl(
  y              = y, 
  x              = x, 
  training_frame = train,
  project_name   = "ibema",
  max_models     = 20,
  seed           = 1,
  exclude_algos  = "DeepLearning"
)

# ** H2O AutoML Leaderboard ----

# The leader model is stored at `aml@leader` and the leaderboard is stored at `aml@leaderboard`.
lb <- aml@leaderboard

# Now we will view a snapshot of the top models.  Here we should see the two Stacked Ensembles 
# at or near the top of the leaderboard.  Stacked Ensembles can almost always outperform a single model.
print(lb)

# To view the entire leaderboard, specify the `n` argument of the `print.H2OFrame()` 
# function as the total number of rows:
print(lb, n = nrow(lb))


# ** Ensemble Exploration ----

# To understand how the ensemble works, let's take a peek inside the Stacked Ensemble "All Models" model.  
# The "All Models" ensemble is an ensemble of all of the individual models in the AutoML run.  
# This is often the top performing model on the leaderboard.

# Get model ids for all models in the AutoML Leaderboard
model_ids <- as.data.frame(aml@leaderboard$model_id)[, 1]
# Get the "All Models" Stacked Ensemble model
se <- h2o.getModel(
  grep("StackedEnsemble_AllModels", model_ids, value = TRUE)[1]
)
# Get the Stacked Ensemble metalearner model
metalearner <- h2o.getModel(se@model$metalearner$name)

# Examine the variable importance of the metalearner (combiner) algorithm in the ensemble.  
# This shows us how much each base learner is contributing to the ensemble. The AutoML Stacked Ensembles 
# use the default metalearner algorithm (GLM with non-negative weights), so the variable importance of the 
# metalearner is actually the standardized coefficient magnitudes of the GLM. 
h2o.varimp(metalearner)

# We can also plot the base learner contributions to the ensemble.
h2o.varimp_plot(metalearner)


# ** Variable Importance ----

# Now let's look at the variable importance on the training set using the top model
# (Stacked Ensembles don't have variable importance yet)
best <- h2o.getModel(grep("GBM_grid__1", model_ids, value = TRUE)[1])

# Examine the variable importance of the top XGBoost model
h2o.varimp(best)

# We can also plot the base learner contributions to the ensemble.
h2o.varimp_plot(best)

# ** Model Performance ----

# Performance on the test set, to compare with the other algos
best_perf <- h2o.performance(best, newdata = test)
best_perf

best_pred <- as.vector(h2o.predict(best, newdata = test))

test_pred <- bind_cols(actual = ibema_test$qtd_volume, pred = best_pred)

ggplot(test_pred, aes(x = actual, y = pred)) + 
  geom_abline(col = "green", lty = 2, lwd = 1) + 
  geom_point(alpha = 0.5) + 
  coord_fixed(ratio = 1)

rmse_vec(test_pred$actual, test_pred$pred)
mae_vec(test_pred$actual, test_pred$pred)
rsq_trad_vec(test_pred$actual, test_pred$pred)
100 - smape_vec(test_pred$actual, test_pred$pred)


# Performance on the test set, to compare with the other algos
ml_perf <- h2o.performance(metalearner, newdata = test)
ml_perf

ml_pred <- as.vector(h2o.predict(best, newdata = test))

test_pred <- bind_cols(actual = ibema_test$qtd_volume, pred = ml_pred)

ggplot(test_pred, aes(x = actual, y = pred)) + 
  geom_abline(col = "green", lty = 2, lwd = 1) + 
  geom_point(alpha = 0.5) + 
  coord_fixed(ratio = 1)

rmse_vec(test_pred$actual, test_pred$pred)
mae_vec(test_pred$actual, test_pred$pred)
rsq_trad_vec(test_pred$actual, test_pred$pred)
100 - smape_vec(test_pred$actual, test_pred$pred)

