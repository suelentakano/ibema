# PACOTES ----
library(readr)
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

ibema <- read_delim("00_data/Dados _BlueShift.csv", ";",
                  escape_double = FALSE, col_types = cols(
                      VLR_PRECO_LIQUIDO = col_number(),           
                      VLR_FATURAMENTO_BRUTO = col_number(), 
                      VLR_FATURAMENTO_SEM_IPI = col_number(), 
                      VLR_FATURAMENTO_SEM_IMPOSTO = col_number(), 
                      VLR_TAXA_NOTA = col_number(),
                      QTD_VOLUME = col_number()), trim_ws = TRUE)

#ibema <- readRDS("00_data/dataset_ibema.rds")

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
    dat_movimento      = as.Date(dat_movimento,
                                 format = "%d/%m/%Y"),
    cod_produto        = fct_lump_prop(cod_produto, prop = 0.01),
    cod_configuracao   = fct_lump_prop(cod_configuracao, prop = 0.01),
    cod_uf             = fct_lump_prop(cod_uf, prop = 0.01),
    cod_gramatura      = fct_lump_prop(cod_gramatura, prop = 0.01)) %>% 
  filter(
    num_comprimento > 0, num_largura > 0,  qtd_volume > 0, 
      vlr_faturamento_bruto > 0, vlr_preco_liquido > 0) %>% 
  select(
      -vlr_despesa_financeira, -vlr_faturamento_bruto,
      -vlr_comissao, -vlr_despesa_venda, -vlr_frete_unitario,
      -nom_cidade, -vlr_faturamento_sem_imposto, -vlr_impostos,
      -vlr_taxa_nota, -vlr_faturamento_sem_ipi, -vlr_frete) %>% 
  drop_na()

glimpse(ibema_prep)

create_report(ibema_prep, 
              output_file = 'ibema_prep.html', config = report_config)

saveRDS(ibema, "00_data/ibema.rds")
saveRDS(ibema_prep, "00_data/ibema_prep.rds")


ibema_sp <- filter(ibema_prep, cod_uf == 'SP')
create_report(ibema_sp, 
              output_file = 'ibema_sp.html', config = report_config)

ibema_sp_bobina <- filter(ibema_sp, cod_tipo_embalagem == 'BOBINA')
create_report(ibema_sp_bobina, 
              output_file = 'ibema_sp_bobina.html', config = report_config)

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

ibema_final <- ibema_prep

# MACHINE LEARNING ----

# data splitting 01 
treino <- subset(ibema_final, dat_movimento <= "2020-03-29") 
#treino entre o período de jan/2018 - mar/2020
teste <- subset(ibema_final, dat_movimento > "2020-03-29")
#teste entre o período de mar/2020 - mar/2021

# Regressão ----
mod_reg <- lm(qtd_volume ~ cod_site + cod_produto + vlr_preco_liquido +
                cod_uf + num_largura + num_comprimento + cod_tipo_embalagem +
                cod_gramatura + cod_configuracao, data = treino)

summary(mod_reg)
# Multiple R-squared: 0.1336

pred_reg <- predict(mod_reg, test)
summary(pred_reg)

par(mfrow = c(1,2))
plot(treino$qtd_volume, mod_reg$fitted.values, 
     main = "Regressão - Treino",
     xlab = "QUANTIDADE VOLUME", ylab = "Fitted")
plot(teste$qtd_volume, pred_reg, 
     main = "Regressão - Teste", 
     xlab = "QUANTIDADE VOLUME", ylab = "Fitted")

modelo_reg <- cbind(teste$qtd_volume, pred_reg)
colnames(modelo_reg) <- c('Actual', 'Predicted')
modelo_reg <- as.data.frame(modelo_reg)

mse_regre <- mean((modelo_reg$Actual - modelo_reg$Predicted)**2)
rmse_regre <- sqrt(mse_regre)
rmse_regre
# 50.18698

# Random Forest ----
rf_spec <- rand_forest(mode = "regression") %>% 
  set_engine("ranger")

# com todas as variáveis
rf_fit <- rf_spec %>% 
  fit(qtd_volume ~ ., data = treino)

rf_fit
# R squared (OOB): 0.3843352

# com as mesmas variáveis da regressão
rf_fit2 <- rf_spec %>% 
  fit(qtd_volume ~ cod_site + cod_produto + vlr_preco_liquido +
        cod_uf + num_largura + num_comprimento + cod_tipo_embalagem +
        cod_gramatura + cod_configuracao, data = treino)

rf_fit2
# R squared (OOB): 0.3817225 

pred_random <- predict(rf_fit, teste)

modelo_random <- cbind(teste$qtd_volume, pred_random)
colnames(modelo_random) <- c('Actual', 'Predicted')
modelo_random <- as.data.frame(modelo_random)

mse_forest <- mean((modelo_random$Actual - modelo_random$Predicted)**2)
rmse_forest <- sqrt(mse_forest)
rmse_forest
# 48.77711

# * H2O Auto ML ----
library(h2o)

# Start a clean h2o cluster
Sys.setenv(JAVA_HOME = "C:/Program Files (x86)/Java/jre1.8.0_281")
h2o.init()
h2o.removeAll()

# The training set (convert to an H2OFrame)
ibema_train <- as.h2o(treino)

# The test set (convert to an H2OFrame)
ibema_test <- as.h2o(teste)

# Take a look at the training set
h2o.describe(ibema_train)

# Identify the response column
y <- "qtd_volume"

# Identify the predictor columns (remove response and ID column)
x <- setdiff(names(ibema_train), y)


# ** H2O AutoML Training ----

# Execute an AutoML run for 20 models
aml <- h2o.automl(
  y              = y, 
  x              = x, 
  training_frame = ibema_train,
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
best_perf <- h2o.performance(best, newdata = ibema_test)
best_perf
# MSE:  2254.297
# RMSE:  47.47943
# MAE:  31.53385
# RMSLE:  NaN
# Mean Residual Deviance :  2254.297

best_pred <- as.vector(h2o.predict(best, newdata = ibema_test))

test_pred <- bind_cols(actual = test$qtd_volume, pred = best_pred)

ggplot(test_pred, aes(x = actual, y = pred)) + 
  geom_abline(col = "green", lty = 2, lwd = 1) + 
  geom_point(alpha = 0.5) + 
  coord_fixed(ratio = 1)

rmse_vec(test_pred$actual, test_pred$pred)
# 47.47943

mae_vec(test_pred$actual, test_pred$pred)
# 31.53385

rsq_trad_vec(test_pred$actual, test_pred$pred)
# 0.2067433

100 - smape_vec(test_pred$actual, test_pred$pred)
# 20.06842

# Performance on the test set, to compare with the other algos
ml_perf <- h2o.performance(metalearner, newdata = ibema_test)
ml_perf

ml_pred <- as.vector(h2o.predict(best, newdata = ibema_test))

test_pred <- bind_cols(actual = test$qtd_volume, pred = ml_pred)

ggplot(test_pred, aes(x = actual, y = pred)) + 
  geom_abline(col = "green", lty = 2, lwd = 1) + 
  geom_point(alpha = 0.5) + 
  coord_fixed(ratio = 1)

rmse_vec(test_pred$actual, test_pred$pred)
# 47.47943

mae_vec(test_pred$actual, test_pred$pred)
# 31.53385

rsq_trad_vec(test_pred$actual, test_pred$pred)
# 0.2067433

100 - smape_vec(test_pred$actual, test_pred$pred)
# 20.06842
