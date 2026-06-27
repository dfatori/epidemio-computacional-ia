# ══════════════════════════════════════════════════════════════════════════════
# NHANES 2021-2023 — Regressão Linear para Predição de Colesterol Total
# Rode setup.R antes para ter nhanes_cardio disponível
# ══════════════════════════════════════════════════════════════════════════════

# ── 1. Dados ──────────────────────────────────────────────────────────────────
# nhanes_cardio preparado pelo 00_setup.R
# Desfecho contínuo: colesterol total (LBXTC, mg/dL)
# Adicionamos imc e cintura — não usados no split de classificação, mas
# relevantes para predição de colesterol

dados_col <- nhanes_cardio %>%
  select(colesterol, idade, sexo, imc, cintura, hipertens, diabetes, fumante) %>%
  drop_na()

set.seed(42)
split_col  <- initial_split(dados_col, prop = 0.75, strata = colesterol, breaks = 4)
treino_col <- training(split_col)
teste_col  <- testing(split_col)

cat("Treino:", nrow(treino_col), "observações\n")
cat("Teste: ", nrow(teste_col),  "observações\n")
cat("Colesterol médio (treino):", round(mean(treino_col$colesterol), 1), "mg/dL\n")

# ══════════════════════════════════════════════════════════════════════════════
# 2. CONCEITO CENTRAL: MÍNIMOS QUADRADOS ORDINÁRIOS (OLS)
# ══════════════════════════════════════════════════════════════════════════════
# A regressão linear modela o VALOR ESPERADO de Y como função linear dos preditores:
#
#   Ŷ = β₀ + β₁·x₁ + β₂·x₂ + ... + ε      ε ~ N(0, σ²)
#
# Os coeficientes β são escolhidos para MINIMIZAR a soma dos quadrados dos resíduos:
#
#   SQR = Σᵢ (yᵢ − ŷᵢ)²
#
# Diferente da regressão logística:
#   • O desfecho é contínuo (mg/dL), não uma probabilidade
#   • Não há transformação — a saída é diretamente o valor predito
#   • Métricas: RMSE (erro médio em mg/dL) e R² (variância explicada)

# Visualização do conceito OLS com os dados reais do desfecho.
# A dispersão alta é esperada — colesterol é influenciado por genética,
# dieta e uso de estatinas que não estão no modelo. Colorir por sexo
# revela o efeito pós-menopausa: mulheres mais velhas têm colesterol mais alto.
set.seed(42)
demo_ols <- treino_col %>%
  slice_sample(n = 400)

p_ols <- ggplot(demo_ols, aes(x = idade, y = colesterol)) +
  geom_point(aes(color = sexo), alpha = 0.4, size = 1.3) +
  geom_smooth(method = "lm", se = TRUE, color = "#333333", linewidth = 1.5) +
  scale_color_manual(values = c("Masculino" = "#2166ac", "Feminino" = "#d73027")) +
  labs(
    title    = "OLS — Colesterol Total ~ Idade (NHANES 2021-2023)",
    subtitle = "",
    x        = "Idade (anos)",
    y        = "Colesterol total (mg/dL)",
    color    = NULL,
    caption  = "Reta = ajuste OLS geral  |  Faixa = IC 95%  |  Amostra de 400 obs."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position    = "top",
    plot.caption       = element_text(color = "gray40", size = 9)
  )
print(p_ols)
ggsave("output/05_linear_regression/ols_conceito.png", plot = p_ols,
       width = 8, height = 6, dpi = 150, bg = "white")

# ══════════════════════════════════════════════════════════════════════════════
# 3. AJUSTE DO MODELO
# ══════════════════════════════════════════════════════════════════════════════

# 3.1 Recipe — mesma lógica dos scripts anteriores
# step_dummy:     converte fatores em dummies (0/1)
# step_normalize: padroniza contínuas → coeficientes β comparáveis entre si
receita_lm <- recipe(colesterol ~ ., data = treino_col) %>%
  step_dummy(all_nominal_predictors(), one_hot = FALSE) %>%
  step_normalize(all_numeric_predictors())

# 3.2 Especificação
espec_lm <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# 3.3 Workflow
wf_lm <- workflow() %>%
  add_recipe(receita_lm) %>%
  add_model(espec_lm)

# 3.4 Treinar
modelo_lm <- wf_lm %>% fit(data = treino_col)

# ══════════════════════════════════════════════════════════════════════════════
# 4. INTERPRETAÇÃO: COEFICIENTES β
# ══════════════════════════════════════════════════════════════════════════════
# Como usamos step_normalize, cada β representa a variação ESPERADA em mg/dL
# de colesterol por aumento de 1 desvio-padrão no preditor contínuo
# (ou presença vs. ausência para dummies), mantendo os demais constantes.
#
#   β > 0  → preditor associado a maior colesterol
#   β < 0  → preditor associado a menor colesterol
#
# Intervalo de confiança: se IC não cruza 0, o efeito é estatisticamente
# significativo — equivalente ao p < 0.05.

coefs_lm <- tidy(extract_fit_engine(modelo_lm), conf.int = TRUE) %>%
  filter(term != "(Intercept)")

print(coefs_lm %>% select(term, estimate, conf.low, conf.high, p.value))

p_coefs <- ggplot(coefs_lm, aes(x = estimate, y = reorder(term, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2, color = "#555555") +
  geom_point(aes(color = estimate > 0), size = 3) +
  scale_color_manual(
    values = c("TRUE" = "#d73027", "FALSE" = "#2166ac"),
    labels = c("TRUE" = "Aumenta colesterol", "FALSE" = "Reduz colesterol")
  ) +
  labs(
    title    = "Coeficientes β — Regressão Linear (NHANES 2021-2023)",
    subtitle = "Preditores padronizados (β em mg/dL por DP)  |  Barras = IC 95%",
    x        = "Coeficiente β (mg/dL por desvio-padrão)",
    y        = NULL,
    color    = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")
print(p_coefs)
ggsave("output/05_linear_regression/coeficientes.png", plot = p_coefs,
       width = 8, height = 6, dpi = 150, bg = "white")

# ══════════════════════════════════════════════════════════════════════════════
# 5. AVALIAÇÃO: RMSE + R² + DIAGNÓSTICO DE RESÍDUOS
# ══════════════════════════════════════════════════════════════════════════════
# RMSE (Root Mean Squared Error): erro médio na mesma unidade do desfecho (mg/dL)
#   → "em média, o modelo erra X mg/dL"
# R²: proporção da variância de colesterol explicada pelos preditores
#   → 0 = modelo inútil, 1 = modelo perfeito
# MAE: erro absoluto médio (menos sensível a outliers que o RMSE)
#
# PRESSUPOSTOS avaliados pelo diagnóstico de resíduos:
#   1. Linearidade: resíduos sem padrão sistemático vs. valor predito
#   2. Homocedasticidade: variância dos resíduos constante ao longo dos preditos
#   3. Normalidade dos resíduos (importante para inferência dos IC)

preds_teste <- predict(modelo_lm, teste_col) %>%
  bind_cols(teste_col %>% select(colesterol)) %>%
  rename(predito = .pred, observado = colesterol) %>%
  mutate(residuo = observado - predito)

metricas <- preds_teste %>%
  metrics(truth = observado, estimate = predito)
print(metricas)

rmse_val <- metricas %>% filter(.metric == "rmse") %>% pull(.estimate) %>% round(1)
r2_val   <- metricas %>% filter(.metric == "rsq")  %>% pull(.estimate) %>% round(3)
mae_val  <- metricas %>% filter(.metric == "mae")  %>% pull(.estimate) %>% round(1)

cat(sprintf("RMSE: %.1f mg/dL  |  R²: %.3f  |  MAE: %.1f mg/dL\n",
            rmse_val, r2_val, mae_val))

# 5.1 Observado vs. predito
p_pred <- ggplot(preds_teste, aes(x = predito, y = observado)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(alpha = 0.25, color = "#2166ac", size = 1.2) +
  geom_smooth(method = "loess", se = FALSE, color = "#d73027", linewidth = 1.2) +
  labs(
    title    = "Observado vs. Predito — Regressão Linear (NHANES 2021-2023)",
    subtitle = paste0("RMSE = ", rmse_val, " mg/dL  |  R² = ", r2_val,
                      "  |  MAE = ", mae_val, " mg/dL"),
    x        = "Colesterol predito (mg/dL)",
    y        = "Colesterol observado (mg/dL)",
    caption  = "Diagonal pontilhada = predição perfeita  |  Linha vermelha = tendência LOESS"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.caption = element_text(color = "gray40", size = 9))
print(p_pred)
ggsave("output/05_linear_regression/obs_vs_pred.png", plot = p_pred,
       width = 7, height = 6, dpi = 150, bg = "white")

# 5.2 Diagnóstico: resíduos vs. predito
p_resid <- ggplot(preds_teste, aes(x = predito, y = residuo)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(alpha = 0.25, color = "#2166ac", size = 1.1) +
  geom_smooth(method = "loess", se = FALSE, color = "#d73027", linewidth = 1.2) +
  labs(
    title    = "Diagnóstico de Resíduos — Regressão Linear (NHANES 2021-2023)",
    subtitle = "Resíduos devem se distribuir aleatoriamente em torno de 0",
    x        = "Colesterol predito (mg/dL)",
    y        = "Resíduo  (observado − predito)",
    caption  = "Linha vermelha = tendência LOESS  |  Padrão sistemático indica violação de pressupostos"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.caption = element_text(color = "gray40", size = 9))
print(p_resid)
ggsave("output/05_linear_regression/residuos.png", plot = p_resid,
       width = 7, height = 6, dpi = 150, bg = "white")

# ══════════════════════════════════════════════════════════════════════════════
# fim do script ────────────────────────────────────────────────────────────────
# ══════════════════════════════════════════════════════════════════════════════
