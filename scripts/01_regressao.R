# ══════════════════════════════════════════════════════════════════════════════
# NHANES 2021-2023: Regressão Logística para Predição de HAS (baseline)
# Rode 00_config.R antes (ele cria dados, dados_treino e dados_teste)
# ══════════════════════════════════════════════════════════════════════════════

# ── 1. Dados ─────────────────────────────────────────────────────────────────
# dados, dados_treino e dados_teste preparados pelo 00_config.R

# ══════════════════════════════════════════════════════════════════════════════
# 2. CONCEITO CENTRAL: A FUNÇÃO LOGÍSTICA
# ══════════════════════════════════════════════════════════════════════════════
# A regressão logística modela a PROBABILIDADE de um evento (has_pas = "Sim")
# como função linear dos preditores, transformada pela função logística:
#
#   P(Y=1) = 1 / (1 + e^(-(β0 + β1·x1 + β2·x2 + ...)))
#
# O resultado é sempre entre 0 e 1: ao contrário da regressão linear,
# que poderia prever valores absurdos como P = -0.3 ou P = 1.7.
#
# Os coeficientes β são interpretados como LOG-ODDS:
#   e^β = Odds Ratio (OR): quanto a chance de HAS muda por unidade do preditor.

x_vals   <- seq(-6, 6, by = 0.05)
log_vals <- 1 / (1 + exp(-x_vals))

png("output/01_regressao/conceito_funcao_logistica.png", width = 900, height = 700, res = 120)
plot(
  x_vals, log_vals,
  type = "l", lwd = 2.5, col = "#2166ac",
  xlab = "Combinação linear dos preditores  (β₀ + β₁x₁ + ...)",
  ylab = "P(HAS = Sim)",
  main = "Função Logística: Transforma Score Linear em Probabilidade",
  las  = 1, bty = "l", ylim = c(-0.05, 1.1)
)
abline(h = c(0, 0.5, 1), lty = 2, col = "gray70")
abline(v = 0,             lty = 2, col = "gray70")
points(0, 0.5, pch = 19, col = "#d73027", cex = 1.8)
text(0.3, 0.50, "Score = 0  →  P = 0,5\n(limiar de decisão padrão)", adj = 0, cex = 0.8)
text(-5.5, 0.08, "Score muito\nnegativo\nP ≈ 0", cex = 0.8, col = "gray40")
text( 4.5, 0.93, "Score muito\npositivo\nP ≈ 1", cex = 0.8, col = "gray40")
mtext(expression(P == frac(1, 1 + e^{-(beta[0] + beta[1]*x[1] + ldots)})),
      side = 3, line = 0.2, cex = 0.9, col = "#2166ac")
dev.off()

# ══════════════════════════════════════════════════════════════════════════════
# 3. AJUSTE DO MODELO
# ══════════════════════════════════════════════════════════════════════════════

# 3.1 Recipe
# step_dummy: converte fatores em dummies (0/1)
# step_normalize: padroniza contínuas → coeficientes comparáveis entre si
receita_lr <- recipe(has_pas ~ ., data = dados_treino) %>%
  step_dummy(all_nominal_predictors(), one_hot = FALSE) %>%
  step_normalize(all_numeric_predictors())

# 3.2 Especificação
espec_lr <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

# 3.3 Workflow
wf_lr <- workflow() %>%
  add_recipe(receita_lr) %>%
  add_model(espec_lr)

# 3.4 Treinar
modelo_lr <- wf_lr %>% fit(data = dados_treino)

# ══════════════════════════════════════════════════════════════════════════════
# 4. INTERPRETAÇÃO: ODDS RATIOS
# ══════════════════════════════════════════════════════════════════════════════
# O coeficiente β de cada preditor é o log-odds.
#   e^β = Odds Ratio: quanto a CHANCE de HAS multiplica por unidade do preditor.
#   OR > 1 → aumenta o risco
#   OR < 1 → reduz o risco
#   OR = 1 → sem efeito
#
# ATENÇÃO: como aplicamos step_normalize(), os preditores contínuos foram
# padronizados (média 0, dp 1). O OR de idade, colesterol etc. é, portanto,
# por +1 DESVIO-PADRÃO: não por unidade original (ano, mg/dL).
#
# Intervalo de confiança: se o IC não cruza 1, o efeito é estatisticamente
# significativo (equivalente ao p < 0.05 no modelo logístico).

coefs <- tidy(extract_fit_engine(modelo_lr), conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    OR      = exp(estimate),
    OR_low  = exp(conf.low),
    OR_high = exp(conf.high),
    term    = dplyr::recode(term,
      "idade"                 = "Idade",
      "whr"                   = "Razão cintura/estatura",
      "renda"                 = "Renda",
      "colesterol"            = "Colesterol",
      "sedentarismo"          = "Sedentarismo (min/dia)",
      "sexo_Feminino"         = "Sexo (feminino)",
      "raca_Branco"           = "Raça (branco)",
      "diabetes_Sim"          = "Diabetes (sim)",
      "fumante_Ex.fumante"    = "Ex-fumante",
      "fumante_Fumante.atual" = "Fumante atual"
      # termos sem tradução aqui mantêm o nome bruto (acontece no exercicio.R)
    )
  )

print(coefs %>% select(term, OR, OR_low, OR_high, p.value))

# Gráfico de forest plot dos Odds Ratios
p_or <- ggplot(coefs, aes(x = OR, y = reorder(term, OR))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(xmin = OR_low, xmax = OR_high), height = 0.2, color = "#555555",
                orientation = "y") +
  geom_point(aes(color = OR > 1), size = 3) +
  scale_color_manual(values = c("TRUE" = "#d73027", "FALSE" = "#2166ac"),
                     labels = c("TRUE" = "Aumenta risco", "FALSE" = "Reduz risco")) +
  labs(
    title    = "Odds Ratios: Regressão Logística (NHANES 2021-2023)",
    subtitle = "Linha vertical = OR 1 (sem efeito)  |  Barras = IC 95%",
    x        = "Razão de Chances (Odds Ratio)",
    y        = NULL,
    color    = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")
print(p_or)
ggsave("output/01_regressao/logistic_odds_ratios.png", plot = p_or,
       width = 8, height = 6, dpi = 150, bg = "white")

# ══════════════════════════════════════════════════════════════════════════════
# 5. AVALIAÇÃO: CURVA ROC + AUC
# ══════════════════════════════════════════════════════════════════════════════
# Usamos ROC + AUC: mesma abordagem dos scripts seguintes, para comparação.
# Este bloco de avaliação é IDÊNTICO nos scripts 01-04 (repetição proposital:
# cada script é autocontido e a métrica comum permite comparar os modelos).
# O evento positivo é has_pas = "Sim": como os níveis do fator são
# c("Não", "Sim"), passamos event_level = "second" explicitamente.

probs_teste <- predict(modelo_lr, dados_teste, type = "prob") %>%
  bind_cols(dados_teste %>% select(has_pas))

probs_roc <- probs_teste %>%
  rename(prob_evento = .pred_Sim)   # probabilidade do evento: HAS = "Sim"

auc_lr <- roc_auc(probs_roc, truth = has_pas, prob_evento, event_level = "second")
cat("AUC da Regressão Logística:", round(auc_lr$.estimate, 3), "\n")

# Ponto ótimo (critério de Youden): maximiza sensibilidade + especificidade
# J = sens + spec - 1  →  equivalente a minimizar a distância ao canto (0,1)
roc_df <- roc_curve(probs_roc, truth = has_pas, prob_evento, event_level = "second")
ponto_otimo <- roc_df %>%
  mutate(youden = sensitivity + specificity - 1) %>%
  slice_max(youden, n = 1)

cat(sprintf(
  "Limiar ótimo (Youden): %.3f  |  Sensibilidade: %.3f  |  Especificidade: %.3f\n",
  ponto_otimo$.threshold, ponto_otimo$sensitivity, ponto_otimo$specificity
))

p_roc <- roc_df %>%
  autoplot() +
  geom_point(
    data  = ponto_otimo,
    aes(x = 1 - specificity, y = sensitivity),
    color = "#d73027", size = 3.5
  ) +
  annotate(
    "text",
    x     = 1 - ponto_otimo$specificity + 0.03,
    y     = ponto_otimo$sensitivity - 0.04,
    label = sprintf("Limiar ótimo\n(Youden = %s)", fmt_br(ponto_otimo$youden, 2)),
    hjust = 0, size = 3.2, color = "#d73027"
  ) +
  annotate(
    "text",
    x = 0.72, y = 0.18,
    label = paste0("AUC = ", fmt_br(auc_lr$.estimate)),
    size = 4.5, fontface = "bold", color = "#2166ac", hjust = 0
  ) +
  labs(
    title    = "Curva ROC: Regressão Logística",
    subtitle = "Análise 1 (9 preditores): NHANES 2021-2023",
    x        = "1 − Especificidade  (taxa de falsos positivos)",
    y        = "Sensibilidade  (taxa de verdadeiros positivos)",
    caption  = "Diagonal pontilhada = modelo aleatório (AUC = 0,5)  |  Ponto vermelho = limiar ótimo (Youden)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(color = "gray40", size = 9))
print(p_roc)
ggsave("output/01_regressao/logistic_roc.png", plot = p_roc,
       width = 7, height = 6, dpi = 150, bg = "white")

# ══════════════════════════════════════════════════════════════════════════════
# fim do script ────────────────────────────────────────────────────────────────
# ══════════════════════════════════════════════════════════════════════════════