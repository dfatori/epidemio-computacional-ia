# ══════════════════════════════════════════════════════════════════════════════
# NHANES 2021-2023 — Comparação de Modelos: Supercontrole vs DAG vs Causal Forest
#
# Pergunta causal: qual o efeito do tabagismo (fumante) sobre infarto?
#
# Modelo 1 — Regressão logística com supercontrole (inclui mediadores)
# Modelo 2 — Regressão logística com ajuste correto pelo DAG
# Modelo 3 — Causal Forest com ajuste correto pelo DAG
#
# Rode setup.R antes para ter nhanes_cardio disponível
# ══════════════════════════════════════════════════════════════════════════════

# ── 0. Pacotes ────────────────────────────────────────────────────────────────
library(dplyr)
library(tidyr)   # drop_na()
library(ggplot2)
library(broom)   # tidy() para extrair coeficientes
library(grf)     # causal_forest()

# ── 1. Preparar dados ─────────────────────────────────────────────────────────
# Remove NAs em todas as variáveis usadas nos 3 modelos
# (mesmo conjunto para comparação justa)
dados <- nhanes_cardio %>%
  select(infarto, fumante, idade, sexo, raca, renda,
         escolaridade, est_civil, imc, hipertens, diabetes, colesterol) %>%
  drop_na() %>%
  mutate(
    # grf exige numérico
    infarto_num = as.integer(infarto  == "Sim"),
    fumante_num = as.integer(fumante  == "Sim")
  )

# ══════════════════════════════════════════════════════════════════════════════
# MODELO 1 — Regressão logística com SUPERCONTROLE
# Inclui mediadores (hipertens, diabetes, colesterol) — ERRADO causalmente.
# O efeito de fumante será atenuado porque parte do efeito real foi
# "absorvida" pelos mediadores incluídos como covariáveis.
# ══════════════════════════════════════════════════════════════════════════════
modelo1 <- glm(
  infarto ~ fumante + idade + sexo + raca + renda + escolaridade + est_civil +
            imc + hipertens + diabetes + colesterol,
  data   = dados,
  family = binomial
)

# ══════════════════════════════════════════════════════════════════════════════
# MODELO 2 — Regressão logística com ajuste CORRETO (via DAG)
# Apenas confundidores identificados pelo dagitty::adjustmentSets():
#   idade, sexo, raca, renda, escolaridade, est_civil
# Mediadores (hipertens, diabetes, colesterol, imc) EXCLUÍDOS.
# ══════════════════════════════════════════════════════════════════════════════
modelo2 <- glm(
  infarto ~ fumante + idade + sexo + raca + renda + escolaridade + est_civil,
  data   = dados,
  family = binomial
)

# ── Extrair OR e IC 95% dos dois modelos ──────────────────────────────────────
extrair_or <- function(modelo, nome_modelo) {
  tidy(modelo, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term == "fumanteSim") %>%
    mutate(modelo = nome_modelo)
}

resultados_logit <- bind_rows(
  extrair_or(modelo1, "Modelo 1\nSupercontrole\n(com mediadores)"),
  extrair_or(modelo2, "Modelo 2\nDAG correto\n(sem mediadores)")
)

# ── Gráfico de comparação dos ORs ─────────────────────────────────────────────
p_or <- ggplot(resultados_logit,
               aes(x = modelo, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  geom_errorbar(width = 0.15, linewidth = 0.8, color = "#2c3e50") +
  geom_point(size = 4, color = "#e74c3c") +
  geom_text(aes(label = round(estimate, 2)), vjust = -1, size = 4) +
  scale_y_continuous(limits = c(0.5, NA)) +
  labs(
    title    = "Efeito do tabagismo sobre infarto: supercontrole vs ajuste correto",
    subtitle = "OR < no Modelo 1 porque mediadores absorvem parte do efeito real",
    x        = NULL,
    y        = "Odds Ratio (IC 95%)"
  ) +
  theme_bw(base_size = 13)

print(p_or)
ggsave("output/causal_inference_comparacao_or.png", plot = p_or, width = 8, height = 6, dpi = 150)

# ══════════════════════════════════════════════════════════════════════════════
# MODELO 3 — Causal Forest (pacote grf)
#
# W (tratamento) : fumante (0/1)
# Y (desfecho)   : infarto (0/1)
# X (covariáveis): apenas confundidores do DAG — mesmo ajuste do Modelo 2
#                  grf exige matriz numérica → model.matrix() converte fatores
#
# O que o Causal Forest estima: τ(x) = E[Y(1) - Y(0) | X = x]
# Efeito individual do tabagismo para cada perfil de covariáveis.
# ══════════════════════════════════════════════════════════════════════════════
X <- model.matrix(
  ~ idade + sexo + raca + renda + escolaridade + est_civil - 1,
  data = dados
)

Y <- dados$infarto_num
W <- dados$fumante_num

set.seed(42)
cf <- causal_forest(
  X         = X,
  Y         = Y,
  W         = W,
  num.trees = 2000
)

# ── ATE — Efeito médio de tratamento (comparável ao OR do Modelo 2) ───────────
ate <- average_treatment_effect(cf, target.sample = "all")
cat("\n── ATE (Causal Forest) ────────────────────────────────\n")
cat(sprintf("Diferença de risco média: %.4f  (EP: %.4f)\n", ate["estimate"], ate["std.err"]))
cat(sprintf("IC 95%%: [%.4f, %.4f]\n",
            ate["estimate"] - 1.96 * ate["std.err"],
            ate["estimate"] + 1.96 * ate["std.err"]))

# ── Efeitos individuais τ̂(xᵢ) ────────────────────────────────────────────────
dados$tau_hat <- predict(cf)$predictions

# ── Gráfico de distribuição dos efeitos individuais ───────────────────────────
p_dist <- ggplot(dados, aes(x = tau_hat)) +
  geom_histogram(bins = 50, fill = "#2980b9", color = "white", alpha = 0.85) +
  geom_vline(xintercept = ate["estimate"], color = "#e74c3c",
             linetype = "dashed", linewidth = 1) +
  annotate("text",
           x     = ate["estimate"] + 0.005,
           y     = Inf, vjust = 2, hjust = 0,
           label = paste0("ATE = ", round(ate["estimate"], 3)),
           color = "#e74c3c", size = 4) +
  labs(
    title    = "Causal Forest: distribuição dos efeitos individuais (CATE)",
    subtitle = "Cada barra = número de indivíduos com aquele efeito estimado do tabagismo",
    x        = expression(hat(tau)(x[i]) ~ " — efeito individual do tabagismo sobre P(infarto)"),
    y        = "Frequência"
  ) +
  theme_bw(base_size = 13)

print(p_dist)
ggsave("output/causal_inference_cate.png", plot = p_dist, width = 9, height = 6, dpi = 150)

# ── Heterogeneidade por idade ─────────────────────────────────────────────────
# Divide idade em quartis e compara a distribuição de τ̂ entre grupos
dados <- dados %>%
  mutate(faixa_idade = cut(idade,
                           breaks   = quantile(idade, probs = c(0, .25, .5, .75, 1)),
                           labels   = c("Q1 (mais jovens)", "Q2", "Q3", "Q4 (mais velhos)"),
                           include.lowest = TRUE))

p_hetero <- ggplot(dados, aes(x = faixa_idade, y = tau_hat, fill = faixa_idade)) +
  geom_boxplot(alpha = 0.75, outlier.size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title    = "Heterogeneidade do efeito do tabagismo por faixa etária",
    subtitle = "Efeito estimado pelo Causal Forest varia entre grupos",
    x        = "Quartil de idade",
    y        = expression(hat(tau)(x[i])),
    fill     = NULL
  ) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none")

print(p_hetero)
ggsave("output/causal_inference_heterogeneidade.png",
       plot = p_hetero, width = 9, height = 6, dpi = 150)

# ── Resumo comparativo final ──────────────────────────────────────────────────
cat("\n══ RESUMO COMPARATIVO ══════════════════════════════════════════\n")
cat(sprintf("Modelo 1 (supercontrole) — OR fumante: %.3f [%.3f, %.3f]\n",
            resultados_logit$estimate[1],
            resultados_logit$conf.low[1],
            resultados_logit$conf.high[1]))
cat(sprintf("Modelo 2 (DAG correto)   — OR fumante: %.3f [%.3f, %.3f]\n",
            resultados_logit$estimate[2],
            resultados_logit$conf.low[2],
            resultados_logit$conf.high[2]))
cat(sprintf("Modelo 3 (Causal Forest) — ATE:        %.4f (EP: %.4f)\n",
            ate["estimate"], ate["std.err"]))
cat("(ATE = diferença de risco absoluta; OR = razão de chances)\n")
cat("════════════════════════════════════════════════════════════════\n")

# ══════════════════════════════════════════════════════════════════════════════
# fim do script ────────────────────────────────────────────────────────────────
# ══════════════════════════════════════════════════════════════════════════════