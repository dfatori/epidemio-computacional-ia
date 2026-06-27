# ══════════════════════════════════════════════════════════════════════════════
# NHANES 2021-2023 — Árvore de Decisão e Random Forest para HAS
# Rode setup.R antes para ter nhanes_cardio disponível
# ══════════════════════════════════════════════════════════════════════════════

# ── 0. Pacotes ────────────────────────────────────────────────────────────────
# Pacotes carregados em 00_setup.R

# ── 1. Dados ─────────────────────────────────────────────────────────────────
# dados, dados_treino e dados_teste preparados pelo 00_setup.R

# ══════════════════════════════════════════════════════════════════════════════
# 3. ÁRVORE DE DECISÃO SIMPLES
# ══════════════════════════════════════════════════════════════════════════════

# 3.1 Árvore rasa — profundidade 3, sem poda (cp = 0)
arvore_nativa <- rpart::rpart(
  has_pas ~ .,
  data    = dados_treino,
  control = rpart::rpart.control(
    maxdepth = 3,  # máximo de 3 níveis de profundidade (árvore rasa / stump)
    minsplit = 5,  # nó precisa de ao menos 5 observações para tentar um split
    cp = -1)       # desativa o critério de parada por ganho mínimo
)

png("output/02_random_forest/decision_tree_rasa.png", width = 900, height = 700, res = 120)
rpart.plot(
  arvore_nativa,
  type          = 2,
  extra         = 104,
  under         = TRUE,
  fallen.leaves = TRUE,
  box.palette   = "BuGn",
  roundint      = FALSE,
  ycompress     = FALSE,
  yesno         = 0
)
dev.off()

# ══════════════════════════════════════════════════════════════════════════════
# 3.5 ÁRVORE PROFUNDA
# ══════════════════════════════════════════════════════════════════════════════
# Removemos os limites de profundidade para mostrar até onde o algoritmo
# consegue subdividir os dados. O resultado é uma árvore que memoriza o
# treino (overfitting) — ilegível e inútil em produção, mas ilustra por que
# controlamos tree_depth na prática.

arvore_profunda <- rpart::rpart(
  has_pas ~ .,
  data    = dados_treino,
  control = rpart::rpart.control(
    maxdepth  = 4,   # 4 níveis
    minsplit  = 20,  # padrão do rpart; evita splits em grupos minúsculos
    cp        = -1)
)

cat("Nós na árvore rasa:   ", nrow(arvore_nativa$frame), "\n")
cat("Nós na árvore profunda:", nrow(arvore_profunda$frame), "\n")

png("output/02_random_forest/decision_tree_profunda.png", width = 2800, height = 1800, res = 120)
rpart.plot(
  arvore_profunda,
  type          = 2,
  extra         = 104,
  under         = TRUE,
  fallen.leaves = FALSE,  # evita comprimir tudo na base
  box.palette   = "BuGn",
  roundint      = FALSE,
  ycompress     = FALSE,
  yesno         = 0,
  cex           = 1.4
)
dev.off()

# 3.2 Avaliar árvore rasa no teste (via tidymodels para consistência)
espec_arvore <- decision_tree(tree_depth = 3, min_n = 5, cost_complexity = 0.001) %>%
  set_engine("rpart") %>%
  set_mode("classification")

modelo_arvore <- espec_arvore %>% fit(has_pas ~ ., data = dados_treino)

predict(modelo_arvore, dados_teste) %>%
  bind_cols(dados_teste %>% select(has_pas)) %>%
  metrics(truth = has_pas, estimate = .pred_class)

# ══════════════════════════════════════════════════════════════════════════════
# 4. RANDOM FOREST
# ══════════════════════════════════════════════════════════════════════════════

# 4.1 Especificação
espec_rf <- rand_forest(
  trees = 500,
  mtry  = floor(sqrt(ncol(dados_treino) - 1)) # regra padrao para classificacao
) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

# 4.2 Ajuste
modelo_rf <- espec_rf %>%
  fit(has_pas ~ ., data = dados_treino)

# 4.3 Avaliar no teste — ROC + AUC
# Usamos predict(type="prob") igual aos outros scripts para comparação direta
probs_teste_rf <- predict(modelo_rf, dados_teste, type = "prob") %>%
  bind_cols(dados_teste %>% 
              select(has_pas))

col_evento <- paste0(".pred_", levels(dados_teste$has_pas)[1])
probs_roc_rf <- probs_teste_rf %>% rename(prob_evento = all_of(col_evento))

auc_rf <- roc_auc(probs_roc_rf, truth = has_pas, prob_evento)
cat("AUC do Random Forest:", round(auc_rf$.estimate, 3), "\n")

roc_df_rf <- roc_curve(probs_roc_rf, truth = has_pas, prob_evento)
ponto_otimo_rf <- roc_df_rf %>%
  mutate(youden = sensitivity + specificity - 1) %>%
  slice_max(youden, n = 1)

cat(sprintf(
  "Limiar ótimo (Youden): %.3f | Sensibilidade: %.3f | Especificidade: %.3f\n",
  ponto_otimo_rf$.threshold, ponto_otimo_rf$sensitivity, ponto_otimo_rf$specificity
))

p_roc_rf <- roc_df_rf %>%
  autoplot() +
  geom_point(
    data  = ponto_otimo_rf,
    aes(x = 1 - specificity, y = sensitivity),
    color = "#d73027", size = 3.5
  ) +
  annotate(
    "text",
    x     = 1 - ponto_otimo_rf$specificity + 0.03,
    y     = ponto_otimo_rf$sensitivity - 0.04,
    label = sprintf("Limiar ótimo\n(Youden = %.2f)", max(roc_df_rf$sensitivity + roc_df_rf$specificity - 1, na.rm = TRUE)),
    hjust = 0, size = 3.2, color = "#d73027"
  ) +
  annotate(
    "text",
    x = 0.72, y = 0.18,
    label = paste0("AUC = ", round(auc_rf$.estimate, 3)),
    size = 4.5, fontface = "bold", color = "#2166ac", hjust = 0
  ) +
  labs(
    title   = "Curva ROC: Random Forest",
    x       = "1 - Especificidade (taxa de falsos positivos)",
    y       = "Sensibilidade (taxa de verdadeiros positivos)",
    caption = paste0(
      "AUC = ", round(auc_rf$.estimate, 3),
      " | Diagonal pontilhada = modelo aleatório (AUC = 0,5)",
      " | Ponto vermelho = limiar ótimo (Youden)"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(color = "gray40", size = 9))
print(p_roc_rf)
ggsave("output/02_random_forest/random_forest_roc.png", plot = p_roc_rf,
       width = 7, height = 6, dpi = 150, bg = "white")

# 4.4 Importância das variáveis
rotulos_rf <- c(
  "idade"        = "Idade",
  "whr"          = "Razão cintura/estatura",
  "renda"        = "Renda",
  "colesterol"   = "Colesterol",
  "sedentarismo" = "Sedentarismo (min/dia)",
  "sexo"         = "Sexo",
  "raca"         = "Raça/Etnia",
  "diabetes"     = "Diabetes",
  "fumante"      = "Tabagismo"
)

imp_rf <- vi(modelo_rf) %>%
  mutate(Variable = dplyr::recode(Variable, !!!rotulos_rf))

p_vip_rf <- ggplot(imp_rf, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(fill = "#2166ac", color = "white") +
  labs(title = "Importância de cada preditor",
       x = "Redução média de impureza de Gini (média sobre 500 árvores)", y = NULL) +
  theme_minimal(base_size = 13)
print(p_vip_rf)
ggsave("output/02_random_forest/random_forest_importancia.png", plot = p_vip_rf,
       width = 7, height = 5, dpi = 150, bg = "white")

# ══════════════════════════════════════════════════════════════════════════════
# 4.5 CONVERGÊNCIA DO ERRO OOB vs Nº DE ÁRVORES
# ══════════════════════════════════════════════════════════════════════════════
# Out-of-Bag (OOB): cada árvore é treinada em ~63% dos dados (bootstrap).
# Os 37% restantes são usados como "teste embutido" — sem tocar nos dados_teste.
# À medida que adicionamos árvores, o erro OOB estabiliza: a partir de certo
# ponto, mais árvores não melhoram o modelo — apenas custam mais tempo.
# Esse gráfico mostra onde está esse ponto de estabilização.

n_trees_seq <- c(1, 5, 10, 25, 50, 75, 100, 150, 200, 300, 400, 500)

oob_seq <- sapply(n_trees_seq, function(n) {
  ranger::ranger(
    has_pas ~ ., data = dados_treino,
    num.trees   = n,
    probability = TRUE,
    seed        = 42
  )$prediction.error
})

oob_df <- data.frame(n_arvores = n_trees_seq, erro_oob = oob_seq)

p_oob <- ggplot(oob_df, aes(x = n_arvores, y = erro_oob)) +
  geom_line(color = "#2166ac", linewidth = 1.2) +
  geom_point(color = "#2166ac", size = 2.5) +
  geom_vline(xintercept = 100, linetype = "dashed", color = "#d73027", linewidth = 0.8) +
  annotate("text", x = 108, y = max(oob_seq) * 0.98,
           label = "~100 árvores:\nerro estabiliza",
           hjust = 0, size = 3.5, color = "#d73027") +
  scale_x_continuous(breaks = c(1, 25, 50, 100, 200, 300, 400, 500)) +
  labs(
    title    = "Convergência do Erro",
    subtitle = "Erro Out-of-Bag calculado nos ~37% de dados não utilizados",
    x        = "Número de árvores",
    y        = "Erro OOB",
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.caption = element_text(color = "gray40", size = 9))
print(p_oob)
ggsave("output/02_random_forest/random_forest_oob_convergencia.png", plot = p_oob,
       width = 8, height = 5, dpi = 150, bg = "white")

# ══════════════════════════════════════════════════════════════════════════════
# 5. COMPARAÇÃO COM REGRESSÃO LOGÍSTICA (baseline)
# ══════════════════════════════════════════════════════════════════════════════
if (exists("auc_lr")) {
  cat(sprintf(
    "\nAUC  Regressão Logística : %.3f (baseline)\nAUC  Random Forest        : %.3f\nGanho                     : %+.3f\n",
    auc_lr$.estimate, auc_rf$.estimate, auc_rf$.estimate - auc_lr$.estimate
  ))
} else {
  cat("\n[Comparação] Rode 01_logistic_regression.R antes para ver o ganho sobre o baseline.\n")
}

# ══════════════════════════════════════════════════════════════════════════════
# fim do script ────────────────────────────────────────────────────────────────
# ══════════════════════════════════════════════════════════════════════════════