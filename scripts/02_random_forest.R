# ══════════════════════════════════════════════════════════════════════════════
# NHANES 2021-2023 — Árvore de Decisão e Random Forest para Infarto
# Rode setup.R antes para ter nhanes_cardio disponível
# ══════════════════════════════════════════════════════════════════════════════

# ── 0. Pacotes ────────────────────────────────────────────────────────────────
# Pacotes carregados em 00_setup.R

# ── 1. Dados ─────────────────────────────────────────────────────────────────
# dados, dados_treino e dados_teste preparados pelo 00_setup.R

# ══════════════════════════════════════════════════════════════════════════════
# 3. ÁRVORE DE DECISÃO SIMPLES
# ══════════════════════════════════════════════════════════════════════════════

# 3.1 Especificação
espec_arvore <- decision_tree(
  tree_depth = 3,  # rasa para facilitar visualizacao
  min_n      = 5
) %>%
  set_engine("rpart", model = TRUE,
             parms = list(loss = matrix(c(0, 10, 1, 0), nrow = 2))) %>%  # falso negativo custa 10x mais (desbalanceamento)
  set_mode("classification")

# 3.2 Ajuste
modelo_arvore <- espec_arvore %>%
  fit(infarto ~ ., data = dados_treino)

# 3.3 Visualizar a árvore
arvore_nativa <- extract_fit_engine(modelo_arvore)

png("output/random_forest_stump.png", width = 900, height = 700, res = 120)
rpart.plot(
  arvore_nativa,
  type          = 2,
  extra         = 104,  # % por classe + proporcao de obs no no
  under         = TRUE,
  fallen.leaves = TRUE,
  box.palette   = "BuGn",
  roundint      = FALSE,
  ycompress     = FALSE,
  yesno         = 0     # remove "yes/no" nos galhos numericos
)
dev.off()

# 3.4 Avaliar no teste
predict(modelo_arvore, dados_teste) %>%
  bind_cols(dados_teste %>% select(infarto)) %>%
  metrics(truth = infarto, estimate = .pred_class)

# ══════════════════════════════════════════════════════════════════════════════
# 3.5 ÁRVORE PROFUNDA
# ══════════════════════════════════════════════════════════════════════════════
# Removemos os limites de profundidade para mostrar até onde o algoritmo
# consegue subdividir os dados. O resultado é uma árvore que memoriza o
# treino (overfitting) — ilegível e inútil em produção, mas ilustra por que
# controlamos tree_depth na prática.

espec_profunda <- decision_tree(
  tree_depth = 15,   # permite muito mais subdivisoes
  min_n      = 2     # divide ate nos com apenas 2 observacoes
) %>%
  set_engine("rpart", model = TRUE,
             parms = list(loss = matrix(c(0, 10, 1, 0), nrow = 2))) %>%
  set_mode("classification")

modelo_profundo <- espec_profunda %>%
  fit(infarto ~ ., data = dados_treino)

arvore_profunda <- extract_fit_engine(modelo_profundo)
cat("Nós na árvore rasa:   ", nrow(modelo_arvore  %>% extract_fit_engine() %>% .$frame), "\n")
cat("Nós na árvore profunda:", nrow(arvore_profunda$frame), "\n")

png("output/random_forest_arvore_profunda.png", width = 4000, height = 2800, res = 120)
rpart.plot(
  arvore_profunda,
  type          = 2,
  extra         = 104,
  under         = TRUE,
  fallen.leaves = TRUE,
  box.palette   = "BuGn",
  roundint      = FALSE,
  ycompress     = TRUE,
  yesno         = 0,
  cex           = 2   # fonte maior — aumentamos a imagem para compensar
)
dev.off()

# ══════════════════════════════════════════════════════════════════════════════
# 4. RANDOM FOREST
# ══════════════════════════════════════════════════════════════════════════════

# 4.1 Especificação
espec_rf <- rand_forest(
  trees = 500,
  mtry  = floor(sqrt(ncol(dados_treino) - 1))  # regra padrao para classificacao
) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

# 4.2 Ajuste
modelo_rf <- espec_rf %>%
  fit(infarto ~ ., data = dados_treino)

# 4.3 Avaliar no teste — ROC + AUC
# Usamos predict(type="prob") igual aos outros scripts para comparação direta
probs_teste_rf <- predict(modelo_rf, dados_teste, type = "prob") %>%
  bind_cols(dados_teste %>% 
              select(infarto))

col_evento <- paste0(".pred_", levels(dados_teste$infarto)[1])
probs_roc_rf <- probs_teste_rf %>% rename(prob_evento = all_of(col_evento))

auc_rf <- roc_auc(probs_roc_rf, truth = infarto, prob_evento)
cat("AUC do Random Forest:", round(auc_rf$.estimate, 3), "\n")

roc_df_rf <- roc_curve(probs_roc_rf, truth = infarto, prob_evento)
ponto_otimo_rf <- roc_df_rf %>%
  mutate(youden = sensitivity + specificity - 1) %>%
  slice_max(youden, n = 1)

cat(sprintf(
  "Limiar ótimo (Youden): %.3f  |  Sensibilidade: %.3f  |  Especificidade: %.3f\n",
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
  labs(
    title   = "Curva ROC — Random Forest (NHANES 2021-2023)",
    x       = "1 - Especificidade  (taxa de falsos positivos)",
    y       = "Sensibilidade  (taxa de verdadeiros positivos)",
    caption = paste0(
      "AUC = ", round(auc_rf$.estimate, 3),
      "  |  Diagonal pontilhada = modelo aleatório (AUC = 0,5)",
      "  |  Ponto vermelho = limiar ótimo (Youden)"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(color = "gray40", size = 9))
print(p_roc_rf)
ggsave("output/random_forest_roc.png", plot = p_roc_rf,
       width = 7, height = 6, dpi = 150, bg = "white")

# 4.4 Importância das variáveis
p_vip_rf <- vip(modelo_rf,
  geom       = "col",
  aesthetics = list(fill = "#2166ac", color = "white")
) +
  labs(title = "Importância de cada preditor",
       y = "Redução média de impureza de Gini (média sobre 500 árvores)", x = NULL) +
  theme_minimal(base_size = 13)
print(p_vip_rf)
ggsave("output/random_forest_importancia.png", plot = p_vip_rf,
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
    infarto ~ ., data = dados_treino,
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
    subtitle = "Erro Out-of-Bag calculado nos ~37% de dados não usados em cada árvore",
    x        = "Número de árvores",
    y        = "Erro OOB",
    caption  = "Linha vermelha = ponto de estabilização  |  Mais árvores além desse ponto = custo sem ganho"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.caption = element_text(color = "gray40", size = 9))
print(p_oob)
ggsave("output/random_forest_oob_convergencia.png", plot = p_oob,
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