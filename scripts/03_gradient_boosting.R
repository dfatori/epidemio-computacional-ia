
# ══════════════════════════════════════════════════════════════════════════════
# NHANES 2021-2023 — Gradient Boosting para Predição de Infarto (foco didático)
# Rode setup.R antes para ter nhanes_cardio disponível
# ══════════════════════════════════════════════════════════════════════════════

# ── 0. Pacotes ────────────────────────────────────────────────────────────────
# Pacotes carregados em 00_setup.R

# ── 1. Dados ─────────────────────────────────────────────────────────────────

# ══════════════════════════════════════════════════════════════════════════════
# 2. CONCEITO CENTRAL: O "APRENDIZ FRACO" — STUMP (PROFUNDIDADE 1)
# ══════════════════════════════════════════════════════════════════════════════
# Um stump é uma árvore com apenas 1 split: divide os dados em dois grupos
# usando UMA variável e UM ponto de corte.
# Sozinho, é um preditor muito fraco — acerta pouco mais que o acaso.
# O Gradient Boosting usa centenas desses stumps em sequência,
# cada um corrigindo os erros do anterior. A força vem da soma, não do indivíduo.

# Usamos gbm() diretamente para tornar o processo iterativo VISÍVEL:
#   interaction.depth = 1  → stump (o aprendiz fraco clássico)
#   n.trees            → número de iterações (quantos stumps serão somados)
#   shrinkage          → learning rate: quanto cada stump "pesa" na soma final
#   cv.folds           → cross-validation interna para detectar o n ótimo de árvores

# gbm() exige desfecho numérico (0/1) para distribuição "bernoulli"
# Mantemos os preditores como fator — gbm aceita fator mas não character
treino_gbm <- dados_treino %>%
  mutate(
    infarto_num = as.integer(infarto == "Sim" | infarto == "Yes")
  )

# ── 2.1 Visualizar aprendiz fraco ─────────────────────────────────
stump <- rpart::rpart(
  infarto ~ idade + sexo + hipertens + imc + colesterol + diabetes + fumante + sedentarismo,
  data    = dados_treino,
  control = rpart::rpart.control(maxdepth = 1, minsplit = 1, minbucket = 1, cp = -1)
)
png("output/gradient_boosting_stump.png", width = 800, height = 600, res = 120)
rpart.plot(
  stump,
  type          = 2,
  extra         = 104,
  fallen.leaves = TRUE,
  box.palette   = "BuGn",
  roundint      = FALSE,
  main          = "Aprendiz fraco: árvore simples sem divisão."
)
dev.off()

set.seed(42)

# Modelo GBM completo: 500 stumps encadeados em sequência (interaction.depth = 1).
# Cada árvore é um stump, mas o ensemble das 500 é o gradient boosting em si.
modelo_gbm <- gbm(
  infarto_num ~ idade + sexo + hipertens + imc + colesterol + diabetes + fumante + sedentarismo,
  data             = treino_gbm,
  distribution     = "bernoulli",   # log-loss — adequada para classificação binária
  n.trees          = 500,           # máximo de stumps a treinar
  interaction.depth = 1,            # STUMP: 1 split por árvore
  shrinkage        = 0.05,          # learning rate (menor = mais conservador, precisa mais árvores)
  bag.fraction     = 0.8,           # usa 80% das obs por iteração (reduz overfitting)
  cv.folds         = 5,             # cross-validation para escolher n ótimo de árvores
  verbose          = FALSE
)

# ══════════════════════════════════════════════════════════════════════════════
# 3. VISUALIZAÇÃO DIDÁTICA: CURVA DE APRENDIZADO
# ══════════════════════════════════════════════════════════════════════════════
# Este gráfico é a visualização central do Gradient Boosting:
# mostra como o ERRO CAI a cada nova árvore adicionada.
#
# Eixo Y — Desvio de Bernoulli (log-loss):
#   Cada observação recebe uma probabilidade predita p^ entre 0 e 1.
#   O desvio mede o quão errada foi essa probabilidade usando o log:
#     -2 * [ y * log(p^) + (1-y) * log(1 - p^) ]
#   O log penaliza confiança errada severamente: prever 1% para quem infartou
#   gera desvio alto; prever 49% errado gera desvio pequeno.
#   O valor no gráfico é a média desse desvio sobre todas as observações.
#   Não tem interpretação direta em unidades naturais — importa a tendência.
#
# Duas linhas são exibidas:
#   • Treino (preto): erro no conjunto de treino — sempre cai com mais árvores
#   • CV (verde):     erro na cross-validation — cai até certo ponto e estabiliza
#
# O n ótimo de árvores é onde a curva de CV é mínima (linha vertical azul).
# Adicionar mais árvores além desse ponto = overfitting.

# gbm.perf() cria o gráfico internamente (base R), não retorna objeto ggplot.
# Usamos png()/dev.off() para capturar o gráfico antes de ele ser exibido.
png("output/gradient_boosting_curva_aprendizado.png", width = 900, height = 700, res = 120)
n_otimo <- gbm.perf(
  modelo_gbm,
  method  = "cv",
  plot.it = TRUE
)
title(
  main    = "Curva de Aprendizado",
  sub     = paste0("N ótimo de árvores (validação cruzada): ", n_otimo,
                   " | Taxa de aprendizado: 0.05 | Profundidade: 1"),
  cex.sub = 0.8, col.sub = "gray40"
)
# Cobre labels em inglês do gbm.perf() com branco, depois substitui em PTBR
mtext("Iteration",          side = 1, line = 3, col = "white", cex = 1.0)
mtext("Bernoulli deviance", side = 2, line = 3, col = "white", cex = 1.0, las = 0)
mtext("Número de árvores (iterações)", side = 1, line = 3)
mtext("Desvio de Bernoulli",           side = 2, line = 3, las = 0)
legend("topright",
  legend = c("Treino", "Validação cruzada"),
  col    = c("black", "green"),
  lwd    = 2, bty = "n"
)
dev.off()

# ══════════════════════════════════════════════════════════════════════════════
# 3.5 EVOLUÇÃO DAS PREDIÇÕES AO LONGO DAS ITERAÇÕES
# ══════════════════════════════════════════════════════════════════════════════
# O que muda a cada nova árvore adicionada?
# Usamos predict(..., n.trees = k) para obter a predição do modelo com apenas
# k árvores — e comparamos como a separação entre classes melhora.
#
# Cada painel mostra a distribuição das probabilidades preditas de infarto,
# separada por desfecho real (Sim / Não).
# • Com poucas árvores: distribuições sobrepostas — modelo confuso
# • Com mais árvores:   distribuições se separam — modelo aprende

# gbm() aceita fator nos preditores — não converter para character
teste_gbm <- dados_teste

iteracoes <- c(1, 10, 50, n_otimo)

evol_df <- purrr::map_dfr(iteracoes, function(k) {
  prob <- predict(modelo_gbm, newdata = teste_gbm, n.trees = k, type = "response")
  data.frame(
    prob    = prob,
    infarto = dados_teste$infarto,
    iter    = paste0("Árvores: ", k)
  )
})
evol_df$iter <- factor(evol_df$iter, levels = paste0("Árvores: ", iteracoes))

p_evol <- ggplot(evol_df, aes(x = prob, fill = infarto, color = infarto)) +
  geom_density(alpha = 0.4, linewidth = 0.8) +
  facet_wrap(~ iter, nrow = 2) +
  scale_fill_manual(values  = c("Sim" = "#d73027", "Yes" = "#d73027",
                                "Não" = "#2166ac", "No"  = "#2166ac")) +
  scale_color_manual(values = c("Sim" = "#d73027", "Yes" = "#d73027",
                                "Não" = "#2166ac", "No"  = "#2166ac")) +
  labs(
    title    = "Evolução das Predições",
    subtitle = "Distribuição de P(infarto) com k árvores acumuladas",
    x        = "P(infarto = Sim)",
    y        = "Densidade",
    fill     = "Infarto real", color = "Infarto real",
    caption  = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "top",
    plot.caption     = element_text(color = "gray40", size = 9),
    strip.text       = element_text(face = "bold")
  )
print(p_evol)
ggsave("output/gradient_boosting_evolucao_predicoes.png", plot = p_evol,
       width = 9, height = 7, dpi = 150, bg = "white")

# ══════════════════════════════════════════════════════════════════════════════
# 4. EFEITO DO LEARNING RATE (shrinkage)
# ══════════════════════════════════════════════════════════════════════════════
# Learning rate controla o quanto cada árvore contribui para a predição final.
# • Rate alto  (ex: 0.3): aprende rápido, precisa de menos árvores, mas mais grosseiro
# • Rate baixo (ex: 0.01): aprende devagar, precisa de muito mais árvores, mas mais fino
#
# Regra prática: use rate baixo + muitas árvores para melhor performance.
# Para didática, comparamos 3 rates com o mesmo n.trees:

rates     <- c(0.3, 0.05, 0.01)
cores_lr  <- c("#d73027", "#2166ac", "#4dac26")
rotulos   <- c("Rate alto (0.30)", "Rate médio (0.05)", "Rate baixo (0.01)")

# Coleta erros de treino para cada rate e monta um data.frame para ggplot
erros_lr <- purrr::map_dfr(seq_along(rates), function(i) {
  set.seed(42)
  m_tmp <- gbm(
    infarto_num ~ idade + sexo + hipertens + imc + colesterol + diabetes + fumante + sedentarismo,
    data              = treino_gbm,
    distribution      = "bernoulli",
    n.trees           = 300,
    interaction.depth = 1,
    shrinkage         = rates[i],
    bag.fraction      = 0.8,
    verbose           = FALSE
  )
  data.frame(
    arvore  = seq_along(m_tmp$train.error),
    erro    = m_tmp$train.error,
    rotulo  = rotulos[i]
  )
})
erros_lr$rotulo <- factor(erros_lr$rotulo, levels = rotulos)

p_lr <- ggplot(erros_lr, aes(x = arvore, y = erro, color = rotulo)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = setNames(cores_lr, rotulos)) +
  labs(
    title  = "Efeito da taxa de aprendizado no gradient boosting",
    x      = "Número de árvores",
    y      = "Erro (log-loss) no treino",
    color  = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")
print(p_lr)
ggsave("output/gradient_boosting_learning_rate.png", 
          plot = p_lr, 
          width = 8, 
          height = 6, 
          dpi = 150, 
          bg = "white"
)

# ══════════════════════════════════════════════════════════════════════════════
# 5. IMPLEMENTAÇÃO "DE PRODUÇÃO" VIA TIDYMODELS + XGBOOST
# ══════════════════════════════════════════════════════════════════════════════
# Agora usamos o tidymodels (padrão do projeto) com XGBoost como engine.
# XGBoost é Gradient Boosting com regularização L1/L2 e otimização de segunda
# ordem (hessiana) — mais robusto e rápido que gbm para dados grandes.

# 5.1 Recipe
# step_dummy: converte fatores em dummies (XGBoost aceita numérico)
# step_normalize: não obrigatório para GB (árvores são invariantes à escala),
#                 mas mantemos para consistência com os outros scripts
receita_xgb <- recipe(infarto ~ ., data = dados_treino) %>%
  step_dummy(all_nominal_predictors(), one_hot = FALSE) %>%
  step_normalize(all_numeric_predictors())

# 5.2 Especificação
# tree_depth = 1   → stump, consistente com a parte didática acima
# trees = n_otimo  → n ótimo encontrado via CV no gbm (mesmo shrinkage/learn_rate)
# learn_rate = 0.05
#
# Nota: com apenas 6 preditores (maioria binária), a regressão logística já
# captura quase todo o sinal disponível. O XGBoost com stumps acompanha esse
# resultado — o ganho pequeno sobre o baseline É o resultado correto para estes dados.
espec_xgb <- boost_tree(
  trees      = n_otimo,
  tree_depth = 1,
  learn_rate = 0.05
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# 5.3 Workflow
wf_xgb <- workflow() %>%
  add_recipe(receita_xgb) %>%
  add_model(espec_xgb)

# 5.4 Treinar
set.seed(42)
modelo_xgb <- wf_xgb %>% fit(data = dados_treino)

# 5.5 Avaliar no teste
# Acurácia é enganosa com classes desbalanceadas. Usamos ROC + AUC.
# Usamos ROC + AUC como métrica principal — igual ao script de rede neural.
#   AUC = 0.5 → modelo aleatório (inútil)
#   AUC = 1.0 → modelo perfeito
#   AUC > 0.7 → discriminação aceitável
probs_teste <- predict(modelo_xgb, dados_teste, type = "prob") %>%
  bind_cols(dados_teste %>% select(infarto))

col_evento <- paste0(".pred_", levels(dados_teste$infarto)[1])

probs_roc <- probs_teste %>%
  rename(prob_evento = all_of(col_evento))

auc_gb <- roc_auc(probs_roc, truth = infarto, prob_evento)
cat("AUC do Gradient Boosting:", round(auc_gb$.estimate, 3), "\n")

roc_df <- roc_curve(probs_roc, truth = infarto, prob_evento)
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
    label = sprintf("Limiar ótimo\n(Youden = %.2f)", max(roc_df$sensitivity + roc_df$specificity - 1, na.rm = TRUE)),
    hjust = 0, size = 3.2, color = "#d73027"
  ) +
  labs(
    title   = "Curva ROC — Gradient Boosting / XGBoost (NHANES 2021-2023)",
    x       = "1 - Especificidade  (taxa de falsos positivos)",
    y       = "Sensibilidade  (taxa de verdadeiros positivos)",
    caption = paste0(
      "AUC = ", round(auc_gb$.estimate, 3),
      "  |  Diagonal pontilhada = modelo aleatório (AUC = 0,5)",
      "  |  Ponto vermelho = limiar ótimo (Youden)"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(color = "gray40", size = 9))
print(p_roc)
ggsave("output/gradient_boosting_roc.png", plot = p_roc, width = 7, height = 6, dpi = 150, bg = "white")

# ══════════════════════════════════════════════════════════════════════════════
# 6. IMPORTÂNCIA DAS VARIÁVEIS
# ══════════════════════════════════════════════════════════════════════════════
# No Gradient Boosting, a importância é medida pelo GANHO TOTAL de cada variável:
# quanto de redução na loss ela provocou ao ser escolhida como split,
# acumulado ao longo de todas as árvores.
# Variáveis escolhidas com mais frequência E com maior redução de erro ficam no topo.

p_vip <- vip(
  modelo_xgb,
  num_features = 10,
  geom         = "col",
  aesthetics   = list(fill = "#2166ac", color = "white")
) +
  labs(
    title    = "Importância das Variáveis — Gradient Boosting (XGBoost)",
    subtitle = "Ganho total acumulado em todas as árvores",
    x        = NULL, y = "Importância"
  ) +
  theme_minimal(base_size = 13)
print(p_vip)
ggsave("output/gradient_boosting_importancia.png", plot = p_vip, width = 7, height = 5, dpi = 150, bg = "white")

# ══════════════════════════════════════════════════════════════════════════════
# 7. COMPARAÇÃO COM REGRESSÃO LOGÍSTICA (baseline)
# ══════════════════════════════════════════════════════════════════════════════
if (exists("auc_lr")) {
  cat(sprintf(
    "\nAUC  Regressão Logística : %.3f (baseline)\nAUC  Gradient Boosting    : %.3f\nGanho                     : %+.3f\n",
    auc_lr$.estimate, auc_gb$.estimate, auc_gb$.estimate - auc_lr$.estimate
  ))
} else {
  cat("\n[Comparação] Rode 01_logistic_regression.R antes para ver o ganho sobre o baseline.\n")
}

# ══════════════════════════════════════════════════════════════════════════════
# fim do script ────────────────────────────────────────────────────────────────
# ══════════════════════════════════════════════════════════════════════════════