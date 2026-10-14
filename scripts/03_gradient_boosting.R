
# ══════════════════════════════════════════════════════════════════════════════
# NHANES 2021-2023: Gradient Boosting para Predição de HAS (foco didático)
# Rode 00_config.R antes (ele cria dados, dados_treino e dados_teste)
# ══════════════════════════════════════════════════════════════════════════════

# ── 0. Pacotes ────────────────────────────────────────────────────────────────
# Pacotes carregados em 00_config.R

# ── 1. Dados ─────────────────────────────────────────────────────────────────

# ══════════════════════════════════════════════════════════════════════════════
# 2. CONCEITO CENTRAL: O "APRENDIZ FRACO" (STUMP, PROFUNDIDADE 1)
# ══════════════════════════════════════════════════════════════════════════════
# Um stump é uma árvore com apenas 1 split: divide os dados em dois grupos
# usando UMA variável e UM ponto de corte.
# Sozinho, é um preditor muito fraco: acerta pouco mais que o acaso.
# O Gradient Boosting usa centenas desses stumps em sequência,
# cada um corrigindo os erros do anterior. A força vem da soma, não do indivíduo.

# Usamos gbm() diretamente para tornar o processo iterativo VISÍVEL:
#   interaction.depth = 1  → stump (o aprendiz fraco clássico)
#   n.trees            → número de iterações (quantos stumps serão somados)
#   shrinkage          → learning rate: quanto cada stump "pesa" na soma final
#   cv.folds           → cross-validation interna para detectar o n ótimo de árvores

# gbm() exige desfecho numérico (0/1) para distribuição "bernoulli"
# Mantemos os preditores como fator: gbm aceita fator mas não character
treino_gbm <- dados_treino %>%
  mutate(
    has_pas_num = as.integer(has_pas == "Sim")
  )

# ── 2.1 Visualizar aprendiz fraco ─────────────────────────────────
stump <- rpart::rpart(
  has_pas ~ .,
  data    = dados_treino,
  control = rpart::rpart.control(maxdepth = 1, minsplit = 1, minbucket = 1, cp = -1)
)
png("output/03_gradient_boosting/gradient_boosting_stump.png", width = 800, height = 600, res = 120)
rpart.plot(
  stump,
  type          = 2,
  extra         = 104,
  fallen.leaves = TRUE,
  box.palette   = "BuGn",
  roundint      = FALSE,
  main          = "Aprendiz fraco (stump): árvore com um único split"
)
dev.off()

set.seed(42)

# Modelo GBM completo: 500 stumps encadeados em sequência (interaction.depth = 1).
# Cada árvore é um stump, mas o ensemble das 500 é o gradient boosting em si.
# ". - has_pas" = todos os preditores do banco, exceto o desfecho fator
# (assim o exercicio.R usa automaticamente as variáveis novas)
modelo_gbm <- gbm(
  has_pas_num ~ . - has_pas,
  data             = treino_gbm,
  distribution     = "bernoulli",   # log-loss: adequada para classificação binária
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
# Eixo Y: Desvio de Bernoulli (log-loss):
#   Cada observação recebe uma probabilidade predita p^ entre 0 e 1.
#   O desvio mede o quão errada foi essa probabilidade usando o log:
#     -2 * [ y * log(p^) + (1-y) * log(1 - p^) ]
#   O log penaliza confiança errada severamente: prever 1% para quem de fato
#   tem HAS gera desvio alto; prever 49% errado gera desvio pequeno.
#   O valor no gráfico é a média desse desvio sobre todas as observações.
#   Não tem interpretação direta em unidades naturais: importa a tendência.
#
# Duas linhas são exibidas:
#   • Treino (preto): erro no conjunto de treino: sempre cai com mais árvores
#   • CV (verde):     erro na cross-validation: cai até certo ponto e estabiliza
#
# O n ótimo de árvores é onde a curva de CV é mínima (linha vertical azul).
# Adicionar mais árvores além desse ponto = overfitting.

# gbm.perf() retorna o n ótimo de árvores segundo a validação cruzada interna
n_otimo <- gbm.perf(modelo_gbm, method = "cv", plot.it = FALSE)

# Em vez do gráfico pronto do gbm (labels em inglês), extraímos os erros
# e montamos o ggplot: mesmo padrão visual dos demais gráficos do projeto
curva_long <- data.frame(
  arvores = rep(seq_along(modelo_gbm$cv.error), 2),
  erro    = c(modelo_gbm$train.error, modelo_gbm$cv.error),
  serie   = rep(c("Treino", "Validação cruzada"), each = length(modelo_gbm$cv.error))
)

p_curva <- ggplot(curva_long, aes(x = arvores, y = erro, color = serie)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = n_otimo, linetype = "dashed", color = "#2166ac") +
  annotate(
    "text", x = n_otimo, y = max(curva_long$erro),
    label = paste0("n ótimo = ", n_otimo, " "),
    hjust = 1.05, size = 3.5, color = "#2166ac"
  ) +
  scale_color_manual(values = c("Treino" = "black", "Validação cruzada" = "#4dac26")) +
  labs(
    title    = "Curva de Aprendizado: Gradient Boosting",
    subtitle = paste0("Taxa de aprendizado: 0.05  |  Profundidade: 1 (stumps)  |  ",
                      "n ótimo de árvores por validação cruzada: ", n_otimo),
    x        = "Número de árvores (iterações)",
    y        = "Desvio de Bernoulli (log-loss)",
    color    = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")
print(p_curva)
ggsave("output/03_gradient_boosting/gradient_boosting_curva_aprendizado.png", plot = p_curva,
       width = 8, height = 6, dpi = 150, bg = "white")

# ══════════════════════════════════════════════════════════════════════════════
# 3.5 EVOLUÇÃO DAS PREDIÇÕES AO LONGO DAS ITERAÇÕES
# ══════════════════════════════════════════════════════════════════════════════
# O que muda a cada nova árvore adicionada?
# Usamos predict(..., n.trees = k) para obter a predição do modelo com apenas
# k árvores: e comparamos como a separação entre classes melhora.
#
# Cada painel mostra a distribuição das probabilidades preditas de HAS,
# separada por desfecho real (Sim / Não).
# • Com poucas árvores: distribuições sobrepostas: modelo confuso
# • Com mais árvores:   distribuições se separam: modelo aprende

# gbm() aceita fator nos preditores: não converter para character
teste_gbm <- dados_teste

# sort(unique()) evita duplicatas se n_otimo for 1, 10 ou 50
iteracoes <- sort(unique(c(1, 10, 50, n_otimo)))

evol_df <- purrr::map_dfr(iteracoes, function(k) {
  prob <- predict(modelo_gbm, newdata = teste_gbm, n.trees = k, type = "response")
  data.frame(
    prob    = prob,
    has_pas = dados_teste$has_pas,
    # factor com levels ordenados: sem isso, "Árvores: 250" viria antes de "Árvores: 50"
    iter    = factor(paste0("Árvores: ", k), levels = paste0("Árvores: ", iteracoes))
  )
})
evol_df$iter <- factor(evol_df$iter, levels = paste0("Árvores: ", iteracoes))

p_evol <- ggplot(evol_df, aes(x = prob, fill = has_pas, color = has_pas)) +
  geom_density(alpha = 0.4, linewidth = 0.8) +
  facet_wrap(~ iter, nrow = 2) +
  scale_fill_manual(values  = c("Sim" = "#d73027", "Não" = "#2166ac")) +
  scale_color_manual(values = c("Sim" = "#d73027", "Não" = "#2166ac")) +
  labs(
    title    = "Evolução das Predições",
    subtitle = "Distribuição de P(Hipertensão) com k árvores acumuladas",
    x        = "P(Hipertensão = Sim)",
    y        = "Densidade",
    fill     = "Hipertensão real", color = "Hipertensão real"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "top",
    plot.caption     = element_text(color = "gray40", size = 9),
    strip.text       = element_text(face = "bold")
  )
print(p_evol)
ggsave("output/03_gradient_boosting/gradient_boosting_evolucao_predicoes.png", plot = p_evol,
       width = 9, height = 7, dpi = 150, bg = "white")

# ══════════════════════════════════════════════════════════════════════════════
# 4. EFEITO DA TAXA DE APRENDIZADO
# ══════════════════════════════════════════════════════════════════════════════
# Learning rate controla o quanto cada árvore contribui para a predição final.
# • Taxa alta  (ex: 0.3): aprende rápido, precisa de menos árvores, mas mais grosseiro
# • Taxa baixa (ex: 0.01): aprende devagar, precisa de muito mais árvores, mas mais fino
#
# Regra prática: use taxa baixa + muitas árvores para melhor performance.
# Para didática, comparamos 3 taxas com o mesmo n.trees:

rates     <- c(0.3, 0.05, 0.01)
cores_lr  <- c("#d73027", "#2166ac", "#4dac26")
rotulos   <- c("Taxa alta (0.30)", "Taxa média (0.05)", "Taxa baixa (0.01)")

# Coleta erros de cross-validation para cada rate e monta um data.frame para ggplot
erros_lr <- purrr::map_dfr(seq_along(rates), function(i) {
  set.seed(42)
  m_tmp <- gbm(
    has_pas_num ~ . - has_pas,
    data              = treino_gbm,
    distribution      = "bernoulli",
    n.trees           = 300,
    interaction.depth = 1,
    shrinkage         = rates[i],
    bag.fraction      = 0.8,
    cv.folds          = 5,
    verbose           = FALSE
  )
  data.frame(
    arvore  = seq_along(m_tmp$cv.error),
    erro    = m_tmp$cv.error,
    rotulo  = rotulos[i]
  )
})
erros_lr$rotulo <- factor(erros_lr$rotulo, levels = rotulos)

p_lr <- ggplot(erros_lr, aes(x = arvore, y = erro, color = rotulo)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = setNames(cores_lr, rotulos)) +
  labs(
    title  = "Efeito da taxa de aprendizado",
    x      = "Número de árvores",
    y      = "Erro (log-loss): validação cruzada (5-fold)",
    color  = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")
print(p_lr)
ggsave("output/03_gradient_boosting/gradient_boosting_learning_rate.png", plot = p_lr,
       width = 8, height = 6, dpi = 150, bg = "white")

# ══════════════════════════════════════════════════════════════════════════════
# 5. IMPLEMENTAÇÃO "DE PRODUÇÃO" VIA TIDYMODELS + XGBOOST
# ══════════════════════════════════════════════════════════════════════════════
# Agora usamos o tidymodels (padrão do projeto) com XGBoost como engine.
# XGBoost é Gradient Boosting com regularização L1/L2 e otimização de segunda
# ordem (hessiana): mais robusto e rápido que gbm para dados grandes.

# 5.1 Recipe
# step_dummy: converte fatores em dummies (XGBoost aceita numérico)
# step_normalize: não obrigatório para GB (árvores são invariantes à escala),
#                 mas mantemos para consistência com os outros scripts
receita_xgb <- recipe(has_pas ~ ., data = dados_treino) %>%
  step_dummy(all_nominal_predictors(), one_hot = FALSE) %>%
  step_normalize(all_numeric_predictors())

# 5.2 Especificação
# tree_depth = 1   → stump, consistente com a parte didática acima
# trees = n_otimo  → n ótimo encontrado via CV no gbm (mesmo shrinkage/learn_rate)
# learn_rate = 0.05
#
# Nota: com apenas 9 preditores (vários binários), a regressão logística já
# captura quase todo o sinal disponível. O XGBoost com stumps acompanha esse
# resultado: o ganho pequeno sobre o baseline É o resultado correto para estes dados.
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
# Acurácia é enganosa com classes desbalanceadas. Usamos ROC + AUC como
# métrica principal: igual aos demais scripts (bloco idêntico, repetição
# proposital para manter cada script autocontido).
#   AUC = 0.5 → modelo aleatório (inútil)
#   AUC = 1.0 → modelo perfeito
#   AUC > 0.7 → discriminação aceitável
probs_teste <- predict(modelo_xgb, dados_teste, type = "prob") %>%
  bind_cols(dados_teste %>% select(has_pas))

probs_roc <- probs_teste %>%
  rename(prob_evento = .pred_Sim)   # probabilidade do evento: HAS = "Sim"

auc_gb <- roc_auc(probs_roc, truth = has_pas, prob_evento, event_level = "second")
cat("AUC do Gradient Boosting:", round(auc_gb$.estimate, 3), "\n")

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
    label = paste0("AUC = ", fmt_br(auc_gb$.estimate)),
    size = 4.5, fontface = "bold", color = "#2166ac", hjust = 0
  ) +
  labs(
    title    = "Curva ROC: Gradient Boosting (XGBoost)",
    subtitle = "Análise 1 (9 preditores): NHANES 2021-2023",
    x        = "1 − Especificidade  (taxa de falsos positivos)",
    y        = "Sensibilidade  (taxa de verdadeiros positivos)",
    caption  = "Diagonal pontilhada = modelo aleatório (AUC = 0,5)  |  Ponto vermelho = limiar ótimo (Youden)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(color = "gray40", size = 9))
print(p_roc)
ggsave("output/03_gradient_boosting/xgboost_roc.png", plot = p_roc, width = 7, height = 6, dpi = 150, bg = "white")

# ══════════════════════════════════════════════════════════════════════════════
# 6. IMPORTÂNCIA DAS VARIÁVEIS
# ══════════════════════════════════════════════════════════════════════════════
# No Gradient Boosting, a importância é medida pelo GANHO TOTAL de cada variável:
# quanto de redução na loss ela provocou ao ser escolhida como split,
# acumulado ao longo de todas as árvores.
# Variáveis escolhidas com mais frequência E com maior redução de erro ficam no topo.

rotulos_xgb <- c(
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
)

imp_xgb <- vi(modelo_xgb) %>%
  mutate(Variable = dplyr::recode(Variable, !!!rotulos_xgb))

p_vip <- ggplot(imp_xgb, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(fill = "#2166ac", color = "white") +
  labs(
    title    = "Importância das Variáveis: XGBoost",
    subtitle = "Proporção do ganho acumulado em todas as árvores",
    x        = "Importância", y = NULL
  ) +
  theme_minimal(base_size = 13)
print(p_vip)
ggsave("output/03_gradient_boosting/xgboost_importancia.png", plot = p_vip, width = 7, height = 5, dpi = 150, bg = "white")

# ══════════════════════════════════════════════════════════════════════════════
# 7. IMPORTÂNCIA POR PERMUTAÇÃO: MÉTODO AGNÓSTICO DE MODELO
# ══════════════════════════════════════════════════════════════════════════════
# A importância acima (ganho) ensina a MECÂNICA interna do boosting.
# Esta aqui é um EXPERIMENTO: embaralhamos UMA variável por vez nos dados de
# teste e medimos quanto o AUC cai. Queda grande = o modelo depende dela.
# Por ser agnóstica de modelo, é a única métrica de importância comparável
# ENTRE os scripts 02-04 (Gini ≠ ganho ≠ Olden).
# NOTA: a permutação atua nas variáveis BRUTAS (sexo, raca...), não nas
# dummies da receita: por isso o mapa de rótulos aqui usa os nomes brutos.

pred_prob <- function(object, newdata) {
  predict(object, newdata, type = "prob")$.pred_Sim
}

auc_evento_sim <- function(truth, estimate) {
  roc_auc_vec(truth, estimate, event_level = "second")
}

rotulos_perm <- c(
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

set.seed(42)
imp_perm <- vip::vi_permute(
  modelo_xgb,
  train        = dados_teste,
  target       = "has_pas",
  metric       = auc_evento_sim,
  smaller_is_better = FALSE,    # AUC maior = melhor (obrigatório p/ métrica customizada)
  pred_wrapper = pred_prob,
  nsim         = 10
) %>%
  mutate(Variable = dplyr::recode(Variable, !!!rotulos_perm))  # sem tradução = nome bruto (exercicio.R)

p_perm <- ggplot(imp_perm, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(fill = "#4dac26", color = "white") +
  labs(
    title    = "Importância por permutação: XGBoost",
    subtitle = "Queda média de AUC ao embaralhar cada variável\n(10 repetições, dados de teste)",
    x        = "Queda de AUC", y = NULL
  ) +
  theme_minimal(base_size = 13)
print(p_perm)
ggsave("output/03_gradient_boosting/xgboost_importancia_permutacao.png", plot = p_perm,
       width = 7, height = 5, dpi = 150, bg = "white")

# ══════════════════════════════════════════════════════════════════════════════
# 8. COMPARAÇÃO COM REGRESSÃO LOGÍSTICA (baseline)
# ══════════════════════════════════════════════════════════════════════════════
if (exists("auc_lr")) {
  cat(sprintf(
    "\nAUC  Regressão Logística : %.3f (baseline)\nAUC  Gradient Boosting    : %.3f\nGanho                     : %+.3f\n",
    auc_lr$.estimate, auc_gb$.estimate, auc_gb$.estimate - auc_lr$.estimate
  ))
} else {
  cat("\n[Comparação] Rode 01_regressao.R antes para ver o ganho sobre o baseline.\n")
}

# ══════════════════════════════════════════════════════════════════════════════
# fim do script ────────────────────────────────────────────────────────────────
# ══════════════════════════════════════════════════════════════════════════════