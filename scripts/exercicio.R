# ══════════════════════════════════════════════════════════════════════════════
# Execução completa do exercício
# ══════════════════════════════════════════════════════════════════════════════
# EXERCÍCIO: Banco ampliado para predição de Hipertensão
#
# OBJETIVO: expandir o conjunto de preditores do fluxo principal (9 variáveis)
# para 21 variáveis, incorporando marcadores laboratoriais, comportamentais
# e sociais.
#
# AMOSTRA COMUM: o 00_config.R prepara os DOIS bancos sobre as mesmas
# observações (casos completos nas 21 variáveis) e o MESMO split: portanto
# qualquer diferença de AUC entre a Análise 1 e este exercício vem das 12
# variáveis extras, nunca da amostra.
#
#   Este script é autocontido: a seção 0 roda o 00_config.R (pacotes, dados
#   e os dois bancos), então não é preciso rodar nada antes. Ele sobrescreve
#   os objetos dados, dados_treino e dados_teste: após rodar, os scripts
#   01-04 usam o banco ampliado.
#   Única exigência: rodar a partir da raiz do projeto (como os demais).
#
# TAREFA DO ALUNO:
#   1. Rode este script: ele roda os 4 modelos no banco ampliado e termina
#      com a tabela de AUCs
#   2. Compare com os resultados da Análise 1 (mesma amostra: comparação direta)
#   3. Quais variáveis apareceram como mais importantes?
#   4. O ganho de AUC justifica a complexidade extra?
#   5. Qual modelo performa melhor? Segundo quais critérios?
# ══════════════════════════════════════════════════════════════════════════════

# ── 0. Configuração ───────────────────────────────────────────────────────────
# Mesmo motivo do rodar.R: em execução não-interativa (ex.: Rscript), os
# print() dos gráficos criariam um Rplots.pdf na raiz: desviar para o nulo
nao_interativo <- !interactive()
if (nao_interativo) pdf(NULL)

source("scripts/00_config.R")

# ── 1. Trocar para o banco ampliado (mesmas linhas, 21 preditores) ────────────
# Guarda o banco da Análise 1 (9 preditores): usado na tabela comparativa final
dados_base        <- dados
dados_treino_base <- dados_treino
dados_teste_base  <- dados_teste

dados        <- dados_ampliado
dados_treino <- dados_treino_ampliado
dados_teste  <- dados_teste_ampliado

cat(sprintf(
  "Banco ampliado: %d obs × %d preditores (treino: %d | teste: %d) | mesma amostra da Análise 1\n\n",
  nrow(dados), ncol(dados) - 1, nrow(dados_treino), nrow(dados_teste)
))

# ── 2. Tabela descritiva das variáveis novas ──────────────────────────────────
# Salva em output/exercicio/ (mesmo padrão da tabela da Análise 1)
dir.create("output/exercicio", showWarnings = FALSE)

tabela_desc_ampliada <- nhanes_ext %>%
  select(hba1c, hdl, pcr, ac_urico, creatinina,
         alcool, sono_horas, saude_geral,
         escolaridade, est_civil, imc, freq_cardiaca) %>%
  tbl_summary(
    missing = "ifany",
    missing_text = "Perda",
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    label = list(
      hba1c         ~ "HbA1c (%)",
      hdl           ~ "HDL (mg/dL)",
      pcr           ~ "PCR alta sensibilidade (mg/L)",
      ac_urico      ~ "Ácido úrico (mg/dL)",
      creatinina    ~ "Creatinina sérica (mg/dL)",
      alcool        ~ "Álcool (doses/dia)",
      sono_horas    ~ "Horas de sono/noite",
      saude_geral   ~ "Saúde geral autorrelatada",
      escolaridade  ~ "Escolaridade",
      est_civil     ~ "Estado civil",
      imc           ~ "IMC (kg/m²)",
      freq_cardiaca ~ "Frequência cardíaca de repouso (bpm)"
    )
  )

tabela_desc_ampliada %>%
  as_gt() %>%
  gt::gtsave("output/exercicio/tabela_descritiva_ampliada.html")

# PNG via flextable (gráficos nativos do R, sem Chrome): mesmo método do 00_config.R
ft_ampliada <- tabela_desc_ampliada %>%
  as_flex_table() %>%
  flextable::autofit()

dm <- dim(ft_ampliada)

png("output/exercicio/tabela_descritiva_ampliada.png",
    width = sum(dm$widths) * 300, height = sum(dm$heights) * 300, res = 300)
grid::grid.newpage()
grid::grid.draw(flextable::gen_grob(ft_ampliada, fit = "auto"))
dev.off()

# ══════════════════════════════════════════════════════════════════════════════
# 3. MODELOS: versões condensadas dos scripts 01-04
# ══════════════════════════════════════════════════════════════════════════════
# Mesmos 4 modelos da Análise 1, sem os gráficos de conceito (que já foram
# vistos): aqui importa só a comparação: AUC, ROC e importância das variáveis.
# Tudo é salvo em output/exercicio/: os resultados da Análise 1 ficam intactos.

# ── Funções auxiliares do bloco de avaliação (usadas pelos 4 modelos) ───────
# pred_prob: probabilidade do evento (has_pas = "Sim")
pred_prob <- function(object, newdata) {
  predict(object, newdata, type = "prob")$.pred_Sim
}

# AUC com evento = "Sim" (2º nível do fator): mesma convenção dos scripts 01-04
auc_evento_sim <- function(truth, estimate) {
  roc_auc_vec(truth, estimate, event_level = "second")
}

# Rótulos PT-BR para os 21 preditores (gráficos de importância)
rotulos <- c(
  "idade" = "Idade", "sexo" = "Sexo", "raca" = "Raça/Etnia",
  "whr" = "Razão cintura/estatura", "renda" = "Renda",
  "colesterol" = "Colesterol", "diabetes" = "Diabetes", "fumante" = "Tabagismo",
  "sedentarismo" = "Sedentarismo (min/dia)",
  "escolaridade" = "Escolaridade", "est_civil" = "Estado civil",
  "imc" = "IMC", "freq_cardiaca" = "Freq. cardíaca (bpm)",
  "hba1c" = "HbA1c", "hdl" = "HDL", "pcr" = "PCR",
  "ac_urico" = "Ácido úrico", "creatinina" = "Creatinina",
  "alcool" = "Álcool (doses/dia)", "sono_horas" = "Sono (h/noite)",
  "saude_geral" = "Saúde autorrelatada"
)

# avalia(): AUC + curva ROC + importância por permutação de um modelo ajustado.
# É a condensação dos blocos de avaliação dos scripts 01-04: repare que o
# MESMO código serve para os 4 modelos: essa uniformidade é o tidymodels.
avalia <- function(modelo, nome, arquivo) {
  # AUC + ROC no conjunto de teste
  probs <- predict(modelo, dados_teste, type = "prob") %>%
    bind_cols(dados_teste %>% select(has_pas)) %>%
    rename(prob_evento = .pred_Sim)
  auc <- roc_auc(probs, truth = has_pas, prob_evento, event_level = "second")
  cat(sprintf("AUC (%s): %.3f\n", nome, auc$.estimate))

  # Curva ROC no mesmo padrão visual dos scripts 01-04:
  # ponto de Youden em vermelho + AUC anotado dentro da figura
  roc_df <- roc_curve(probs, truth = has_pas, prob_evento, event_level = "second")
  ponto_otimo <- roc_df %>%
    mutate(youden = sensitivity + specificity - 1) %>%
    slice_max(youden, n = 1)

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
      label = paste0("AUC = ", fmt_br(auc$.estimate)),
      size = 4.5, fontface = "bold", color = "#2166ac", hjust = 0
    ) +
    labs(
      title    = paste0("Curva ROC: ", nome),
      subtitle = "Banco ampliado (21 preditores): NHANES 2021-2023",
      x        = "1 − Especificidade  (taxa de falsos positivos)",
      y        = "Sensibilidade  (taxa de verdadeiros positivos)",
      caption  = "Diagonal pontilhada = modelo aleatório (AUC = 0,5)  |  Ponto vermelho = limiar ótimo (Youden)"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.caption = element_text(color = "gray40", size = 9))
  ggsave(file.path("output/exercicio", paste0(arquivo, "_roc.png")), p_roc,
         width = 7, height = 6, dpi = 150, bg = "white")

  # Importância por permutação: queda de AUC ao embaralhar cada variável
  set.seed(42)
  imp <- vip::vi_permute(
    modelo,
    train             = dados_teste,
    target            = "has_pas",
    metric            = auc_evento_sim,
    smaller_is_better = FALSE,
    pred_wrapper      = pred_prob,
    nsim              = 10
  ) %>%
    mutate(Variable = dplyr::recode(Variable, !!!rotulos))

  p_imp <- ggplot(imp, aes(x = Importance, y = reorder(Variable, Importance))) +
    geom_col(fill = "#4dac26", color = "white") +
    labs(title = paste0("Importância por permutação: ", nome),
         x = "Queda de AUC", y = NULL) +
    theme_minimal(base_size = 12)
  ggsave(file.path("output/exercicio", paste0(arquivo, "_importancia.png")), p_imp,
         width = 7, height = 6, dpi = 150, bg = "white")

  invisible(auc$.estimate)
}

# Receita única (mesma dos scripts 01-04): dummies + normalização
receita <- recipe(has_pas ~ ., data = dados_treino) %>%
  step_dummy(all_nominal_predictors(), one_hot = FALSE) %>%
  step_normalize(all_numeric_predictors())

# ── 6.1 Regressão Logística ──────────────────────────────────────────────────
modelo_lr <- workflow() %>%
  add_recipe(receita) %>%
  add_model(logistic_reg() %>% set_engine("glm") %>% set_mode("classification")) %>%
  fit(data = dados_treino)
auc_lr_ex <- avalia(modelo_lr, "Regressão Logística", "logistica")

# ── 6.2 Random Forest ────────────────────────────────────────────────────────
set.seed(42)  # o bootstrap do RF é aleatório
modelo_rf <- rand_forest(trees = 500, mtry = floor(sqrt(ncol(dados_treino) - 1))) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification") %>%
  fit(has_pas ~ ., data = dados_treino)
auc_rf_ex <- avalia(modelo_rf, "Random Forest", "random_forest")

# ── 6.3 Gradient Boosting (XGBoost) ──────────────────────────────────────────
# No script 03, o nº de árvores vinha da validação cruzada do gbm; aqui
# fixamos 300 stumps para manter o exercício enxuto
set.seed(42)
modelo_xgb <- workflow() %>%
  add_recipe(receita) %>%
  add_model(boost_tree(trees = 300, tree_depth = 1, learn_rate = 0.05) %>%
              set_engine("xgboost") %>% set_mode("classification")) %>%
  fit(data = dados_treino)
auc_gb_ex <- avalia(modelo_xgb, "Gradient Boosting (XGBoost)", "gradient_boosting")

# ── 6.4 Rede Neural (MLP) ────────────────────────────────────────────────────
set.seed(42)
modelo_nn <- workflow() %>%
  add_recipe(receita) %>%
  add_model(mlp(hidden_units = 4, penalty = 0.01, epochs = 300) %>%
              set_engine("nnet") %>% set_mode("classification")) %>%
  fit(data = dados_treino)
auc_nn_ex <- avalia(modelo_nn, "Rede Neural (MLP)", "rede_neural")

# ── 4. Baseline da Análise 1 na MESMA amostra ────────────────────────────────
# Para a tabela comparativa final, reajustamos os 4 modelos com os 9 preditores
# originais: mesmas linhas, mesmo split. Assim o Δ de AUC é atribuível
# exclusivamente às 12 variáveis extras, e o exercício fica autocontido
# (não depende de ter rodado os scripts 01-04 antes).
receita_base <- recipe(has_pas ~ ., data = dados_treino_base) %>%
  step_dummy(all_nominal_predictors(), one_hot = FALSE) %>%
  step_normalize(all_numeric_predictors())

set.seed(42)
mod_lr_base <- workflow() %>%
  add_recipe(receita_base) %>%
  add_model(logistic_reg() %>% set_engine("glm") %>% set_mode("classification")) %>%
  fit(data = dados_treino_base)

set.seed(42)
mod_rf_base <- rand_forest(trees = 500, mtry = floor(sqrt(ncol(dados_treino_base) - 1))) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification") %>%
  fit(has_pas ~ ., data = dados_treino_base)

set.seed(42)
mod_xgb_base <- workflow() %>%
  add_recipe(receita_base) %>%
  add_model(boost_tree(trees = 300, tree_depth = 1, learn_rate = 0.05) %>%
              set_engine("xgboost") %>% set_mode("classification")) %>%
  fit(data = dados_treino_base)

set.seed(42)
mod_nn_base <- workflow() %>%
  add_recipe(receita_base) %>%
  add_model(mlp(hidden_units = 4, penalty = 0.01, epochs = 300) %>%
              set_engine("nnet") %>% set_mode("classification")) %>%
  fit(data = dados_treino_base)

auc_base <- c(
  auc_evento_sim(dados_teste_base$has_pas, pred_prob(mod_lr_base,  dados_teste_base)),
  auc_evento_sim(dados_teste_base$has_pas, pred_prob(mod_rf_base,  dados_teste_base)),
  auc_evento_sim(dados_teste_base$has_pas, pred_prob(mod_xgb_base, dados_teste_base)),
  auc_evento_sim(dados_teste_base$has_pas, pred_prob(mod_nn_base,  dados_teste_base))
)

# ── 5. Tabela comparativa final ──────────────────────────────────────────────
# Análise 1 × Exercício na mesma amostra: o Δ é só das 12 variáveis extras.
# A versão formatada (HTML/PNG) fica commitada no repositório para consulta
# de quem não conseguir rodar o pipeline.
cat("\n══ Resumo: Análise 1 × Exercício (mesma amostra) ═══════════════════════\n")
resumo_auc <- data.frame(
  Modelo = c("Regressão Logística", "Random Forest",
             "Gradient Boosting (XGBoost)", "Rede Neural (MLP)"),
  AUC_Analise1  = auc_base,
  AUC_Exercicio = c(auc_lr_ex, auc_rf_ex, auc_gb_ex, auc_nn_ex)
) %>%
  mutate(Ganho = AUC_Exercicio - AUC_Analise1) %>%
  arrange(desc(AUC_Exercicio))

print(resumo_auc)

tab_comp <- resumo_auc %>%
  gt::gt() %>%
  gt::tab_header(
    title    = "Predição de hipertensão (PAS ≥ 140 mmHg): Análise 1 × Exercício",
    subtitle = sprintf(
      "AUC no conjunto de teste (n = %s)  |  mesma amostra (N = %s) e mesmo split  |  NHANES 2021-2023",
      format(nrow(dados_teste), big.mark = ".", decimal.mark = ","),
      format(nrow(dados),       big.mark = ".", decimal.mark = ",")
    )
  ) %>%
  gt::cols_label(
    Modelo        = "Modelo",
    AUC_Analise1  = "Análise 1 (9 preditores)",
    AUC_Exercicio = "Exercício (21 preditores)",
    Ganho         = "Ganho (Δ AUC)"
  ) %>%
  gt::fmt_number(columns = c(AUC_Analise1, AUC_Exercicio), decimals = 3, dec_mark = ",", sep_mark = ".") %>%
  gt::fmt_number(columns = Ganho, decimals = 3, dec_mark = ",", sep_mark = ".", force_sign = TRUE)

tab_comp %>% gt::gtsave("output/exercicio/comparacao_modelos.html")

# PNG da tabela comparativa via flextable (mesmo método das demais tabelas):
# gráficos nativos do R, sem depender de navegador
tab_comp_ft <- resumo_auc %>%
  mutate(
    AUC_Analise1  = fmt_br(AUC_Analise1),
    AUC_Exercicio = fmt_br(AUC_Exercicio),
    Ganho         = sub(".", ",", sprintf("%+.3f", Ganho), fixed = TRUE)
  ) %>%
  flextable::flextable() %>%
  flextable::set_header_labels(
    Modelo        = "Modelo",
    AUC_Analise1  = "Análise 1 (9 preditores)",
    AUC_Exercicio = "Exercício (21 preditores)",
    Ganho         = "Ganho (Δ AUC)"
  ) %>%
  flextable::add_header_lines(values = c(
    "Predição de hipertensão (PAS ≥ 140 mmHg): Análise 1 × Exercício",
    sprintf(
      "AUC no conjunto de teste (n = %s) | mesma amostra (N = %s) e mesmo split | NHANES 2021-2023",
      format(nrow(dados_teste), big.mark = ".", decimal.mark = ","),
      format(nrow(dados),       big.mark = ".", decimal.mark = ",")
    )
  )) %>%
  flextable::bold(i = 1, part = "header") %>%
  flextable::autofit()

dm <- dim(tab_comp_ft)

png("output/exercicio/comparacao_modelos.png",
    width = sum(dm$widths) * 300, height = sum(dm$heights) * 300, res = 300)
grid::grid.newpage()
grid::grid.draw(flextable::gen_grob(tab_comp_ft, fit = "auto"))
dev.off()

cat("\nGráficos em output/exercicio/  |  Tabela comparativa: output/exercicio/comparacao_modelos.html\n")

if (nao_interativo) dev.off()  # fecha o dispositivo nulo aberto no início

# ══════════════════════════════════════════════════════════════════════════════
# fim do script ────────────────────────────────────────────────────────────────
# ══════════════════════════════════════════════════════════════════════════════
