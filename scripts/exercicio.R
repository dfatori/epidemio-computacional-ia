# ══════════════════════════════════════════════════════════════════════════════
# Execução completa do exercício
# ══════════════════════════════════════════════════════════════════════════════
# EXERCÍCIO: Banco de dados ampliado para predição de Hipertensão
#
# OBJETIVO: Expandir o conjunto de preditores do fluxo principal (9 variáveis)
# para 22 variáveis, baixando tabelas adicionais do NHANES e incorporando
# marcadores laboratoriais, comportamentais e sociais.
#
#   Este script é autocontido: a seção 0 roda o 00_config.R
#   (pacotes, função de cache e banco de dados), então não é preciso rodar
#   nada antes. Ele sobrescreve os objetos dados, dados_treino e
#   dados_teste no ambiente — após rodar, os scripts 01–04 usarão
#   automaticamente o conjunto escolhido.
#   Única exigência: rodar a partir da raiz do projeto (como os demais).
#
# COMPARABILIDADE: a referência são os resultados da Análise 1, já salvos
# em output/ no repositório — não é preciso rodar o pipeline baseline.
# O banco abaixo usa casos completos nas 22 variáveis (N = 3.944).
#
# TAREFA DO ALUNO:
#   1. Rode este script — ele prepara o banco ampliado E roda os 4 modelos
#      (01–04) automaticamente, terminando com a tabela de AUCs
#   2. Compare com os resultados da Análise 1 (já salvos no repositório)
#   3. Quais variáveis apareceram como mais importantes?
#   4. O ganho de AUC justifica a complexidade extra?
#   5. Qual modelo performa melhor? Segundo quais critérios?
# ══════════════════════════════════════════════════════════════════════════════

# ── 0. Configuração  ──────────────────────────────────────────────────────────
source("scripts/00_config.R")

# ── 1. Baixar tabelas adicionais ──────────────────────────────────────────────
# Mesma função de cache definida em 00_config.R
# NOTA: TRIGLY_L (triglicerídeos) ficou de fora DE PROPÓSITO — 61% de valores
# faltantes (exame só existe no subgrupo de jejum do NHANES) custaria quase
# metade da amostra (3.944 → 2.187 obs). Primeira decisão de qualquer análise:
# saber quando uma variável não vale o custo.
ghb_L    <- nhanes_cache("GHB_L",    "data/ghb_L.rds")    # HbA1c
hdl_L    <- nhanes_cache("HDL_L",    "data/hdl_L.rds")    # HDL colesterol
hscrp_L  <- nhanes_cache("HSCRP_L",  "data/hscrp_L.rds")  # PCR alta sensibilidade
biopro_L <- nhanes_cache("BIOPRO_L", "data/biopro_L.rds") # Bioquímica (ác. úrico, creatinina)
alq_L    <- nhanes_cache("ALQ_L",    "data/alq_L.rds")    # Álcool
slq_L    <- nhanes_cache("SLQ_L",    "data/slq_L.rds")    # Sono
huq_L    <- nhanes_cache("HUQ_L",    "data/huq_L.rds")    # Saúde autorrelatada
dpq_L    <- nhanes_cache("DPQ_L",    "data/dpq_L.rds")    # Depressão (PHQ-9)

# ── 2. Junta tabelas NHANES ─────────────────────────────────────────────────
nhanes_ext <- nhanes_cardio %>%
  left_join(
    ghb_L    %>% select(SEQN, hba1c        = LBXGH),     # HbA1c (%)
    by = "SEQN") %>%
  left_join(
    hdl_L    %>% select(SEQN, hdl           = LBDHDD),   # HDL (mg/dL)
    by = "SEQN") %>%
  left_join(
    hscrp_L  %>% select(SEQN, pcr           = LBXHSCRP), # PCR alta sensib. (mg/L)
    by = "SEQN") %>%
  left_join(
    biopro_L %>% select(SEQN,
                        ac_urico   = LBXSUA,  # Ácido úrico (mg/dL) — no ciclo L: LBXSUA
                        creatinina = LBXSCR), # Creatinina sérica (mg/dL)
    by = "SEQN") %>%
  left_join(
    alq_L %>% select(SEQN, alcool = ALQ130),  # Doses de álcool por dia (últimos 12 meses)
    by = "SEQN") %>%
  left_join(
    # Ciclo L: a pergunta sobre ronco (SLQ030) foi REMOVIDA do questionário —
    # restam apenas as horas de sono
    slq_L %>% select(SEQN, sono_horas = SLD012),  # Horas de sono por noite
    by = "SEQN") %>%
  left_join(
    huq_L %>% select(SEQN, saude_geral = HUQ010), # Saúde geral ("Excellent"..."Poor")
    by = "SEQN") %>%
  left_join(
    dpq_L %>% select(SEQN,
                     dpq1 = DPQ010, dpq2 = DPQ020, dpq3 = DPQ030,
                     dpq4 = DPQ040, dpq5 = DPQ050, dpq6 = DPQ060,
                     dpq7 = DPQ070, dpq8 = DPQ080, dpq9 = DPQ090),
    by = "SEQN")

# ── 3. Limpar e derivar variáveis novas ──────────────────────────────────────
# Códigos de recusa/não sabe no NHANES: 777/999 (ALQ130), 7777/9999 (SLD012) etc.
invalidos_num <- c(777, 999, 7777, 9999, 77777, 99999)

nhanes_ext <- nhanes_ext %>%
  mutate(
    # Álcool: pressuposto didático — NA em ALQ130 tratado como 0 (não bebe /
    # não informou); em análise real, investigar o motivo da perda
    alcool = ifelse(is.na(alcool) | alcool %in% invalidos_num, 0, alcool),

    # Horas de sono: códigos inválidos → NA
    sono_horas = ifelse(sono_horas %in% invalidos_num, NA, sono_horas),

    # Saúde geral: colapsar em boa vs. ruim
    # ATENÇÃO: no ciclo L os labels vêm com pontuação ("Excellent,", "Poor?" ...)
    saude_geral = case_when(
      as.character(saude_geral) %in% c("Excellent,", "Very good,", "Good,") ~ "Boa",
      as.character(saude_geral) %in% c("Fair, or", "Poor?")                 ~ "Ruim",
      TRUE ~ NA_character_
    ),
    saude_geral = factor(saude_geral, levels = c("Boa", "Ruim")),

    # Score PHQ-9: soma dos 9 itens (0–3 cada; NA em qualquer item → NA no score)
    # ATENÇÃO: o nhanesA traduz os itens DPQ para FATORES com labels
    # ("Not at all", "Several days", ...) — as.numeric() retornaria o índice
    # do nível (1–6), não o escore. É preciso mapear os labels explicitamente:
    across(dpq1:dpq9, ~ case_when(
      . == "Not at all"             ~ 0,
      . == "Several days"           ~ 1,
      . == "More than half the days" ~ 2,
      . == "Nearly every day"       ~ 3,
      TRUE ~ NA_real_  # "Refused", "Don't know"
    )),
    phq9 = rowSums(across(dpq1:dpq9), na.rm = FALSE)
  ) %>%
  select(-dpq1, -dpq2, -dpq3, -dpq4, -dpq5, -dpq6, -dpq7, -dpq8, -dpq9)

# Frequência cardíaca de repouso: 3ª medida de pulso oscilométrico (BPXO_L)
fc_raw <- nhanes_cache("BPXO_L", "data/bpxo_L.rds") %>%
  select(SEQN, freq_cardiaca = BPXOPLS3)
nhanes_ext <- nhanes_ext %>% left_join(fc_raw, by = "SEQN")

# ── 4. Tabela descritiva das variáveis novas ──────────────────────────────────
# Salva em output/exercicio/ (mesmo padrão do 00_config.R) — vira a
# "Tabela da análise 2" referenciada no README
dir.create("output/exercicio", showWarnings = FALSE)

tabela_desc_ampliada <- nhanes_ext %>%
  select(hba1c, hdl, pcr, ac_urico, creatinina,
         alcool, sono_horas, saude_geral, phq9,
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
      phq9          ~ "Score PHQ-9 (depressão)",
      escolaridade  ~ "Escolaridade",
      est_civil     ~ "Estado civil",
      imc           ~ "IMC (kg/m²)",
      freq_cardiaca ~ "Frequência cardíaca de repouso (bpm)"
    )
  )

tabela_desc_ampliada %>%
  as_gt() %>%
  gt::gtsave("output/exercicio/tabela_descritiva_ampliada.html")

# PNG gerado nativamente (flextable + device png do R), sem depender de
# navegador — o gt::gtsave em PNG exigiria Chrome via webshot2/chromote.
ft_ampliada <- tabela_desc_ampliada %>%
  as_flex_table() %>%
  flextable::autofit()

dm <- dim(ft_ampliada)  # dimensões em polegadas: o PNG sai no tamanho exato

png("output/exercicio/tabela_descritiva_ampliada.png",
    width = sum(dm$widths) * 300, height = sum(dm$heights) * 300, res = 300)
grid::grid.newpage()
grid::grid.draw(flextable::gen_grob(ft_ampliada, fit = "auto"))
dev.off()

# ── 5. Preparar dados para modelagem (sobrescreve os objetos do 00_config.R) ──
# ATENÇÃO: a partir daqui, dados/dados_treino/dados_teste contêm 22 preditores.
# Os scripts 01–04 usarão este conjunto automaticamente.

dados <- nhanes_ext %>%
  select(
    has_pas,
    # ── preditores do baseline ────────────────────────────────────────────
    idade, sexo, raca, whr, renda, colesterol, diabetes, fumante, sedentarismo,
    # ── variáveis já no setup, não usadas no baseline ─────────────────────
    escolaridade, est_civil, imc, freq_cardiaca,
    # ── marcadores laboratoriais novos ────────────────────────────────────
    hba1c, hdl, pcr, ac_urico, creatinina,
    # ── comportamentais / contextuais ─────────────────────────────────────
    alcool, sono_horas, saude_geral, phq9
  ) %>%
  drop_na()

set.seed(42)
split        <- initial_split(dados, prop = 0.75, strata = has_pas)
dados_treino <- training(split)
dados_teste  <- testing(split)

cat(sprintf(
  "Banco ampliado pronto: %d obs × %d preditores (treino: %d | teste: %d)\n\n",
  nrow(dados), ncol(dados) - 1, nrow(dados_treino), nrow(dados_teste)
))

# ══════════════════════════════════════════════════════════════════════════════
# 6. MODELOS — versões condensadas dos scripts 01–04
# ══════════════════════════════════════════════════════════════════════════════
# Mesmos 4 modelos da Análise 1, sem os gráficos de conceito (que já foram
# vistos): aqui importa só a comparação — AUC, ROC e importância das variáveis.
# Tudo é salvo em output/exercicio/: os resultados da Análise 1 ficam intactos.

# ── Funções auxiliares do bloco de avaliação (usadas pelos 4 modelos) ───────
# pred_prob: probabilidade do evento (has_pas = "Sim")
pred_prob <- function(object, newdata) {
  predict(object, newdata, type = "prob")$.pred_Sim
}

# AUC com evento = "Sim" (2º nível do fator) — mesma convenção dos scripts 01–04
auc_evento_sim <- function(truth, estimate) {
  roc_auc_vec(truth, estimate, event_level = "second")
}

# Rótulos PT-BR para os 22 preditores (gráficos de importância)
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
  "saude_geral" = "Saúde autorrelatada", "phq9" = "PHQ-9 (depressão)"
)

# avalia(): AUC + curva ROC + importância por permutação de um modelo ajustado.
# É a condensação dos blocos de avaliação dos scripts 01–04 — repare que o
# MESMO código serve para os 4 modelos: essa uniformidade é o tidymodels.
avalia <- function(modelo, nome, arquivo) {
  # AUC + ROC no conjunto de teste
  probs <- predict(modelo, dados_teste, type = "prob") %>%
    bind_cols(dados_teste %>% select(has_pas)) %>%
    rename(prob_evento = .pred_Sim)
  auc <- roc_auc(probs, truth = has_pas, prob_evento, event_level = "second")
  cat(sprintf("AUC — %s: %.3f\n", nome, auc$.estimate))

  p_roc <- roc_curve(probs, truth = has_pas, prob_evento, event_level = "second") %>%
    autoplot() +
    labs(title = paste0("Curva ROC — ", nome, " (banco ampliado)")) +
    theme_minimal(base_size = 12)
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
    labs(title = paste0("Importância por permutação — ", nome),
         x = "Queda de AUC", y = NULL) +
    theme_minimal(base_size = 12)
  ggsave(file.path("output/exercicio", paste0(arquivo, "_importancia.png")), p_imp,
         width = 7, height = 6, dpi = 150, bg = "white")

  invisible(auc$.estimate)
}

# Receita única (mesma dos scripts 01–04): dummies + normalização
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
auc_gb_ex <- avalia(modelo_xgb, "Gradient Boosting", "gradient_boosting")

# ── 6.4 Rede Neural (MLP) ────────────────────────────────────────────────────
set.seed(42)
modelo_nn <- workflow() %>%
  add_recipe(receita) %>%
  add_model(mlp(hidden_units = 4, penalty = 0.01, epochs = 300) %>%
              set_engine("nnet") %>% set_mode("classification")) %>%
  fit(data = dados_treino)
auc_nn_ex <- avalia(modelo_nn, "Rede Neural", "rede_neural")

# ── 7. Resumo: AUCs do banco ampliado ────────────────────────────────────────
# Compare com os resultados da Análise 1 (banco baseline), salvos em output/
cat("\n══ Resumo — AUCs do banco ampliado ════════════════════════════════════\n")
resumo_auc <- data.frame(
  Modelo = c("Regressão Logística", "Random Forest",
             "Gradient Boosting (XGBoost)", "Rede Neural (MLP)"),
  AUC    = c(auc_lr_ex, auc_rf_ex, auc_gb_ex, auc_nn_ex)
)
print(resumo_auc %>% arrange(desc(AUC)))
cat("\nGráficos salvos em output/exercicio/ — compare com os da Análise 1.\n")

# ══════════════════════════════════════════════════════════════════════════════
# fim do script ────────────────────────────────────────────────────────────────
# ══════════════════════════════════════════════════════════════════════════════