# ══════════════════════════════════════════════════════════════════════════════
# EXERCÍCIO — Banco de dados ampliado para predição de Hipertensão
#
# OBJETIVO: expandir o conjunto de preditores do pipeline principal (9 variáveis)
# para ~20 variáveis, baixando tabelas adicionais do NHANES e incorporando
# marcadores laboratoriais, comportamentais e sociais.
#
# PRÉ-REQUISITO: rode 00_setup.R antes. Este script sobrescreve os objetos
#   dados, dados_treino e dados_teste no ambiente — após rodar, os scripts
#   01–04 usarão automaticamente o conjunto ampliado.
#
# TAREFA DO ALUNO:
#   1. Rode 00_setup.R → rode 01–04 → anote os AUCs (baseline)
#   2. Rode este script → rode 01–04 novamente → compare os AUCs
#   3. Quais variáveis apareceram como mais importantes?
#   4. O ganho de AUC justifica a complexidade extra?
# ══════════════════════════════════════════════════════════════════════════════

# ── 1. Baixar tabelas adicionais ──────────────────────────────────────────────
# Mesma função de cache usada em 00_setup.R
ghb_L    <- nhanes_cache("GHB_L",    "data/ghb_L.rds")    # HbA1c
trigly_L <- nhanes_cache("TRIGLY_L", "data/trigly_L.rds") # Triglicerídeos
hdl_L    <- nhanes_cache("HDL_L",    "data/hdl_L.rds")    # HDL colesterol
hscrp_L  <- nhanes_cache("HSCRP_L",  "data/hscrp_L.rds")  # PCR alta sensibilidade
biopro_L <- nhanes_cache("BIOPRO_L", "data/biopro_L.rds") # Bioquímica (ác. úrico, creatinina)
alq_L    <- nhanes_cache("ALQ_L",    "data/alq_L.rds")    # Álcool
slq_L    <- nhanes_cache("SLQ_L",    "data/slq_L.rds")    # Sono
huq_L    <- nhanes_cache("HUQ_L",    "data/huq_L.rds")    # Saúde autorrelatada
dpq_L    <- nhanes_cache("DPQ_L",    "data/dpq_L.rds")    # Depressão (PHQ-9)

# ── 2. Join com nhanes_cardio ─────────────────────────────────────────────────
nhanes_ext <- nhanes_cardio %>%
  left_join(
    ghb_L    %>% select(SEQN, hba1c        = LBXGH),     # HbA1c (%)
    by = "SEQN") %>%
  left_join(
    trigly_L %>% select(SEQN, triglicerides = LBXTR),    # Triglicerídeos (mg/dL)
    by = "SEQN") %>%
  left_join(
    hdl_L    %>% select(SEQN, hdl           = LBDHDD),   # HDL (mg/dL)
    by = "SEQN") %>%
  left_join(
    hscrp_L  %>% select(SEQN, pcr           = LBXHSCRP), # PCR alta sensib. (mg/L)
    by = "SEQN") %>%
  left_join(
    biopro_L %>% select(SEQN,
                        ac_urico   = LBXUA,   # Ácido úrico (mg/dL)
                        creatinina = LBXSCR), # Creatinina sérica (mg/dL)
    by = "SEQN") %>%
  left_join(
    alq_L %>% select(SEQN, alcool = ALQ130),  # Doses de álcool por dia (últimos 12 meses)
    by = "SEQN") %>%
  left_join(
    slq_L %>% select(SEQN,
                     sono_horas = SLD012,  # Horas de sono por noite
                     ronco      = SLQ030), # Ronca? ("Never", "Rarely", "Occasionally", "Frequently", "Almost always")
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
invalidos_num <- c(7777, 9999, 77777, 99999)  # códigos de recusa/não sabe no NHANES

nhanes_ext <- nhanes_ext %>%
  mutate(
    # Álcool: 0 para quem nunca bebeu (NA em ALQ130 = não bebe)
    alcool = ifelse(is.na(alcool) | alcool %in% invalidos_num, 0, alcool),

    # Ronco: colapsar em binário (ronca vs. não ronca)
    ronco = case_when(
      as.character(ronco) %in% c("Never", "Rarely")                        ~ "Não",
      as.character(ronco) %in% c("Occasionally", "Frequently",
                                  "Almost always")                          ~ "Sim",
      TRUE ~ NA_character_
    ),
    ronco = factor(ronco, levels = c("Não", "Sim")),

    # Saúde geral: colapsar em boa vs. ruim
    saude_geral = case_when(
      as.character(saude_geral) %in% c("Excellent", "Very good", "Good") ~ "Boa",
      as.character(saude_geral) %in% c("Fair", "Poor")                   ~ "Ruim",
      TRUE ~ NA_character_
    ),
    saude_geral = factor(saude_geral, levels = c("Boa", "Ruim")),

    # Score PHQ-9: soma dos 9 itens (0–3 cada; NA em qualquer item → NA no score)
    across(dpq1:dpq9, ~ ifelse(as.numeric(.) %in% c(7, 9), NA, as.numeric(.))),
    phq9 = rowSums(across(dpq1:dpq9), na.rm = FALSE)
  ) %>%
  select(-dpq1, -dpq2, -dpq3, -dpq4, -dpq5, -dpq6, -dpq7, -dpq8, -dpq9)

# ── 4. Tabela descritiva das variáveis novas ──────────────────────────────────
nhanes_ext %>%
  select(hba1c, triglicerides, hdl, pcr, ac_urico, creatinina,
         alcool, sono_horas, ronco, saude_geral, phq9,
         escolaridade, est_civil, imc, freq_cardiaca) %>%
  tbl_summary(
    missing = "ifany",
    missing_text = "Perda",
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    label = list(
      hba1c         ~ "HbA1c (%)",
      triglicerides ~ "Triglicerídeos (mg/dL)",
      hdl           ~ "HDL (mg/dL)",
      pcr           ~ "PCR alta sensibilidade (mg/L)",
      ac_urico      ~ "Ácido úrico (mg/dL)",
      creatinina    ~ "Creatinina sérica (mg/dL)",
      alcool        ~ "Álcool (doses/dia)",
      sono_horas    ~ "Horas de sono/noite",
      ronco         ~ "Ronco/apneia",
      saude_geral   ~ "Saúde geral autorrelatada",
      phq9          ~ "Score PHQ-9 (depressão)",
      escolaridade  ~ "Escolaridade",
      est_civil     ~ "Estado civil",
      imc           ~ "IMC (kg/m²)"
    )
  )

# ── 5. Preparar dados para modelagem (sobrescreve os objetos do 00_setup.R) ──
# ATENÇÃO: a partir daqui, dados/dados_treino/dados_teste contêm ~20 preditores.
# Os scripts 01–04 usarão este conjunto automaticamente.

# Frequência cardíaca: já está em bpxo_L (BPXOPLS3), extrair de nhanes_cardio
# se ainda não foi adicionada
if (!"freq_cardiaca" %in% names(nhanes_ext)) {
  fc_raw <- nhanes_cache("BPXO_L", "data/bpxo_L.rds") %>%
    select(SEQN, freq_cardiaca = BPXOPLS3)
  nhanes_ext <- nhanes_ext %>% left_join(fc_raw, by = "SEQN")
}

dados <- nhanes_ext %>%
  select(
    has_pas,
    # ── preditores do baseline ────────────────────────────────────────────
    idade, sexo, raca, whr, renda, colesterol, diabetes, fumante, sedentarismo,
    # ── variáveis já no setup, não usadas no baseline ─────────────────────
    escolaridade, est_civil, imc, freq_cardiaca,
    # ── marcadores laboratoriais novos ────────────────────────────────────
    hba1c, triglicerides, hdl, pcr, ac_urico, creatinina,
    # ── comportamentais / contextuais ─────────────────────────────────────
    alcool, sono_horas, ronco, saude_geral, phq9
  ) %>%
  drop_na()

cat(sprintf(
  "Baseline: %d obs × 9 preditores\nAmpliado: %d obs × %d preditores\nPerda de obs por drop_na(): %+d\n",
  nrow(dados_treino) + nrow(dados_teste),
  nrow(dados),
  ncol(dados) - 1,
  nrow(dados) - (nrow(dados_treino) + nrow(dados_teste))
))

set.seed(42)
split        <- initial_split(dados, prop = 0.75, strata = has_pas)
dados_treino <- training(split)
dados_teste  <- testing(split)

cat("Pronto. Rode os scripts 01–04 e compare os desempenhos.\n")

# ══════════════════════════════════════════════════════════════════════════════
# fim do script ────────────────────────────────────────────────────────────────
# ══════════════════════════════════════════════════════════════════════════════
