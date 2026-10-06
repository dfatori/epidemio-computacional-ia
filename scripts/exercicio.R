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
# COMPARABILIDADE: as duas rodadas (baseline e ampliado) usam EXATAMENTE as
# mesmas pessoas e o mesmo split treino/teste — só muda o conjunto de
# preditores. Sem isso, uma diferença de AUC poderia vir da mudança de
# amostra, não das variáveis novas.
#
# TAREFA DO ALUNO:
#   1. Deixe a CHAVE abaixo em FALSE → rode este script → rode 01–04 → anote
#      os AUCs (baseline do exercício: 9 preditores)
#   2. Mude a chave para TRUE → rode este script de novo → rode 01–04 →
#      compare os AUCs (22 preditores, mesmas pessoas)
#   3. Quais variáveis apareceram como mais importantes?
#   4. O ganho de AUC justifica a complexidade extra?
#   5. Qual modelo performa melhor? Segundo quais critérios?
# ══════════════════════════════════════════════════════════════════════════════

# ── CHAVE DO EXERCÍCIO ────────────────────────────────────────────────────────
# FALSE = baseline (9 preditores)   |   TRUE = banco ampliado (22 preditores)
usar_banco_ampliado <- FALSE

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

tabela_desc_ampliada %>%
  as_gt() %>%
  gt::gtsave("output/exercicio/tabela_descritiva_ampliada.png")

# ── 5. Preparar dados para modelagem (sobrescreve os objetos do 00_config.R) ──
# drop_na() é aplicado ao banco COMPLETO (22 preditores), ANTES de escolher
# as colunas da rodada: assim baseline e ampliado usam as mesmas pessoas.

dados_completos <- nhanes_ext %>%
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

# Split ÚNICO sobre o banco completo — mesma semente e mesmas linhas nas
# duas rodadas, então treino e teste são idênticos nos dois modos
set.seed(42)
split  <- initial_split(dados_completos, prop = 0.75, strata = has_pas)
treino <- training(split)
teste  <- testing(split)

# A chave escolhe apenas as COLUNAS — nunca as linhas
vars_baseline <- c("idade", "sexo", "raca", "whr", "renda", "colesterol",
                   "diabetes", "fumante", "sedentarismo")

if (usar_banco_ampliado) {
  dados_treino <- treino
  dados_teste  <- teste
} else {
  dados_treino <- treino %>% select(has_pas, all_of(vars_baseline))
  dados_teste  <- teste  %>% select(has_pas, all_of(vars_baseline))
}
dados <- bind_rows(dados_treino, dados_teste)

cat(sprintf(
  "\nAmostra do exercício: %d obs (treino: %d | teste: %d) — IDÊNTICA nas duas rodadas\n",
  nrow(dados), nrow(dados_treino), nrow(dados_teste)
))
if (usar_banco_ampliado) {
  cat(sprintf("Modo AMPLIADO: %d preditores. Rode 01–04 e compare com os AUCs anotados.\n",
              ncol(dados) - 1))
} else {
  cat("Modo BASELINE: 9 preditores. Rode 01–04, anote os AUCs,\n")
  cat("depois mude usar_banco_ampliado para TRUE e repita.\n")
}

# ══════════════════════════════════════════════════════════════════════════════
# fim do script ────────────────────────────────────────────────────────────────
# ══════════════════════════════════════════════════════════════════════════════
