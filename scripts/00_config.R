# ══════════════════════════════════════════════════════════════════════════════
# NHANES 2021-2023 (ciclo L): Predicao de Hipertensao Arterial Sistemica (HAS)
# https://cran.r-project.org/web/packages/nhanesA/vignettes/Introducing_nhanesA.html
# ══════════════════════════════════════════════════════════════════════════════

# ── 0. Carrega pacotes ────────────────────────────────────────────────
pkg_load <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

pkg_load("nhanesA")
pkg_load("dplyr")
pkg_load("forcats")
pkg_load("gtsummary")
theme_gtsummary_language("pt", decimal.mark = ",", big.mark = ".")
pkg_load("tidymodels")
# Engines dos modelos: tidymodels NÃO as instala automaticamente (são
# "Suggests" do parsnip): sem estas linhas, os scripts 02 e 03 falham
# em uma máquina limpa.
pkg_load("ranger")
pkg_load("xgboost")
pkg_load("vip")
pkg_load("rpart.plot")
pkg_load("gbm")
pkg_load("NeuralNetTools")
pkg_load("ggplotify")
pkg_load("brulee")   # usado no script 04 para a comparação 1 vs 2 camadas ocultas
pkg_load("flextable")  # PNGs de tabela via gráficos nativos do R, sem Chrome

# O pacote brulee precisa do backend torch: bibliotecas nativas que não
# vem com R. Importante: sem torch, o script 04 falha.
# Abaixo, instala automaticamente se estiver faltando
if (requireNamespace("torch", quietly = TRUE) && !torch::torch_is_installed()) {
  message("Instalando o backend torch (necessário para o script 04, uma vez só)...")
  torch::install_torch()
}

# Formatador numérico PT-BR para rótulos dentro das figuras (vírgula decimal)
fmt_br <- function(x, digits = 3) sub(".", ",", sprintf(paste0("%.", digits, "f"), x), fixed = TRUE)

# ── 1. Carregar tabelas ────────────────────
# Baixa do NHANES na primeira execução; usa cache local nas seguintes.
nhanes_cache <- function(table, path) {
  if (file.exists(path)) readRDS(path) else { x <- nhanes(table); saveRDS(x, path); x }
}

demo_L   <- nhanes_cache('DEMO_L',  "data/demo_L.rds")    # Demograficos: idade, sexo, raca, renda
bmx_L    <- nhanes_cache('BMX_L',   "data/bmx_L.rds")     # Antropometria: IMC, cintura
bpxo_L   <- nhanes_cache('BPXO_L',  "data/bpxo_L.rds")    # Pressao arterial oscilometrica
diq_L    <- nhanes_cache('DIQ_L',   "data/diq_L.rds")     # Diabetes diagnosticada
tchol_L  <- nhanes_cache('TCHOL_L', "data/tchol_L.rds")   # Colesterol total
smq_L    <- nhanes_cache('SMQ_L',   "data/smq_L.rds")     # Tabagismo
paq_L    <- nhanes_cache('PAQ_L',   "data/paq_L.rds")     # Atividade fisica: sedentarismo (PAD680)

# ── 2. Juntar pelo SEQN (ID dos participantes) ────────────────────────────
nhanes_cardio <- demo_L %>%
  left_join(bmx_L,   by = "SEQN") %>%
  left_join(bpxo_L,  by = "SEQN") %>%
  left_join(diq_L,   by = "SEQN") %>%
  left_join(tchol_L, by = "SEQN") %>%
  left_join(smq_L,   by = "SEQN") %>%
  left_join(paq_L,   by = "SEQN")

# ── 3. Selecionar e renomear variaveis de interesse ───────────────────────────
# NOTA: nhanesA ja traduz os codigos para labels (ex: 1 → "Yes", 2 → "No")
nhanes_cardio <- nhanes_cardio %>%
  select(
    SEQN,
    idade      = RIDAGEYR,
    sexo       = RIAGENDR, # "Male"(5575), "Female"(6358)
    raca       = RIDRETH3, # "Mexican American", "Other Hispanic", "Non-Hispanic White",
                           # "Non-Hispanic Black", "Non-Hispanic Asian", "Other Race - Including Multi-Racial"
    renda        = INDFMPIR,  # Razao renda/linha de pobreza (continuo)
    escolaridade = DMDEDUC2,  # "Less than 9th grade", ..., "College graduate or above"
    est_civil    = DMDMARTZ,  # "Married/Living with partner", "Widowed/Divorced/Separated", "Never married"
    imc          = BMXBMI,
    altura       = BMXHT,     # Estatura (cm): para calcular WHtR
    cintura    = BMXWAIST,
    diabetes   = DIQ010,   # "Yes"(1081), "No"(10371), "Borderline"(284), "Don't know"(4)
    colesterol   = LBXTC,
    smq020       = SMQ020,     # "Yes"(fumou 100+ cigarros na vida), "No"
    smq040       = SMQ040,     # "Every day", "Some days", "Not at all": só para quem fumou
    sedentarismo = PAD680,     # Minutos sentado por dia (contínuo)
    pas1         = BPXOSY1,    # Pressão sistólica: 1ª medida oscilométrica (mmHg)
    pas2         = BPXOSY2,    # 2ª medida
    pas3         = BPXOSY3     # 3ª medida
  )

# ── 4. Explorar valores brutos ANTES de limpar ────────────────────────────────
nhanes_cardio %>%
  select(-SEQN) %>%
  tbl_summary(missing = "ifany")

# ── 5. Limpar respostas invalidas → NA, converter fatores ────────────────────
# Usa as.character() antes do ifelse para evitar problema com colunas ja em fator
# NOTA: raca nao entra: sem categorias invalidas
# NOTA: diabetes mantem "Borderline" como categoria valida
invalidos <- c("Don't know", "Refused")

nhanes_cardio <- nhanes_cardio %>%
  mutate(
    diabetes  = ifelse(as.character(diabetes)  %in% invalidos,      NA, as.character(diabetes)),
    smq020       = ifelse(as.character(smq020) %in% invalidos, NA, as.character(smq020)),
    smq040       = ifelse(as.character(smq040) %in% invalidos, NA, as.character(smq040)),
    escolaridade = ifelse(as.character(escolaridade) %in% invalidos, NA, as.character(escolaridade)),
    est_civil    = ifelse(as.character(est_civil)    %in% invalidos, NA, as.character(est_civil)),
    across(c(sexo, raca, diabetes, escolaridade, est_civil), factor)
  ) %>%
  # ── Traduzir, colapsar e derivar variáveis ───────────────────
  mutate(
    sexo         = fct_recode(sexo, "Masculino" = "Male", "Feminino" = "Female"),
    raca         = fct_collapse(
                     fct_recode(raca,
                       "Mexicano-americano"      = "Mexican American",
                       "Outro hispânico"         = "Other Hispanic",
                       "Branco não-hispânico"    = "Non-Hispanic White",
                       "Negro não-hispânico"     = "Non-Hispanic Black",
                       "Asiático não-hispânico"  = "Non-Hispanic Asian",
                       "Outra raça/multirracial" = "Other Race - Including Multi-Racial"),
                     # "Demais" primeiro: vira a referência, e o OR se refere a
                     # ser branco vs. demais (rótulo "Raça (branco)" nos scripts 01/03)
                     "Demais" = c("Mexicano-americano", "Outro hispânico",
                                  "Negro não-hispânico", "Asiático não-hispânico",
                                  "Outra raça/multirracial"),
                     "Branco" = "Branco não-hispânico"),
    # NOTA: "Limítrofe" colapsado em "Não": decisão simplificadora, discutível em aula
    # A ORDEM dos níveis importa: o 1º nível é a referência das dummies
    # (step_dummy), logo é o denominador dos ORs: "Não" antes de "Sim"
    # faz o OR se referir a TER diabetes (direção epidemiologicamente intuitiva)
    diabetes     = fct_collapse(
                     fct_recode(diabetes, "Sim" = "Yes", "Não" = "No", "Limítrofe" = "Borderline"),
                     "Não" = c("Não", "Limítrofe"),
                     "Sim" = "Sim"),
    fumante      = factor(case_when(
                     smq020 == "No"                                            ~ "Nunca fumou",
                     smq020 == "Yes" & smq040 == "Not at all"                  ~ "Ex-fumante",
                     smq020 == "Yes" & smq040 %in% c("Every day", "Some days") ~ "Fumante atual",
                     TRUE ~ NA_character_
                   ), levels = c("Nunca fumou", "Ex-fumante", "Fumante atual")),
    escolaridade = fct_collapse(
                     fct_recode(escolaridade,
                       "< 9º ano"             = "Less than 9th grade",
                       "9º-11º ano"           = "9-11th grade (Includes 12th grade with no diploma)",
                       "Ensino médio/GED"     = "High school graduate/GED or equivalent",
                       "Faculdade incompleta" = "Some college or AA degree",
                       "Graduação ou mais"    = "College graduate or above"),
                     "Graduação ou mais" = "Graduação ou mais",
                     "Sem graduação"     = c("< 9º ano", "9º-11º ano", "Ensino médio/GED", "Faculdade incompleta")),
    est_civil    = fct_collapse(
                     fct_recode(est_civil,
                       "Casado(a)/União estável"             = "Married/Living with partner",
                       "Viúvo(a)/Divorciado(a)/Separado(a)" = "Widowed/Divorced/Separated",
                       "Solteiro(a)"                        = "Never married"),
                     "Casado(a)/União estável" = "Casado(a)/União estável",
                     "Demais"                  = c("Viúvo(a)/Divorciado(a)/Separado(a)", "Solteiro(a)")),
    # PAS: média das 3 medidas oscilométricas (recomendação NHANES:
    # reduz a variabilidade de uma leitura isolada)
    pas          = rowMeans(cbind(pas1, pas2, pas3), na.rm = TRUE),
    # PAS ≥ 140 mmHg: Critério BR
    # NOTA: medida em um único dia NÃO é diagnóstico clínico de HAS:
    # é um desfecho operacional para fins didáticos.
    # Os níveis ficam na ordem alfabética padrão ("Não", "Sim"):
    #   • glm() modela P(2º nível) = P("Sim") → ORs da regressão logística
    #     se referem a TER HAS (direção epidemiologicamente intuitiva)
    #   • o yardstick assume o 1º nível como evento: por isso os scripts
    #     01-04 passam event_level = "second" explicitamente na ROC/AUC
    has_pas      = factor(ifelse(pas >= 140, "Sim", "Não")),
    # WHtR: Waist-to-Height Ratio (Ashwell et al.), risco ↑ se > 0,5
    whr          = cintura / altura
  ) %>%
  select(-smq020, -smq040, -pas1, -pas2, -pas3)

# ── 6. Restringir a adultos (≥ 18 anos) ──────────────────────────────────────
nhanes_cardio <- nhanes_cardio %>%
  filter(idade >= 18)

# ── 7. Tabela descritiva final (labels em PT-BR) ──────────────────────────────
tabela_desc <- nhanes_cardio %>%
  select(-SEQN) %>%
  tbl_summary(
    missing = "ifany",
    missing_text = "Perda",
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    label = list(
      idade      ~ "Idade (anos)",
      sexo       ~ "Sexo",
      raca       ~ "Raça/Etnia",
      renda      ~ "Renda (razão pobreza)",
      imc        ~ "IMC (kg/m²)",
      cintura    ~ "Circunferência abdominal (cm)",
      altura     ~ "Estatura (cm)",
      whr        ~ "Razão cintura/estatura",
      diabetes   ~ "Diabetes",
      colesterol ~ "Colesterol total (mg/dL)",
      fumante      ~ "Tabagismo",
      sedentarismo ~ "Sedentarismo (min/dia sentado)",
      pas          ~ "Pressão arterial sistólica (mmHg)",
      has_pas      ~ "Hipertensão (≥ 140 mmHg)",
      escolaridade ~ "Escolaridade",
      est_civil    ~ "Estado civil"
    )
  )

# Tabela descritiva em HTML e PNG
tabela_desc %>% 
  as_gt() %>% 
  gt::gtsave("output/00_config/tabela_descritiva.html")

# PNG gerado nativamente (flextable + device png do R), sem depender de
# navegador: gt::gtsave em PNG exigiria Chrome via webshot2/chromote
ft_desc <- tabela_desc %>%
  as_flex_table() %>%
  flextable::autofit()

dm <- dim(ft_desc)  # dimensões em polegadas: o PNG sai no tamanho exato

png("output/00_config/tabela_descritiva.png",
    width = sum(dm$widths) * 300, height = sum(dm$heights) * 300, res = 300)
grid::grid.newpage()
grid::grid.draw(flextable::gen_grob(ft_desc, fit = "auto"))
dev.off()

# ── 8. Banco ampliado: marcadores laboratoriais e comportamentais ─────────────
# Mesma função de cache. Estas 12 variáveis extras entram no EXERCÍCIO e
# definem a amostra comum das duas análises (ver seção 9).
ghb_L    <- nhanes_cache("GHB_L",    "data/ghb_L.rds")    # HbA1c
hdl_L    <- nhanes_cache("HDL_L",    "data/hdl_L.rds")    # HDL colesterol
hscrp_L  <- nhanes_cache("HSCRP_L",  "data/hscrp_L.rds")  # PCR alta sensibilidade
biopro_L <- nhanes_cache("BIOPRO_L", "data/biopro_L.rds") # Bioquímica (ác. úrico, creatinina)
alq_L    <- nhanes_cache("ALQ_L",    "data/alq_L.rds")    # Álcool
slq_L    <- nhanes_cache("SLQ_L",    "data/slq_L.rds")    # Sono
huq_L    <- nhanes_cache("HUQ_L",    "data/huq_L.rds")    # Saúde autorrelatada

nhanes_ext <- nhanes_cardio %>%
  left_join(
    ghb_L    %>% 
    select(SEQN, hba1c        = LBXGH),     # HbA1c (%)
    by = "SEQN") %>%
  left_join(
    hdl_L    %>% 
    select(SEQN, hdl           = LBDHDD),   # HDL (mg/dL)
    by = "SEQN") %>%
  left_join(
    hscrp_L  %>% 
    select(SEQN, pcr           = LBXHSCRP), # PCR alta sensib. (mg/L)
    by = "SEQN") %>%
  left_join(
    biopro_L %>% 
    select(SEQN,
                        ac_urico   = LBXSUA,  # Ácido úrico (mg/dL). No ciclo L: LBXSUA
                        creatinina = LBXSCR), # Creatinina sérica (mg/dL)
    by = "SEQN") %>%
  left_join(
    alq_L %>% 
    select(SEQN, alcool = ALQ130),  # Doses de álcool por dia (últimos 12 meses)
    by = "SEQN") %>%
  left_join(
    # Ciclo L: a pergunta sobre ronco (SLQ030) foi REMOVIDA do questionário:
    # restam apenas as horas de sono
    slq_L %>% select(SEQN, sono_horas = SLD012),  # Horas de sono por noite
    by = "SEQN") %>%
  left_join(
    huq_L %>% 
    select(SEQN, saude_geral = HUQ010), # Saúde geral ("Excellent"..."Poor")
    by = "SEQN") %>%
  # Frequência cardíaca de repouso: 3ª medida de pulso oscilométrico
  left_join(
    bpxo_L %>% 
    select(SEQN, freq_cardiaca = BPXOPLS3),
    by = "SEQN")

# ── 8.1 Limpar e derivar variáveis novas ──────────────────────────────────────
# Códigos de recusa/não sabe no NHANES: 777/999 (ALQ130), 7777/9999 (SLD012) etc.
invalidos_num <- c(777, 999, 7777, 9999, 77777, 99999)

nhanes_ext <- nhanes_ext %>%
  mutate(
    # Álcool: pressuposto didático: NA em ALQ130 tratado como 0 (não bebe /
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
    saude_geral = factor(saude_geral, levels = c("Boa", "Ruim"))
  )

# ── 9. Preparar dados para modelagem ──────────────────────────────────────────
# AMOSTRA COMUM: casos completos nas 21 variáveis do banco ampliado.
# A Análise 1 usa um subconjunto de COLUNAS sobre as MESMAS LINHAS e o MESMO
# split: assim os AUCs dos scripts 01-04 e do exercicio.R diferem apenas pelo
# número de preditores, nunca pela amostra.
vars_base <- c("idade", "sexo", "raca", "whr", "renda", "colesterol",
               "diabetes", "fumante", "sedentarismo")
vars_ampliado <- c(vars_base,
                   "escolaridade", "est_civil", "imc", "freq_cardiaca",
                   "hba1c", "hdl", "pcr", "ac_urico", "creatinina",
                   "alcool", "sono_horas", "saude_geral")

dados_ampliado <- nhanes_ext %>%
  select(has_pas, all_of(vars_ampliado)) %>%
  drop_na() # Remove observações com dados perdidos

set.seed(42)
split                 <- initial_split(dados_ampliado, prop = 0.75, strata = has_pas)
dados_treino_ampliado <- training(split)
dados_teste_ampliado  <- testing(split)

# Banco da Análise 1 (9 preditores): mesmas linhas, menos colunas
dados        <- dados_ampliado        %>% select(has_pas, all_of(vars_base))
dados_treino <- dados_treino_ampliado %>% select(has_pas, all_of(vars_base))
dados_teste  <- dados_teste_ampliado  %>% select(has_pas, all_of(vars_base))

cat(sprintf(
  "Amostra comum: %d obs (treino: %d | teste: %d) | Análise 1: %d preditores | exercício: %d\n",
  nrow(dados_ampliado), nrow(dados_treino_ampliado), nrow(dados_teste_ampliado),
  length(vars_base), length(vars_ampliado)
))

# ══════════════════════════════════════════════════════════════════════════════
# fim do script ────────────────────────────────────────────────────────────────
# ══════════════════════════════════════════════════════════════════════════════