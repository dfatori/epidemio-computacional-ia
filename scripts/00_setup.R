# ══════════════════════════════════════════════════════════════════════════════
# NHANES 2021-2023 (ciclo L) — Predicao de Hipertensao Arterial Sistemica (HAS)
# # https://cran.r-project.org/web/packages/nhanesA/vignettes/Introducing_nhanesA.html
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
pkg_load("tidymodels")
pkg_load("vip")
pkg_load("rpart.plot")
pkg_load("gbm")
pkg_load("NeuralNetTools")
pkg_load("ggplotify")
pkg_load("brulee")
pkg_load("webshot2")
# torch::install_torch() # se precisar

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
    altura       = BMXHT,     # Estatura (cm) — para calcular WHtR
    cintura    = BMXWAIST,
    diabetes   = DIQ010,   # "Yes"(1081), "No"(10371), "Borderline"(284), "Don't know"(4)
    colesterol   = LBXTC,
    smq020       = SMQ020,     # "Yes"(fumou 100+ cigarros na vida), "No"
    smq040       = SMQ040,     # "Every day", "Some days", "Not at all" — só para quem fumou
    sedentarismo = PAD680,     # Minutos sentado por dia (contínuo)
    pas          = BPXOSY3    # Pressão sistólica — média das 3 medidas oscilométricas (mmHg)
  )

# ── 4. Explorar valores brutos ANTES de limpar ────────────────────────────────
nhanes_cardio %>%
  select(-SEQN) %>%
  tbl_summary(missing = "ifany")

# ── 5. Limpar respostas invalidas → NA, converter fatores ────────────────────
# Usa as.character() antes do ifelse para evitar problema com colunas ja em fator
# NOTA: raca nao entra — sem categorias invalidas
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
                     "Branco" = "Branco não-hispânico",
                     "Demais" = c("Mexicano-americano", "Outro hispânico",
                                  "Negro não-hispânico", "Asiático não-hispânico",
                                  "Outra raça/multirracial")),
    diabetes     = fct_collapse(
                     fct_recode(diabetes, "Sim" = "Yes", "Não" = "No", "Limítrofe" = "Borderline"),
                     "Sim" = "Sim",
                     "Não" = c("Não", "Limítrofe")),
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
    # PAS ≥ 140 mmHg — Critério BR
    has_pas      = factor(ifelse(pas >= 140, "Sim", "Não")),
    # WHtR — Waist-to-Height Ratio (Ashwell et al.): risco ↑ se > 0,5
    whr          = cintura / altura
  ) %>%
  select(-smq020, -smq040)

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
  gt::gtsave("output/00_setup/tabela_descritiva.html")

tabela_desc %>% 
  as_gt() %>% 
  gt::gtsave("output/00_setup/tabela_descritiva.png")

# ── 8. Preparar dados para modelagem ──────────────────────────────────────────
# Preditores usados em TODOS os scripts de modelagem (01 a 04).
# Split feito aqui uma única vez — garante comparabilidade entre modelos.
dados <- nhanes_cardio %>%
  select(has_pas, idade, sexo, raca, whr, renda, colesterol, diabetes, fumante, sedentarismo) %>%
  drop_na() # Remove observações com dados perdidos

set.seed(42)
split        <- initial_split(dados, prop = 0.75, strata = has_pas)
dados_treino <- training(split)
dados_teste  <- testing(split)

# ══════════════════════════════════════════════════════════════════════════════
# fim do script ────────────────────────────────────────────────────────────────
# ══════════════════════════════════════════════════════════════════════════════