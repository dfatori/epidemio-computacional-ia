# ══════════════════════════════════════════════════════════════════════════════
# NHANES 2021-2023 (ciclo L) — Predicao de Infarto do Miocardio
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
# torch::install_torch() # se precisar

# ── 1. Carregar tabelas ────────────────────
if (file.exists("data/demo_L.rds")) {
  demo_L   <- readRDS("data/demo_L.rds")
  bmx_L    <- readRDS("data/bmx_L.rds")
  bpq_L    <- readRDS("data/bpq_L.rds")
  diq_L    <- readRDS("data/diq_L.rds")
  tchol_L  <- readRDS("data/tchol_L.rds")
  smq_L    <- readRDS("data/smq_L.rds")
  mcq_L    <- readRDS("data/mcq_L.rds")
  paq_L    <- readRDS("data/paq_L.rds")
} else {
  demo_L   <- nhanes('DEMO_L')   # Demograficos: idade, sexo, raca, renda
  bmx_L    <- nhanes('BMX_L')    # Antropometria: IMC, cintura
  bpq_L    <- nhanes('BPQ_L')    # Hipertensao diagnosticada
  diq_L    <- nhanes('DIQ_L')    # Diabetes diagnosticada
  tchol_L  <- nhanes('TCHOL_L')  # Colesterol total
  smq_L    <- nhanes('SMQ_L')    # Tabagismo
  mcq_L    <- nhanes('MCQ_L')    # Desfecho: infarto (MCQ160E)
  paq_L    <- nhanes('PAQ_L')    # Atividade fisica: sedentarismo (PAD680)

  saveRDS(demo_L,  "data/demo_L.rds")
  saveRDS(bmx_L,   "data/bmx_L.rds")
  saveRDS(bpq_L,   "data/bpq_L.rds")
  saveRDS(diq_L,   "data/diq_L.rds")
  saveRDS(tchol_L, "data/tchol_L.rds")
  saveRDS(smq_L,   "data/smq_L.rds")
  saveRDS(mcq_L,   "data/mcq_L.rds")
  saveRDS(paq_L,   "data/paq_L.rds")
}

# ── 2. Juntar pelo SEQN (ID dos participantes) ────────────────────────────
nhanes_cardio <- demo_L %>%
  left_join(bmx_L,   by = "SEQN") %>%
  left_join(bpq_L,   by = "SEQN") %>%
  left_join(diq_L,   by = "SEQN") %>%
  left_join(tchol_L, by = "SEQN") %>%
  left_join(smq_L,   by = "SEQN") %>%
  left_join(mcq_L,   by = "SEQN") %>%
  left_join(paq_L,   by = "SEQN")

# ── 3. Selecionar e renomear variaveis de interesse ───────────────────────────
# NOTA: nhanesA ja traduz os codigos para labels (ex: 1 → "Yes", 2 → "No")
nhanes_cardio <- nhanes_cardio %>%
  select(
    SEQN,
    infarto    = MCQ160E,  # "Yes"(332), "No"(7462), "Don't know"(13)
    idade      = RIDAGEYR,
    sexo       = RIAGENDR, # "Male"(5575), "Female"(6358)
    raca       = RIDRETH3, # "Mexican American", "Other Hispanic", "Non-Hispanic White",
                           # "Non-Hispanic Black", "Non-Hispanic Asian", "Other Race - Including Multi-Racial"
    renda        = INDFMPIR,  # Razao renda/linha de pobreza (continuo)
    escolaridade = DMDEDUC2,  # "Less than 9th grade", ..., "College graduate or above"
    est_civil    = DMDMARTZ,  # "Married/Living with partner", "Widowed/Divorced/Separated", "Never married"
    imc          = BMXBMI,
    cintura    = BMXWAIST,
    hipertens  = BPQ020,   # "Yes"(2969), "No"(5518), "Refused"(1), "Don't know"(10)
    diabetes   = DIQ010,   # "Yes"(1081), "No"(10371), "Borderline"(284), "Don't know"(4)
    colesterol   = LBXTC,
    fumante      = SMQ040,     # "Every day", "Some days", "Not at all", "Refused"
    sedentarismo = PAD680      # Minutos sentado por dia (contínuo)
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
    infarto   = ifelse(as.character(infarto)   %in% c("Yes", "No"), as.character(infarto),   NA),
    hipertens = ifelse(as.character(hipertens) %in% invalidos,      NA, as.character(hipertens)),
    diabetes  = ifelse(as.character(diabetes)  %in% invalidos,      NA, as.character(diabetes)),
    fumante      = ifelse(as.character(fumante)      %in% invalidos, NA, as.character(fumante)),
    escolaridade = ifelse(as.character(escolaridade) %in% invalidos, NA, as.character(escolaridade)),
    est_civil    = ifelse(as.character(est_civil)    %in% invalidos, NA, as.character(est_civil)),
    across(c(infarto, sexo, raca, hipertens, diabetes, fumante, escolaridade, est_civil), factor)
  ) %>%
  filter(!is.na(infarto)) %>%  # Remove apenas quem nao tem o desfecho
  # ── Traduzir levels para PT-BR ──────────────────────────────────────
  mutate(
    infarto      = fct_recode(infarto,
                     "Sim" = "Yes",
                     "Não" = "No"),
    sexo         = fct_recode(sexo,
                     "Masculino" = "Male",
                     "Feminino"  = "Female"),
    raca         = fct_recode(raca,
                     "Mexicano-americano"      = "Mexican American",
                     "Outro hispânico"         = "Other Hispanic",
                     "Branco não-hispânico"    = "Non-Hispanic White",
                     "Negro não-hispânico"     = "Non-Hispanic Black",
                     "Asiático não-hispânico"  = "Non-Hispanic Asian",
                     "Outra raça/multirracial" = "Other Race - Including Multi-Racial"),
    hipertens    = fct_recode(hipertens,
                     "Sim" = "Yes",
                     "Não" = "No"),
    diabetes     = fct_recode(diabetes,
                     "Sim"       = "Yes",
                     "Não"       = "No",
                     "Limítrofe" = "Borderline"),
    fumante      = fct_recode(fumante,
                     "Todo dia" = "Every day",
                     "Às vezes"  = "Some days",
                     "Não fuma" = "Not at all"),
    escolaridade = fct_recode(escolaridade,
                     "< 9º ano"             = "Less than 9th grade",
                     "9º-11º ano"           = "9-11th grade (Includes 12th grade with no diploma)",
                     "Ensino médio/GED"     = "High school graduate/GED or equivalent",
                     "Faculdade incompleta" = "Some college or AA degree",
                     "Graduação ou mais"    = "College graduate or above"),
    est_civil    = fct_recode(est_civil,
                     "Casado(a)/União estável"             = "Married/Living with partner",
                     "Viúvo(a)/Divorciado(a)/Separado(a)" = "Widowed/Divorced/Separated",
                     "Solteiro(a)"                        = "Never married")
  )

# ── 6. Verificar resultado final ──────────────────────────────────────────────
glimpse(nhanes_cardio)
dim(nhanes_cardio)
count(nhanes_cardio, infarto)   # Balanceamento do desfecho

# ── 7. Tabela descritiva final (labels em PT-BR) ──────────────────────────────
tabela_desc <- nhanes_cardio %>%
  select(-SEQN) %>%
  tbl_summary(
    missing = "ifany",
    missing_text = "Perda",
    label = list(
      infarto    ~ "Infarto do miocárdio",
      idade      ~ "Idade (anos)",
      sexo       ~ "Sexo",
      raca       ~ "Raça/Etnia",
      renda      ~ "Renda (razão pobreza)",
      imc        ~ "IMC (kg/m²)",
      cintura    ~ "Circunferência abdominal (cm)",
      hipertens  ~ "Hipertensão arterial",
      diabetes   ~ "Diabetes",
      colesterol ~ "Colesterol total (mg/dL)",
      fumante      ~ "Tabagismo (status atual)",
      sedentarismo ~ "Sedentarismo (min/dia sentado)",
      escolaridade ~ "Escolaridade",
      est_civil    ~ "Estado civil"
    )
  )

# tabela_desc
tabela_desc %>% as_gt() %>% gt::gtsave("output/setup_tabela_descritiva.html")

# ── 8. Preparar dados para modelagem ──────────────────────────────────────────
# Preditores usados em TODOS os scripts de modelagem (01 a 04).
# Split feito aqui uma única vez — garante comparabilidade entre modelos.
dados <- nhanes_cardio %>%
  select(infarto, idade, sexo, hipertens, imc, colesterol, diabetes, fumante, sedentarismo) %>%
  drop_na()

set.seed(42)
split        <- initial_split(dados, prop = 0.75, strata = infarto)
dados_treino <- training(split)
dados_teste  <- testing(split)

# ══════════════════════════════════════════════════════════════════════════════
# fim do script ────────────────────────────────────────────────────────────────
# ══════════════════════════════════════════════════════════════════════════════