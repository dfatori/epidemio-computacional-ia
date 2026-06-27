# ══════════════════════════════════════════════════════════════════════════════
# NHANES 2021-2023 — Execução completa do pipeline
# ══════════════════════════════════════════════════════════════════════════════
# Execute este script para rodar toda a análise em sequência.
# Certifique-se de que o working directory é a raiz do projeto
# (onde estão as pastas data/, scripts/ e output/).

cat("══ 00 · Setup e limpeza dos dados ══════════════════════════════════════\n")
source("scripts/00_setup.R")

cat("══ 01 · Regressão Logística ════════════════════════════════════════════\n")
source("scripts/01_logistic_regression.R")

cat("══ 02 · Árvore de Decisão + Random Forest ══════════════════════════════\n")
source("scripts/02_random_forest.R")

cat("══ 03 · Gradient Boosting (XGBoost) ════════════════════════════════════\n")
source("scripts/03_gradient_boosting.R")

cat("══ 04 · Rede Neural ════════════════════════════════════════════════════\n")
source("scripts/04_neural_network.R")

cat("══ 05 · Regressão Linear (colesterol) ══════════════════════════════════\n")
source("scripts/05_linear_regression.R")

cat("\nPipeline concluído. Outputs salvos em output/\n")

# ══════════════════════════════════════════════════════════════════════════════
# fim do script ────────────────────────────────────────────────────────────────
# ══════════════════════════════════════════════════════════════════════════════