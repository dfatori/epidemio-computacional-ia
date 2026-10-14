# ══════════════════════════════════════════════════════════════════════════════
# Execução completa do fluxo de trabalho (scripts de 00-04).
# ══════════════════════════════════════════════════════════════════════════════

t_inicio <- Sys.time()  # marca o início para medir o tempo total ao final

# Em execução não-interativa (ex.: Rscript), os print() dos gráficos abririam o
# dispositivo padrão e gravariam um Rplots.pdf na raiz: desviar para o nulo.
# No RStudio interativo os prints vão para o painel Plots, como sempre.
nao_interativo <- !interactive()
if (nao_interativo) pdf(NULL)

cat("══ 00 · Configuração e limpeza dos dados ══════════════════════════════════════\n")
source("scripts/00_config.R")

cat("══ 01 · Regressão Logística ════════════════════════════════════════════\n")
source("scripts/01_regressao.R")

cat("══ 02 · Árvore de Decisão + Random Forest ══════════════════════════════\n")
source("scripts/02_random_forest.R")

cat("══ 03 · Gradient Boosting  ════════════════════════════════════════════\n")
source("scripts/03_gradient_boosting.R")

cat("══ 04 · Rede Neural ════════════════════════════════════════════════════\n")
source("scripts/04_rede_neural.R")

cat("══ Resumo: AUCs no conjunto de teste ═══════════════════════════════════\n")
# Todos os scripts usam o mesmo split e o mesmo desfecho: os AUCs são
# diretamente comparáveis. Esta tabela é o ponto de partida da discussão final.
resumo_auc <- data.frame(
  Modelo = c("Regressão Logística", "Random Forest",
             "Gradient Boosting (XGBoost)", "Rede Neural (MLP)"),
  AUC    = c(auc_lr$.estimate, auc_rf$.estimate,
             auc_gb$.estimate, auc_nn$.estimate)
)
print(resumo_auc %>% arrange(desc(AUC)))

cat(sprintf("\nTempo total: %.1f min\n", difftime(Sys.time(), t_inicio, units = "mins")))

if (nao_interativo) dev.off()  # fecha o dispositivo nulo aberto no início

# ══════════════════════════════════════════════════════════════════════════════
# fim do script ────────────────────────────────────────────────────────────────
# ══════════════════════════════════════════════════════════════════════════════